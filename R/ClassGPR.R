#------------------------------------------#
#----------- CLASS DEFINITION -------------#
setClass(
	Class="GPR",	
	slots=c(
		data="matrix", 		# one column per trace
		traces="numeric",	# numbering of each trace (from 1 to ntr)
		depth="numeric",	# depth position
		pos="numeric",		# position	of the traces
		time0="numeric",	# time-zero (first air-wave arrival)
		time="numeric", 	# time of the trace recording
		com="character", 	# GPR$dt1$fid		<-> GPR$dt1$x8
		ann="character",	# annotation
		coord="matrix", 	# coordinates (x,y,z) of each traces
		rec="matrix", 		# coordinates (x,y,z) of the receiver antenna
		trans="matrix", 	# coordinates (x,y,z) of the transmitter antenna
		coordref="numeric", # coordinates references
		ntr = "numeric", 	# number of traces
		w = "numeric", 		# time window length
		freq ="numeric", 	# antenna frequency
		dz = "numeric", 	# time/depth sampling
		dx ="numeric", 		# spatial trace sampling
		antsep ="numeric", 	# antenna separation
		name="character",	# name of the profile
		description ="character",	# description of the profile
		filename ="character",	# filename of the profile
		depthunit ="character", # time/depth unit
		posunit = "character",	# spatial unit
		surveymode ="character", # survey mode (reflection/CMP)
		date ="character",		# date of the survey 
		crs ="character",	# coordinate reference system of coord
		proc="character",	# processing steps
		vel="list",			# velocity model
		delineations="list",	# delineated lines
		hd="list"			# header from *.dt1 file
	)
)


#------------------------------------------#
#-------------- CONSTRUCTOR ---------------#
# GPR = classical GPR list
GPR <- function(x,name="",description="",filename=""){
	rec_coord <- cbind(x$dt1$recx,x$dt1$recy,x$dt1$recz)
	trans_coord <- cbind(x$dt1$transx,x$dt1$transy,x$dt1$transz)
	if(sum(is.na(rec_coord))>0){
		warning(paste(sum(is.na(rec_coord)), "NA's in the receiver coordinates\n"))
	}
	if(sum(is.na(trans_coord))>0){
		warning(paste(sum(is.na(trans_coord)), "NA's in the transmitter coordinates\n"))
	}
	if(sum(is.na(x$dt1$topo))>0){
		warning(paste(sum(is.na(x$dt1$topo)), "NA's in the topo coordinates\n"))
	}
	if(sum(abs(rec_coord),na.rm=TRUE) == 0 ){
		rec_coord <- matrix(nrow=0, ncol=0) 
	}
	if(sum(abs(trans_coord), na.rm=TRUE)== 0){
		trans_coord <- matrix(nrow=0, ncol=0) 
	}
	if(sum(abs(x$dt1$topo),na.rm=TRUE)== 0){
		coord <- matrix(nrow=0, ncol=0) 
	}else{
		coord <- matrix(0,nrow=ncol(x$data), ncol=3)
		coord[,3] <- x$dt1$topo
	}
	dz <- getHD(x$hd,"TOTAL TIME WINDOW")/ getHD(x$hd, "NUMBER OF PTS/TRC")
	if(sum(abs(x$dt1$time0)) == 0){
		time_0 <- rep(getHD(x$hd, "TIMEZERO AT POINT")*dz,ncol(x$data))
	}else{
		time_0 <- x$dt1$time0
	}
	if(!grepl("^([0-9]{4})(-)([0-9]{2})(-)([0-9]{2})",x$hd[3,2])){
		d <- "1970-01-01"
	}else{
		d <- x$hd[3,2]
	}
	myT <- as.double(as.POSIXct(x$dt1$time, origin = as.Date(d)))
	GPR_device <-  x$hd[2,2]
	if(!grepl("^(Data.)",GPR_device)){
		GPR_device <- ""
	}
	hd_list <- list("startpos" = getHD(x$hd, "STARTING POSITION"),
					"endpos" = getHD(x$hd, "FINAL POSITION"),
					"nstacks" = getHD(x$hd, "NUMBER OF STACKS"),
					"nstacks" = getHD(x$hd, "NUMBER OF STACKS"),
					"gprdevice" = GPR_device
					)
	if(nrow(x$hd)>17){
		key <-  trim(x$hd[,1])
		test <- key!="" & seq_along(key)>17
		key <- key[test]
		key2 <- gsub("[[:punct:]]",replacement="",key)
		key2 <- gsub(" ",replacement="_",key2)
		nameL <- trim(x$hd[test,2])
		names(nameL) <- as.character(key2)
		hd_list_supp <- as.list(nameL)
		hd_list <- c(hd_list,hd_list_supp)
	}
	new("GPR", 	data=byte2volt()*x$data,
				traces=x$dt1$traces,	# x$dt1$traces
				com=x$dt1$com, 	# x$dt1$fid		<-> x$dt1$x8
				coord=coord, 	# x$dt1$topo	of the traces
				pos=x$dt1$pos,		# x$dt1$position	of the traces
				depth= seq(0,by=dz,length.out=nrow(x$data)),
				rec=rec_coord, 		# x$dt1$recx,x$dt1$recy,x$dt1$recz
				trans=trans_coord,
				time0=time_0,	# x$dt1$time0
				time=myT, 		# x$dt1$time
				proc=character(0),	# processing steps
				vel=list(0.1),	#m/ns
				name = name,
				description = description,
				filename = filename,
				ntr = ncol(x$data), 
				w = getHD(x$hd,"TOTAL TIME WINDOW"), 
				dz = dz, 
				dx = getHD(x$hd, "STEP SIZE USED"), 
				depthunit = "ns",
				posunit = getHD(x$hd, "POSITION UNITS",number=FALSE),
				freq =getHD(x$hd, "NOMINAL FREQUENCY"), 
				antsep =getHD(x$hd, "ANTENNA SEPARATION"), 
				surveymode =getHD(x$hd, "SURVEY MODE",number=FALSE),
				date = d,
				crs = "",
				# delineations=list(),
				hd=hd_list		# header
	)

}

setMethod("readGPR", "character", function(filename, description="", coordfile=NULL,crs="",intfile=NULL){
		# DT1
		if(file.exists(filename)){
			if(".DT1" == toupper(substr(filename,start=nchar(filename)-3,stop=nchar(filename)))){
				name=strsplit(basename(filename),'[.]')[[1]][1]
				A <- readDT1(filename)
				x <- (GPR(A,name=name,filename=filename,description=description))
				if(!is.null(coordfile)){
					cat("coordinates added\n")
					coords <- as.matrix(read.table(coordfile,sep=",",head=TRUE))
					crs(x)	 <- 	crs		
					coord(x) <- 	coords
				}
				if(!is.null(intfile)){
					cat("intersection added\n")
					intGPR <- (read.table(intfile,sep=" ",head=TRUE,stringsAsFactors = FALSE))
					ann(x) <- intGPR
				}
				return(x)
			}else if(".rds" == tolower(substr(filename,start=nchar(filename)-3,stop=nchar(filename)))){
				x <- readRDS(filename)
				x@filename <- filename
				return(x)
			}
		}else{
			stop(filename, "does not exist!")
		}
		
	} 
)

#------------------------------------------#
#---------------- COERCION ----------------#
# Coercion
setAs("GPR", "matrix", function(from){ from@data } )
setMethod("as.matrix",signature(x="GPR"),function(x){as(x,"matrix")})

# # as.matrix(myGPR2)
# setAs("GPR", "matrix", function(from){ stop("NOT IMPLEMTED YET!\n" )} ) # FIX ME!
# setAs("as.matrix",signature(x="GPR"),function(x){as(x,"matrix")})
# setAs("as.numeric", "GPR", function(x, ...) as.numeric(x@data))
setAs("GPR", "vector", function(from){ from@data})
setMethod("as.vector", signature(x="GPR"), function(x,mode="any"){ as.vector(x@data)})
# # as.matrix(myGPR2)

setMethod("length", "GPR", function(x) ncol(x@data))
# setMethod("range", "GPR", function(..., na.rm=FALSE) range(as.matrix(...),na.rm=na.rm))
setMethod("summary", "GPR", function(object, ...) summary(as.vector(object@data)))
setMethod("mean", "GPR", function(x, ...) mean(as.vector(x@data)))



setMethod(
	f="apply", 
	signature="GPR", 
	definition=function(X,MARGIN,FUN,...){
		apply(X@data,MARGIN,FUN,...)
	}
)
setMethod(
	f="nrow", 
	signature="GPR", 
	definition=function(x){
		nrow(x@data)
	}
)
setMethod(
	f="ncol", 
	signature="GPR", 
	definition=function(x){
		ncol(x@data)
	}
)
setMethod(
	f="dim", 
	signature="GPR", 
	definition=function(x){
		dim(x@data)
	}
)
setMethod(
	f="min", 
	signature="GPR", 
	definition=function(x,...,na.rm=FALSE){
		min(x@data,na.rm=na.rm)
	}
)
setMethod(
	f="max", 
	signature="GPR", 
	definition=function(x,...,na.rm=FALSE){
		max(x@data,na.rm=na.rm)
	}
)
setMethod(
	f="range", 
	signature="GPR", 
	definition=function(x,...,na.rm=FALSE){
		range(x@data,na.rm=na.rm)
	}
)

# math group generic functions
# getGroupMembers("Math")
setMethod(
	f="Math",
	signature="GPR",
	definition=function(x){
		switch(.Generic,
			abs = abs(x@data),
			sign = sign(x@data),
			sqrt =  sign(x@data)*sqrt(abs(x@data)),
			ceiling = ceiling(x@data),
			floor = floor(x@data),
			trunc = trunc(x@data),
			cummax = paste("not allowed"),  
			cumprod =paste("not allowed"),  
			cumsum =paste("not allowed"),  
			exp	= exp(x@data),
			expm1 =expm1( x@data),
			log = sign(x@data) *log(abs(x@data)),
			log10 = sign(x@data) *log10(abs(x@data)),
			log2 = sign(x@data) *log2(abs(x@data)), 
			log1p = sign(x@data) *log1p(abs(x@data)), 
			cos = cos(x@data),
			cosh = cosh(x@data),
			sin = sin(x@data),
			sinh = sinh(x@data),
			tan = tan(x@data),
			tanh = tanh(x@data),
				# "acos"     "acosh"   
	# [25] "asin"     "asinh"    "atan"     "atanh"    "gamma"    "lgamma"   "digamma"  "trigamma"
			stop(paste(.Generic, "not allowed on GPR objects"))
		)
		return(x)
	} 
)

# > getGroupMembers("Arith")
# [1] "+"   "-"   "*"   "^"   "%%"  "%/%" "/" 
.GPR.add <- function(a, b){
	if(is(b,"GPR")){
		x <- b
		b <- b@data
	}
	if(is(a,"GPR")){
		x <- a
		a <- a@data
	}
	x@data <- a + b
	return(x)
}
.GPR.sub <- function(a, b){
	if(is(b,"GPR")){
		x <- b
		b <- b@data
	}
	if(is(a,"GPR")){
		x <- a
		a <- a@data
	}
	x@data <- a - b
	return(x)
}
.GPR.mul <- function(a, b){
	if(is(b,"GPR")){
		x <- b
		b <- b@data
	}
	if(is(a,"GPR")){
		x <- a
		a <- a@data
	}
	x@data <- a * b
	return(x)
}
.GPR.div <- function(a, b){
	if(is(b,"GPR")){
		x <- b
		b <- b@data
	}
	if(is(a,"GPR")){
		x <- a
		a <- a@data
	}
	x@data <- a / b
	return(x)
}
.GPR.pow <- function(a, b){
	if(is(b,"GPR")){
		x <- b
		b <- b@data
	}
	if(is(a,"GPR")){
		x <- a
		a <- a@data
	}
	x@data <- a ^ b
	return(x)
}


.GPR.arith <- function(e1,e2){
	switch(.Generic,
		"+" = .GPR.add(e1, e2),
		"-" = .GPR.sub(e1, e2),
		"*" = .GPR.mul(e1, e2),
		"/" = .GPR.div(e1, e2),
		"^" = .GPR.pow(e1, e2),
		stop(paste("binary operator \"", .Generic, "\" not defined for GPR"))
	) 
}						 

setMethod(
	f= "Arith",
	signature=c(e1="GPR",e2="ANY"), 
	definition=.GPR.arith
)
setMethod(
	f= "Arith",
	signature=c(e1="GPR",e2="GPR"), 
	definition=.GPR.arith
)
setMethod(
	f= "Arith",
	signature=c(e1="ANY",e2="GPR"), 
	definition=.GPR.arith
)

#------------------------------
# "["
setMethod(
	f= "[",
	signature="GPR",
	definition=function(x,i,j,drop){
		rval <- x@data
		n <- nrow(rval)
		if(missing(i)) i <- 1:n
		if(length(dim(rval)) == 2) {
			# drop. <- ifelse(length(i) == 1, FALSE, drop)
			drop <- FALSE
			if(missing(j)){
				# rval <- rval[i, , drop = drop.]
				rval <- rval[i, , drop = drop]
				x@w <- length(i)*x@dz
				x@depth <- x@depth[i]
			} else{ 
				# rval <- rval[i, j, drop = drop.]
				rval <- rval[i, j, drop = drop]
				x@w <- length(i)*x@dz
				x@depth <- x@depth[i]
				x@traces <- x@traces[j]
				x@pos <- x@pos[j]
				x@time0 <- x@time0[j]
				x@time <- x@time[j]
				x@com <- x@com[j]
				if(length(x@coord)>0)	x@coord <- x@coord[j,,drop=FALSE]
				if(length(x@rec)>0) x@rec <- x@rec[j,,drop=FALSE]
				if(length(x@trans)>0) x@trans <- x@trans[j,,drop=FALSE]
				x@ntr <- length(j)
				trpos <- seq(x@hd$startpos, x@hd$endpos,by=x@dx)
				x@hd$endpos <- trpos[j[length(j)]]
				x@hd$startpos <- trpos[j[1]]
			}
			if(drop && length(rval) == 1){ rval <- c(rval)}
		}else{
			rval <- rval[i]
		}
		x@data <- rval
		return(x)
	}
)

#-------------------------------
# "[<-"
setReplaceMethod(
	f="[",
	signature="GPR",
	definition=function(x,i,j,value){
		rval <- x@data
		n <- nrow(rval)
		if(missing(i)) i <- 1:n
		if(missing(j)) j <- 1:ncol(x@data)
		if(length(dim(x@data)) == 2) {
			x@data[i,j] <- value
		}else{
			rval <- rval[i]
		}
		
		return (x)
	}
)


setMethod("gethd", "GPR", function(x,hd=NULL){
		if(is.null(hd)){
			return(x@hd)
		}else{
			if(!is.null(x@hd[[hd]])){
				if(is.character(x@hd[[hd]])){
					as.character(x@hd[[hd]])
				}else{
					as.numeric(x@hd[[hd]])
				}
			}
		}
	} 
)

setMethod("filename", "GPR", function(x){
		return(x@filename)
	} 
)
setMethod("ann", "GPR", function(x){
		return(x@ann)
	} 
)
setReplaceMethod(
	f="ann",
	signature="GPR",
	definition=function(x,values){
		traces <- (values[,1])
		annnames <- as.character(values[,2])
		valuesList <- (tapply(annnames, traces, identity))
		test <- unlist(lapply(valuesList,paste,sep="",collapse="#"))
		x@ann <- character(length(x))
		x@ann[as.numeric(names(test))] <- test
		return(x)
	}
)
setMethod("coord", "GPR", function(x){
		return(x@coord)
	} 
)
setReplaceMethod(
	f="coord",
	signature="GPR",
	definition=function(x,values){
		values <- as.matrix(values)
		if(ncol(x@data) == nrow(values) && ncol(values)==3){
			x@coord <- values
		}else{
			stop("Dimension problem!!")
		}
		return(x)
	}
)
setMethod("crs", "GPR", function(x){
		return(x@crs)
	} 
)
setReplaceMethod(
	f="crs",
	signature="GPR",
	definition=function(x,value){
		value <- as.character(value)[1]
		x@crs <- value
		return(x)
	}
)
setMethod("time0", "GPR", function(x){
		return(x@time0)
	} 
)
setReplaceMethod(
	f="time0",
	signature="GPR",
	definition=function(x,value){
		if(length(value) == length(x@time0)){
			x@time0 <- value
		}else{
			x@time0 <- rep(value[1], length(x@time0))
		}
		x@proc <- c(x@proc, "set time-zero")
		return(x)
	}
)
setReplaceMethod(
	f="fid",
	signature="GPR",
	definition=function(x,values){
		values <- as.character(values)
		x@com <- values
		return(x)
	}
)
setMethod("fid", "GPR", function(x){
		return(x@com)
	} 
)
setMethod("getData", "GPR", function(x){
		return(x@data)
	} 
)
setMethod("setData<-", "GPR", function(x,value){
		if(all(dim(value)==dim(x))){
			x@data <- value
			return(x)
		}else{
			stop("x [",nrow(x),"x",ncol(x),"] and A [",nrow(value),"x",ncol(value),"] should have the same size\n")
		}
	} 
)

setMethod("getAmpl", "GPR", function(x, FUN=mean, ...){
	AMP <- apply(abs(x@data),1,FUN,...)
		return(AMP)
	} 
)

setMethod("name", "GPR", function(x){
		return(x@name)
	} 
)

setMethod("description", "GPR", function(x){
		return(x@description)
	} 
)


#================= PROCESSING ===============#

#----------------- DC-SHIFT
setMethod("dcshift", "GPR", function(x, u){
		x <-  x - apply(x[u,],2,mean)
		x@proc <- c(x@proc, "dc-shift")
		return(x)
	} 
)

#----------------- FIRST-BREAK
# Jaun I. Sabbione and Danilo Velis (2010). Automatic first-breaks picking: 
# New strategies and algorithms. Geophysics, 75 (4): v67-v76
# -> modified Coppens's Method
# nl = length leading window: about one period of the firs-arrival waveform
# ns = length eps (edge preserving smoothing) window: good results with ns between one and two signal periods
#				-> default values ns= 1.5*nl
# bet = stabilisation constant, not critical, set to 0.2*max(amplitude) 
setMethod("firstBreack", "GPR", function(x, nl=11, ns=NULL, bet=NULL){
		if( (nl%%2)==0) nl <- nl+1
		if(is.null(ns)) ns <- round(1.5*nl)
		if( ns%%2 == 0) ns <- ns+1
		# H <- EMD::hilbert(x@data)
		# s <- Mod(H)
		s <- x@data^2
		if(is.null(bet)) bet <- 0.2*max(s)
		fb <- apply(s,2,firstBreackPicking)*x@dz
		return(fb)
	} 
)

#----------------- DEWOW

setMethod("dewow", "GPR", function(x, type=c("MAD","Gaussian"),...){
	type <- match.arg(type)
	if(type=="MAD"){	
		w = 100	# argument initialization
		x0= 0.1	# argument initialization
		if( length(list(...)) ){
			dots <- list(...)
			if( !is.null(dots$w)){
				w <- dots$w
			}
			if( !is.null(dots$x0)){
				x0 <- dots$x0
			}
		}
		cat("dewow with ", w,"ns")
		w <- round(w / x@dz)
		cat("  (", w,"pts)\n")
		A <- x@data
		if(length(dim(A))<2){
			A <- matrix(A,ncol=1,nrow=length(A))
		}
		X <- rbind(matrix(0,ncol=ncol(A),nrow=w), A, matrix(0,ncol=ncol(A),nrow=w))
		n <- nrow(X)
		Y <- X
		cat("use wapply(x, width, by = NULL, FUN = NULL, ...)\n")
		for (i in (w + 1):(n - w)) {
			Xmed <- apply( X[(i - w):(i + w),,drop=FALSE],2, median)
			# S0 <- 1.4826 * apply( abs(X[(i - w):(i + w),,drop=FALSE] - Xmed),2, median)
			# test <- abs(X[i,] - Xmed) > x0 * S0
			# Y[i,test] <- Xmed[test]
			Y[i,] <- Xmed
			
		}
		x@data <- A - Y[(w+1):(n-w),]
	}else if(type == "Gaussian"){
		sig = 100	# argument initialization
		if( length(list(...)) ){
			dots <- list(...)
			if( !is.null(dots$sig)){
				sig <- dots$sig
			}
		}
		sig <- sig * x@dz
		t0 <- round(mean(x@time0)/x@dz)
		A <- x@data
		A[1:t0,] <- 0
		if(length(dim(A))<2){
			A <- matrix(A,ncol=1,nrow=length(A))
		}
		x@data[t0:nrow(x),] <- A[t0:nrow(x),] - gaussianSmooth(A,sig)[t0:nrow(x),]
	}
	proc <- get_args()
	x@proc <- c(x@proc, proc)
	return(x) 
})

setMethod("dewow2", "GPR", function(x, sig=100){
	stop("DEPRECATED!!! USE 'dewow(x,type=\"Gaussian\",sig=100)' instead!\n")
})

#----------------- 1D-FILTER
setMethod("medianFilter1D", "GPR", function(x,w){
		if(w %% 2 == 1){
			w <- w + 1	# uneven window
		}
		w <- (w-1)/2
		x@data <-  apply(x@data,2,.medianFilter1D,w)
		x@proc <- c(x@proc, "median filter 1D")
		return(x)
	} 
)

setMethod("hampelFilter", "GPR", function(x, w=10,x0=0.1){
			w <- round(w / x@dz)
			A <- x@data
			if(length(dim(A))<2){
				A <- matrix(A,ncol=1,nrow=length(A))
			}
			X <- rbind(matrix(0,ncol=ncol(A),nrow=w), A, matrix(0,ncol=ncol(A),nrow=w))
			n <- nrow(X)
			Y <- X
			cat("use wapply(x, width, by = NULL, FUN = NULL, ...)\n")
			for (i in (w + 1):(n - w)) {
				Xmed <- apply( X[(i - w):(i + w),,drop=FALSE],2, median)
				# S0 <- 1.4826 * apply( abs(X[(i - w):(i + w),,drop=FALSE] - Xmed),2, median)
				# test <- abs(X[i,] - Xmed) > x0 * S0
				# Y[i,test] <- Xmed[test]
				Y[i,] <- Xmed
				
			}
			proc <- get_args()
			x@proc <- c(x@proc, proc)
			x@data <- Y[(w+1):(n-w),]
		   # list(y = y[(k+1):(n-k)], ind = ind)
		   return(x)
	} 
)

#----------------- 1D-SCALING (GAIN)
setMethod("gain", "GPR", function(x, type=c("geospreading","exp","agc"),...){
	type <- match.arg(type)
	if(type=="geospreading"){
		# args = alpha,d_t,t_0=NULL,t_end=NULL,t_cst=NULL
		x@data <- gain_geospreading(x@data, d_t=x@dz, ...)
	}else if(type=="exp"){
		x@data <- gain_exp(x@data, d_t=x@dz, ...)
	}else{# if("agc"){
		x@data <- gain_agc(x@data, d_t=x@dz, ...)
	}
	proc <- get_args()
	x@proc <- c(x@proc, proc)
	# list(y = y[(k+1):(n-k)], ind = ind)
	return(x)
	} 
)

#----------------- SPATIAL-FILTER
setMethod("adimproSmooth", "GPR", function(x,hmax=2,...){
 # adsmooth <- function(x,hmax=2){
		 IMG <- x@data
		 IMG <- (IMG-min(IMG))/(max(IMG)-min(IMG))
		 adimg <- make.image(IMG)
		 # img.smooth <- awsimage(adimg, hmax = 2)
		 # img.smooth <- awsaniso(adimg, hmax = 2,...)
		 img.smooth <- awspimage(adimg, hmax = 2,...)
		 AA <- extract.image(img.smooth)
		 AAA <- (AA-mean(AA))/sd(AA)
		 x@data <- AAA
		 proc <- get_args()
		x@proc <- c(x@proc, proc)
		 return(x)
	}
)

setMethod("medianFilter", "GPR", function(x){
		x@data <-  .medianFilter(x@data)
		x@proc <- c(x@proc, "median filter 3x3")
		return(x)
	} 
)

#----------------- CLIP/GAMMA/NORMALIZE
setMethod("clip", "GPR", function(x,Amax=NULL,Amin=NULL){
	x@data <- .clip(x@data,Amax,Amin)
	return(x)
	} 
)
setMethod("gammaCorrection", "GPR", function(x,a=1,b=1){
	x@data <- .gammaCorrection(x@data,a,b)
	return(x)
	} 
)
setMethod("rmsScaling", "GPR", function(x){
		x@data <- x@data/apply(x@data ,2, .RMS)
		x@proc <- c(x@proc, "RMS trace scaling")
		return(x)
	}
)

#----------------- FREQUENCY FILTERS
setMethod("freqFilter", "GPR", function(x, f=100, type=c('low','high','bandpass'),L=257,plot_spec=FALSE){
		x@data <- freqFilter1D(x@data, f=f,  type = type, L=L, T = x@dz, plot_spec = plot_spec)
		proc <- get_args()
		x@proc <- c(x@proc, proc)
		return(x)
		
	} 
)

setMethod("fkFilter", "GPR", function(x, fk=NULL, L=c(5,5),npad=1){
		if(is.null(fk)) stop("fk argument has to be specified")
		# if polygon
		if(is.list(fk) && length(fk) == 2){
			areaunclosed <- t(do.call("rbind",area))
			nk <- npad*(nextpower2(ncol(x@data)))
			nf <- npad*(nextpower2(nrow(x@data)))
			fk <- outer((nf:1)-0.5,(1:nk)-0.5,inPoly,vertx=areaunclosed[,2],verty=areaunclosed[,1])
		}else if(!is.matrix(fk)){
			stop("fk should be either of type 'list' or 'matrix'\n")
		}else if(is.matrix(fk)){
			cat("# FIXME! function to transform matrix into polygon\n")
		}
		
		x@data <- FKFilter(x@data,fk=fk,L=L,npad=npad)
		proc <- get_args()
		x@proc <- c(x@proc, proc)
		return(x)
		
	} 
)

#--------------- DECONVOLUTION
setMethod("rotatePhase", "GPR", function(x, phi){
	# rotatePhase <- function(x,phi){
		x@data <- apply(x@data,2,phaseRotation,phi)
		proc <- get_args()
		x@proc <- c(x@proc,proc)
		return(x)
	}
)

setMethod("deconvSpiking", "GPR", function(x, W,wtr,nf,mu){
# deconvSpiking <- function(gpr,W,wtr,nf,mu){
		W <- seq(W[1],W[2])
		X <- rmsScaling(x)@data
		# X <- X / apply(as.matrix(X),2,RMS)
		Xdec <- matrix(nrow=nrow(X),ncol=ncol(X))
		Fmin <- matrix(nrow=nf,ncol=ncol(X))
		Wmin <- matrix(nrow=nf,ncol=ncol(X))
		for(i in 1:ncol(X)){
			ww <- (i-wtr):(i+wtr)
			ww <- ww[ww <= ncol(X)]
			ww <- ww[ww >= 1]
			supertrace <- as.vector(X[W,ww])
			# inverse minimum-phase wavelet estimation # variante 1 (Emanuel)
			Fmin[,i] <- spikingFilter(supertrace,nf=nf ,mu=mu, shft=1)
			# Wmin[,i] <- deconv(c(1,rep(0,nf-1)),Fmin[,i], nf=nf,mu=mu)
			Wmin[,i] <- deconvFreq(c(1,rep(0,nf-1)),Fmin[,i], mu=mu)
			# minimum-phase deconvolued data
			Xdec[,i] <- convolution(X[,i],Fmin[,i])[1:nrow(X)]
		}
		# gprdec <- gpr
		x@data <- Xdec
		proc <- get_args()
		x@proc <- c(x@proc, proc)
		return(list("dec"=x,"fmin"=Fmin,"wmin"=Wmin))
	}
)

#--------------- DATA EDITING FUNCTIONS
setMethod("traceShift", "GPR", function(x,  fb,kip=10){
# traceShift <- function(gpr_mix,fbmix, kip=10){
		fb <- fbmix/x@dz 
		if(min(fb) > kip){
			fb <- fb - kip
		}else{
			fb <- fb - min(fb) + 1
		}
		A <- gpr_mix@data
		Anew <- matrix(nrow=nrow(A),ncol=ncol(A))
		
		minShift <- min(fbmix) - kip
		maxShift <- max(fbmix) - kip
		
		for(i in seq_along(A[1,])){
			vs <- seq(fb[i],nrow(A))
			vsp <- seq(fb[i],nrow(A))-fb[i]+1
			Anew[vsp,i] <- A[vs,i]
		}
		x@data <- Anew	
		x@proc <- c( x@proc, "trace shift")
		return(x)
	}
)

#-------------------------------------------#
#---------------- SETMETHOD ----------------#
# Print methods
# setMethod("print", "GPR", function(x) print.GPR(x))
# > 1. helper function:
.GPR.print 	<-	function(x, digits=5){
	topaste <- c(paste("***","Class GPR", "***\n"))
	topaste <- c(topaste, paste("name = ", x@name, "\n",sep=""))
	if(x@filename != ""){
		topaste <- c(topaste, paste("filename = ", x@filename, "\n",sep=""))
	}
	nbfid <- sum(trim(x@com)!= "")
	if(nbfid > 0){
		topaste <- c(topaste, paste(nbfid, " fiducial(s)\n",sep=""))
	}
	if(x@description != ""){
		topaste <- c(topaste, paste("description = ", x@description, "\n",sep=""))
	}
	if(x@date != ""){
		topaste <- c(topaste, paste("survey date = ", x@date,"\n",sep=""))
	}
	topaste <- c(topaste, paste(x@surveymode,", ",x@freq,"MHz,", " W=",x@w,x@depthunit,", dz=",x@dz,x@depthunit,"\n",sep=""))
	topaste <- c(topaste, paste(x@ntr, " traces, ",diff(range(x@pos)),"",x@posunit," long\n",sep=""))
	
	if(length(x@proc)>0){
		topaste <- c(topaste, paste("> PROCESSING\n"))
		for(i in seq_along(x@proc)){
			topaste <- c(topaste, paste("  ",i,". ", x@proc[i],"\n",sep=""))
		}			
	}
	topaste <- c(topaste, paste("****************\n"))
	return(topaste)	 	 
} 	 
# > 2. S3 function:
print.GPR <- function(x, ...){
	jj <- .GPR.print(x, ...)
	cat(jj)
	return(invisible(jj))
}
# > 3. And finally a call to setMethod():
setMethod("show", "GPR", function(object){print.GPR(object)}) 	


# options: type=c(raster,wiggles), add_topo, clip, normalize
plot.GPR <- function(x,y,...){
	# type=c("raster","wiggles"),add_topo=FALSE,clip=NULL,normalize=NULL,nupspl=NULL,...){
	# print(list(...))
	type <- "raster"
	add_topo <- FALSE
	clip=NULL
	normalize=NULL
	nupspl=NULL
	dots <- list()
	add_fid <- TRUE
	if( length(list(...)) ){
		dots <- list(...)
		if( !is.null(dots$type)){
			type <- dots$type
			dots$type <- NULL
		}
		if( !is.null(dots$clip)){
			clip <- dots$clip
			dots$clip <- NULL
		}
		if( !is.null(dots$normalize)){
			normalize <- dots$normalize
			dots$normalize <- NULL
		}
		if( !is.null(dots$nupspl)){
			nupspl <- dots$nupspl
			dots$nupspl <- NULL
		}
		add_ann <- TRUE
		if( !is.null(dots$add_ann) && !isTRUE(dots$add_ann) ){
			add_ann <- FALSE
		}
		if( !is.null(dots$add_fid) && !isTRUE(dots$add_fid) ){
			add_fid <- FALSE
		}
		add_topo <- FALSE
		if( !is.null(dots$add_topo) && isTRUE(dots$add_topo) ){
			add_topo <- TRUE
		}
		dots$add_fid <- NULL
		dots$add_topo <- NULL
		dots$addArrows <- NULL
		if(!is.null(dots$lwd)){
			lwd <- dots$lwd
		}
		dots$add <- NULL
		if(!is.null(dots$shp_files)){
			add_shp_files <- TRUE
			shp_files <- dots$shp_files
		}
		dots$shp_files <- NULL
		# print(dots)
	}
	if(length(x@vel)>0){	
		vel <- x@vel[[1]]
	}else{
		vel <- 0
	}
	if(any(dim(x) == 1)){
		par(mar=c(5,4,3,2)+0.1,oma=c(0,0,3,0), mgp=c(2, 0.5, 0))
		z <- seq(0,by=x@dz,length.out=length(x@data))
		plot(z,x@data,type="n",xlab=x@depthunit,ylab="mV",xaxt="n")
		x_axis <- pretty(seq(x@time0,by=x@dz,length.out=length(x@data)))
		axis(side=1,at=x_axis+x@time0, labels=x_axis,tck=+0.02)
		depth_0 <- depth0(x@time0, vel, antsep=x@antsep)
		depth <- (seq(0,by=2.5,max(z)*vel))
		depth2 <- seq(0.1,by=0.1,0.9)
		depthat <- depthToTime(depth, x@time0, vel, antsep=x@antsep)
		depthat2 <- depthToTime(depth2, x@time0, vel, antsep=x@antsep)
		axis(side=3,at=depthat, labels=depth,tck=+0.02)
		axis(side=3,at=depthat2, labels=FALSE,tck=+0.01)
		axis(side=3,at=depthToTime(1, x@time0, vel, antsep=x@antsep), labels=FALSE,tck=+0.02)
		abline(h=0,lty=3,col="grey")
		abline(v=x@time0,col="red")
		abline(v=depth_0,col="grey",lty=3)
		lines(z,x@data)
		title(paste(x@name, ": trace n°", x@traces," @",x@pos,x@posunit,sep=""),outer=TRUE)
		mtext(paste("depth (m),   v=",vel,"m/ns",sep="") ,side=3, line=2)
 	}else{
		if(!is.null(nupspl)){
			x <- upsample(x,n=nupspl)
		}
		if(!is.null(normalize)){
			x@data <- normalize(x@data,type=normalize)
		}
		# warning("First upsample then add_topo. Problem: interpolate also coord!!!")
		if(!is.null(clip) && is.numeric(clip)){
			if(length(clip)>1){
				x@data <- .clip(x@data,clip[2],clip[1])
			}else if(length(clip)==1){
				x@data <- .clip(x@data,clip[1])
			}
		}
		if(add_fid == FALSE){
			x@com <- character(length(x@com))
		}
		type=match.arg(type, c("raster","wiggles"))
		if(type=="raster"){
			if(add_topo){
				x <- migration(x)
			}
			if(grepl("[m]$",x@depthunit)){
				ylab <- paste("depth (",x@depthunit,")",sep="")
			}else if(grepl("[s]$",x@depthunit)){
				ylab <- paste("two-way travel time (",x@depthunit,")",sep="")
			}
			# xvalues <- x@pos
			if(length(x@coord)>0 && sum(abs(x@coord[,1:2])>0)){
				xvalues <- lineDist(x@coord)
			}else{
				xvalues <- x@pos
			}
			# cat(mean(x@time0))
			do.call(plotRaster, c(list(A=x@data, col= diverge_hcl(101, h = c(246, 10), c = 120, l = c(30, 90)), 
										x=xvalues, y= -rev(x@depth), main=x@name, xlab=x@posunit, ylab=ylab, 
										note=x@filename,time_0=x@time0,antsep=x@antsep, v=vel,fid=x@com,ann=x@ann,
										depthunit=x@depthunit),dots))
		}else if(type=="wiggles"){
			if(add_topo && length(x@coord)>0){
				topo <- x@coord[,3]
				# cat("add topo")
			}else{
				topo = NULL
			}
			if(grepl("[m]$",x@depthunit)){
				ylab <- paste("depth (",x@depthunit,")",sep="")
			}else if(grepl("[s]$",x@depthunit)){
				if(add_topo){
					ylab <- paste("depth (m)",sep="")
				}else{
					ylab <- paste("two-way travel time (",x@depthunit,")",sep="")
				}
			}
			if(length(x@coord)>0){
				xvalues <- lineDist(x@coord)
			}else{
				xvalues <- x@pos
			}
			# x@name
			# x@posunit
			# print(...)
			do.call(plotWig, c(list(A=x@data,x=xvalues, y= -rev(x@depth), main=x@name, xlab=x@posunit, ylab=ylab, topo= topo,
					note=x@filename,col="black",time_0=x@time0,antsep=x@antsep, v=vel,fid=x@com,ann=x@ann,
					depthunit=x@depthunit),dots))
		}
	}
}





setMethod("plot3D", "GPR", function(x,add_topo=FALSE,clip=NULL,normalize=NULL,nupspl=NULL,add=TRUE,xlim=NULL,ylim=NULL,zlim=NULL,...) {
# plot3D <- function(x,type=c("raster","wiggles"),add_topo=FALSE,clip=NULL,normalize=NULL,nupspl=NULL,...){
		if(length(x@vel)>0){	
			vel <- x@vel[[1]]
		}else{
			vel <- 0
		}
		xsel <- rep(TRUE,length(x))
		if(!is.null(xlim)){
			xlim <- sort(xlim)
			xsel <- coord(x)[,1] >= xlim[1] &  coord(x)[,1] <= xlim[2]
		}
		ysel <- rep(TRUE,length(x))
		if(!is.null(ylim)){
			ylim <- sort(ylim)
			ysel <- coord(x)[,2] >= ylim[1] &  coord(x)[,2] <= ylim[2]
			cat(ylim,"  range=",range(coord(x)[,2]),"\n")
		}
		xysel <- xsel & ysel
		if(sum(xysel)<=2){
			return(NULL)
		}
		x <- x[,xysel]
		if(!is.null(nupspl)){
			cat("upsample...")
			x <- upsample(x,n=nupspl)
		}
		if(!is.null(normalize)){
			x@data <- normalize(x@data,type=normalize)
		}
		# warning("First upsample then add_topo. Problem: interpolate also coord!!!")
		if(!is.null(clip) && is.numeric(clip)){
			if(length(clip)>1){
				x@data <- .clip(x@data,clip[2],clip[1])
			}else if(length(clip)==1){
				x@data <- .clip(x@data,clip[1])
			}
		}
		if(length(x@coordref)!=3 ){
			coordref <- apply(coord(x),2,min)
		}else{
			coordref <-x@coordref
		}
		z0 <- coord(x)[,3]-coordref[3]
		if(add_topo){
			x <- migration(x)
			z0 <- rep(max(coord(x)[,3]),length(x))-coordref[3]
		}
		cat(coordref,max(coord(x)[,3]),"\n")
		A <-as.matrix(x)
		# cat(coordref,"\n")
		xpos <- coord(x)[,1]-coordref[1]
		ypos <- coord(x)[,2]-coordref[2]
		zpos <- x@depth
		if(add==FALSE){
			# rgl.open()
			open3d()
		}
		.plot3D(A,xpos,ypos,zpos,z0,...)
	}
)

setMethod("plotAmpl", "GPR", function(x, FUN=mean, add=FALSE, ylim=NULL,xlim=NULL,col=1,all=FALSE, ...){
	AMP <- apply(abs(x@data),1,FUN,...)
	z <- seq(0,by=x@dz,length.out=length(AMP))
	if(!add){
		par(mar=c(5, 4, 4, 2)+0.1)
		plot(z,log(AMP),type="l",xlab=x@depthunit,ylab="log(mV)",ylim=ylim,xlim=xlim,col=col)
		if(all == TRUE){
			nothing <- apply(log(abs(x@data)),2,lines,x=z, col=rgb(0.2,0.2,0.2,7/max(ncol(A),7)))
		}
		title(x@name)
	}else{
		if(all == TRUE){
			nothing <- apply(log(abs(x@data)),2,lines,x=z, col=rgb(0.2,0.2,0.2,7/max(ncol(A),7)))
		}
		lines(z,log(AMP),col=col)
	}
	# return(AMP)
	} 
)



setMethod("spec", "GPR", function(x, type=c("f-x","f-k"), return_spec=FALSE,plot_spec=TRUE, ...){
		type <- match.arg(type)
		if(type == "f-x"){
			S <- powSpec(x@data,T = x@dz, fac = 1000000, 
						plot_spec = plot_spec, return_spec = return_spec, 
						title_spec = x@name)
						# cat(x@name)
			
		}else if(type == "f-k"){
			S <- FKSpectrum(x@data,dx=x@dx,dz=x@dz, plot_spec=plot_spec,return_spec=return_spec,...)
		}
		return(S)
	} 
)











setMethod("interpTraces", "GPR", function(x,topo){
		# test if any measured points have a reference to a trace of the GPR-line
		if(all(is.na(topo[,"TRACE"]))){
			warning(paste(x@name, ": no link between the measured points and the GPR traces!\n"))
		}else{
			# if we have measured some points before the line start or after the end
			# of the GPR Line, we delete them
			test <- !is.na(topo[,"TRACE"])				# test <- topo$PNAME %in% FID$PNAME
			trueEnd <- max(which(test==TRUE))
			trueBeg <- min(which(test==TRUE))
			topo <- topo[trueBeg:trueEnd,]
			#--- 3D topo Distance ---#
			dist3D <- lineDist(topo[,c("N","E","Z")], last=FALSE)
			#--- INTERPOLATION GPR POSITION FROM TOTAL STATION ---#
			# if there are points measured with the total station
			# that do not have an fiducial (FID) > interpolate them!
			myWarning <- ""
			if(!all(test[trueBeg:trueEnd])){
				# dist3D[topo$PNAME %in% FID$PNAME] > distance for the points also recorded in FID
				myWarning <- "  points total station without fiducials"
				# cat("  points total station without fiducials")
				test <- !is.na(topo[,"TRACE"])
				if(sum(test)>2){
					interp_method <- "linear"
				}else{
					interp_method <- "linear"
				}
				FIDpos 	<- signal::interp1(x=dist3D[test],y=x@pos[topo[test,"TRACE"]], xi=dist3D, method =interp_method , extrap=TRUE)
			}else{
				FIDpos 	<- x@pos[topo[test,"TRACE"]]
			}
			# now: FIDpos <=> topo
			

			#--- INTERPOLATION N,E,Z ---#
			posInt 	<- signal::interp1(FIDpos,dist3D, x@pos, method ="linear" , extrap=TRUE)
			# 'pchip': piecewise cubic hermite interpolating polynomial 
			Nint 	<- signal::interp1(dist3D,topo$N, posInt, method ="linear" , extrap=TRUE)
			Eint 	<- signal::interp1(dist3D,topo$E, posInt, method ="linear", extrap=TRUE)
			if(length(dist3D)<= 2){
				Zint <-	signal::interp1(dist3D,topo$Z, posInt, method ="linear", extrap=NA)
			}else{
				Zint <-	signal::interp1(dist3D,topo$Z, posInt, method ="pchip", extrap=NA)
			}
			
			lastNA  <- max(which(!is.na(Zint)))
			firstNA <- min(which(!is.na(Zint)))
			if(firstNA > 1)	Zint[1:(firstNA-1)] <- Zint[firstNA]
			if(lastNA < length(Zint))	Zint[(lastNA+1):length(Zint)] <- Zint[lastNA]
			
			cat(x@name,": mean dx=", round(mean(diff(posInt)),3), "  range dx=",round(min(diff(posInt)),3),"-", round(max(diff(posInt)),3))
			cat(myWarning)
			cat("\n")
			par(mfrow=c(1,3))
			plot(FIDpos,dist3D, pch=20,col="red",cex=2,asp=1,xlim=range(x@pos),ylim=range(posInt),main=paste(x@name))
			points(x@pos,posInt,pch=20,col="blue")
			plot(posInt,Zint,type="l", asp=10, main=paste(x@name, " min dx=",round(min(diff(posInt)),2), "  max dx=",round(max(diff(posInt)),2) ,sep=""))
			points(dist3D, topo[,c("Z")],pch=20,col="red")
			plot(topo[,c("E","N")],col=1,type="l",lwd=2,asp=1, ylim=range(Nint),xlim=range(Eint), 
				main=paste(x@name, " mean dx=",round(mean(diff(posInt)),2),sep=""))
			points(topo[,c("E","N")],col=1,pch=20,cex=2)
			lines(Eint,Nint,col=2,lwd=2)
			Sys.sleep(1)
		
			A <- matrix(nrow=length(Nint),ncol=3)
			A[,1] <- Eint
			A[,2] <- Nint
			A[,3] <- Zint
			colnames(A) <- c("E","N","Z")
			x@coord <- A
			x@proc <- c(x@proc, "coordinates added & traces interpolated")
		}
		return(x)
	}
)


setMethod("reverse", "GPR", function(x){
		xnew <- x
		xnew@data <- x@data[,length(x):1]
		# traces="numeric",	# trace number
		# depth="numeric",	# depth position
		# pos="numeric",		# position	of the traces					
		xnew@time0 <- rev(x@time0)
		xnew@time <- rev(x@time)
		xnew@com <- rev(x@com)
		xnew@ann <- rev(x@ann)
		if(length(gpr@coord)>0){
			xnew@coord <- x@coord[nrow(x@coord):1,]
		}
		if(length(gpr@rec)>0){
			xnew@rec <- x@rec[nrow(x@rec):1,]
		}
		if(length(gpr@trans)>0){
			xnew@trans <- x@trans[nrow(x@trans):1,]
		}
		return(xnew)
	}
)

#---------------------- DELINEATIONS ---------------------#

setMethod("delineate", "GPR", function(x,name=NULL,type=c("raster","wiggles"),add_topo=FALSE,nupspl=NULL,n=10000,...){
		if(is.null(dev.list())){
			stop("You must first plot the GPR profile with the function \"plot\"!\n")
		}
		xsave <- x
		itp <- locator(type="l", n=n)
		if(length(itp)>0){
			if(length(x@vel)>0){	
				vel <- x@vel[[1]]
			}else{
				vel <- 0
			}
			if(!is.null(nupspl)){
				x <- upsample(x,n=nupspl)
			}
			topo <- rep(0,length(x))
			type=match.arg(type)
			if(type=="raster"){
				if(add_topo){
					x <- migration(x)
				}
				# yvalues <- -rev(x@depth) 
				yvalues <- -(x@depth) 
				yvalues <- yvalues + mean(x@time0)
			}else if(type=="wiggles"){
				yvalues <- -rev(x@depth) 
				if(add_topo){
					topo <- x@coord[,3]
					topo <- topo - max(topo)
					yvalues <- yvalues * vel/ 2
					time_0 <- mean(x@time0)
					depth_0 <- depthToTime(z=0, time_0, v=vel, antsep=x@antsep) * vel/ 2
					yvalues <- yvalues + depth_0
				}
			}
			xvalues <- lineDist(x@coord)			
			posxOnPlot <- sapply(itp$x, myWhichMin, xvalues)
			posyOnPlot <- sapply(itp$y, myWhichMin, yvalues)
			mySel <- posxOnPlot >= 0 & posxOnPlot <= length(x) & posyOnPlot >= 0 & posyOnPlot <= nrow(x)
			posxOnPlot2 <- posxOnPlot[mySel]
			posPts <- posyOnPlot[mySel]
			posTrace <- x@traces[posxOnPlot2]
			xpos <- x@coord[posxOnPlot2,1]
			ypos <- x@coord[posxOnPlot2,2]
			# zpos <- x@coord[posPts,3]
			zpos <- itp$y[mySel]
			if(is.null(name)){
				xsave@delineations <- c(xsave@delineations, list(cbind(posTrace,posPts,xpos,ypos,zpos)))
			}else{
				name <- as.character(name)
				if(length(xsave@delineations[[name]])>0){
					xsave@delineations[[name]] <- c(xsave@delineations[[name]], list(cbind(posTrace,posPts,xpos,ypos,zpos)))
				}else{
					xsave@delineations[[name]] <- list(cbind(posTrace,posPts,xpos,ypos,zpos))
				}
			}
		}
		return(xsave)
	}
)

# add "manually" delineation to GPR data
# m - m or m - ns (depends on add_topo = FALSE/TRUE
setMethod("addDelineation", "GPR", function(x,itp, name=NULL,type=c("raster","wiggles"),add_topo=FALSE,...){
		if(is.null(dev.list())){
			stop("You must first plot the GPR profile with the function \"plot\"!\n")
		}
		xsave <- x
		# itp <- locator(type="l", n=n)
		topo <- rep(0,length(x))
		type=match.arg(type)
		if(type=="raster"){
			if(add_topo){
				x <- migration(x)
			}
			# yvalues <- -rev(x@depth) 
			yvalues <- -(x@depth) 
			yvalues <- yvalues + mean(x@time0)
		}else if(type=="wiggles"){
			yvalues <- -rev(x@depth) 
			if(add_topo){
				topo <- x@coord[,3]
				topo <- topo - max(topo)
				yvalues <- yvalues * vel/ 2
				time_0 <- mean(x@time0)
				depth_0 <- depthToTime(z=0, time_0, v=vel, antsep=x@antsep) * vel/ 2
				yvalues <- yvalues + depth_0
			}
		}
		xvalues <- lineDist(x@coord)			
		posxOnPlot <- sapply(itp$x, myWhichMin, xvalues)
		posyOnPlot <- sapply(itp$y, myWhichMin, yvalues)
		mySel <- posxOnPlot >= 0 & posxOnPlot <= length(x) & posyOnPlot >= 0 & posyOnPlot <= nrow(x)
		posxOnPlot2 <- posxOnPlot[mySel]
		posPts <- posyOnPlot[mySel]
		posTrace <- x@traces[posxOnPlot2]
		xpos <- x@coord[posxOnPlot2,1]
		ypos <- x@coord[posxOnPlot2,2]
		# zpos <- x@coord[posPts,3]
		zpos <- itp$y[mySel]
		if(is.null(name)){
			xsave@delineations <- c(xsave@delineations, list(cbind(posTrace,posPts,xpos,ypos,zpos)))
		}else{
			name <- as.character(name)
			if(length(xsave@delineations[[name]])>0){
				xsave@delineations[[name]] <- c(xsave@delineations[[name]], list(cbind(posTrace,posPts,xpos,ypos,zpos)))
			}else{
				xsave@delineations[[name]] <- list(cbind(posTrace,posPts,xpos,ypos,zpos))
			}
		}
		return(xsave)
	}
)


setReplaceMethod("rmDelineations", "GPR", function(x,values=NULL){
		deli <- x@delineations
		n_d <- length(deli)
		if(!is.null(values) && n_d >0 && values!="all"){
			n_tot <- sum(sapply(deli, lengthList))
			it <- 0
			values <- n_tot - values + 1
			for(i in n_d:1){
				if(typeof(deli[[i]])=="list"){
					n_sub_d <- length(deli[[i]])
					for(j in n_sub_d:1){
						it <- it + 1
						# itdel <- 0
						if(it %in% values){
						# if(values==it){
							# x@delineations[[i]][[j]] <- NULL
							# j <- j - itdel
							# cat("---- j=", j,"  ,   j-itdel=",j- itdel,"\n")
							x@delineations[[i]][j] <- NULL
							# itdel <- itdel + 1
							if(length(x@delineations[[i]])==0 || is.null(unlist(x@delineations[[i]]))){
								# x@delineations[[i]] <- NULL
								x@delineations[i] <- NULL
								# i<-i-1
								break
							}
							# break
							# print(x@delineations[[i]])
						}
					}
					# if(!is.null(x@delineations[[i]]) ||length(x@delineations[[i]])==0 || is.null(unlist(x@delineations[[i]]))){
						# x@delineations[i] <- NULL
					# }
				}else{
					it <- it + 1
					if(it %in% values){
					# if(values==it){
						x@delineations[i] <- NULL
						# i <- i-1
						# break
						# print(x@delineations)
					}
				}
			}
		}else if(n_d <1){
			warning("No delineation to delete\n")
		}else if(is.null(values)){
			stop("You must specified the no of the delineation you want to delete!\n")
		}else if(values=="all"){
			x@delineations <- list()
		}
		return(x)
	}
)


setMethod("delineations", "GPR", function(x,sel=NULL,...){
		deli <- x@delineations
		n_d <- length(deli)
		if(n_d >0){
			x_dist <- lineDist(x@coord)
			cat("*** delineated lines ****\n")
			it <- 0
			for(i in 1:n_d){
				if(typeof(deli[[i]])=="list"){
					n_sub_d <- length(deli[[i]])
					cat(names(deli[i]), ":\n",sep="")
					for(j in 1:n_sub_d){
						it <- it + 1
						tracePos <- sapply( deli[[i]][[j]][,1], myWhich, x@traces)
						xpos <- x_dist[tracePos]
						zpos <- deli[[i]][[j]][,4]
						cat(it,". length =", round(diff(range(xpos)),2), "; depth =",  round(diff(range(zpos)),2),"; number of pts =", length(xpos) ,"\n",sep="")
						# lines(xpos, zpos,col=col[i],...)
					}
				}else{
					it <- it + 1
					tracePos <- sapply( deli[[i]][,1], myWhich, x@traces)
					xpos <- x_dist[tracePos]
					zpos <- deli[[i]][,4]
					cat(it,". length =", round(diff(range(xpos)),2), "; depth =",  round(diff(range(zpos)),2),"; number of pts =", length(xpos) ,"\n",sep="")
					# lines(xpos, zpos,col=col[i],...)
				}
				cat("- - - - - - - - - - -\n")
			}
		}else{
			cat("No lines were delineated!\n")
		}
	}
)
setMethod("showDelineations", "GPR", function(x,sel=NULL,...){
		stop("This function is deprecated...\n Use \"delineations(x)\" instead of \"showDelineations(x)\"\n")
		# deli <- x@delineations
		# n_d <- length(deli)
		# if(n_d >0){
			# x_dist <- lineDist(x@coord)
			# cat("*** delineated lines ****\n")
			# it <- 0
			# for(i in 1:n_d){
				# if(typeof(deli[[i]])=="list"){
					# n_sub_d <- length(deli[[i]])
					# cat(names(deli[i]), ":\n",sep="")
					# for(j in 1:n_sub_d){
						# it <- it + 1
						# tracePos <- sapply( deli[[i]][[j]][,1], myWhich, x@traces)
						# xpos <- x_dist[tracePos]
						# zpos <- deli[[i]][[j]][,4]
						# cat(it,". length =", round(diff(range(xpos)),2), "; depth =",  round(diff(range(zpos)),2),"; number of pts =", length(xpos) ,"\n",sep="")
						# # lines(xpos, zpos,col=col[i],...)
					# }
				# }else{
					# it <- it + 1
					# tracePos <- sapply( deli[[i]][,1], myWhich, x@traces)
					# xpos <- x_dist[tracePos]
					# zpos <- deli[[i]][,4]
					# cat(it,". length =", round(diff(range(xpos)),2), "; depth =",  round(diff(range(zpos)),2),"; number of pts =", length(xpos) ,"\n",sep="")
					# # lines(xpos, zpos,col=col[i],...)
				# }
				# cat("- - - - - - - - - - -\n")
			# }
		# }else{
			# cat("No lines were delineated!\n")
		# }
	}
)
setMethod("exportDelineations", "GPR", function(x, path=""){
		myPath <- path
		x_dist <- lineDist(x@coord)
		deli <- x@delineations
		z0 <- max(coord(x)[,3]) 
		it <- 0
		for(i in seq_along(deli)){
			if(typeof(deli[[i]])=="list"){
				n_sub_d <- length(deli[[i]])
					for(j in n_sub_d:1){
						it<-it+1
						tracePos <- sapply( deli[[i]][[j]][,1], myWhich, x@traces)
						xprofile <- x_dist[tracePos]
						zabs <- z0 + deli[[i]][[j]][,5] 
						# zpos <- deli[[i]][[j]][,5]
						table_path_name <- paste(myPath,name(x),"_",it,"_",names(deli[i]),".txt",sep="")
						write.table(cbind(deli[[i]][[j]][,c("xpos","ypos","zpos")],zabs, xprofile),file=table_path_name, sep = ";", 
						row.names = FALSE, col.names = c("x","y","zr","z","xprofile"))
					}
			}else{
				it<-it+1
				tracePos <- sapply( deli[[i]][,1], myWhich, x@traces)
				xprofile <- x_dist[tracePos]
				zabs <- z0 + deli[[i]][[j]][,5] 
				table_path_name <- paste(myPath,name(x),"_",it,"_",names(deli[i]),".txt",sep="")
				write.table(cbind(deli[[i]][[j]][,c("xpos","ypos","zpos")],zabs,xprofile),file=table_path_name, sep = ";", 
				row.names = FALSE, col.names =  c("x","y","zr","z","xprofile"))
			}
		}
	}
)
setMethod("plotDelineations3D", "GPR", function(x,sel=NULL,col=NULL,add=TRUE,...){
		deli <- x@delineations
		n_d <- length(deli)
		if(n_d >0){
			if(is.null(col)){
				col <- 1:n_d
			}
			if(length(col)<=n_d){
				col <- rep(col, n_d)
			}
			if(add==FALSE){
				open3d()
			}
			for(i in 1:n_d){
				if(typeof(deli[[i]])=="list"){
					n_sub_d <- length(deli[[i]])
					for(j in 1:n_sub_d){
						tracePos <- sapply( deli[[i]][[j]][,1], myWhich, x@traces)
						# ptsPos <- deli[[i]][[j]][tracePos==deli[[i]][[j]][,1],2]
						xpos <- x@coord[tracePos,1] - x@coordref[1]
						ypos <- x@coord[tracePos,2] - x@coordref[2]
						z0 <- max(coord(x)[,3])   -   x@coordref[3]
						zpos <- z0 + deli[[i]][[j]][,5] 
						# X <- matrix(x, ncol=nc, nrow=nr, byrow=TRUE)
						# Y <- matrix(y, ncol=nc, nrow=nr, byrow=TRUE)
						# Z <-  matrix(z0, ncol=nc, nrow=nr, byrow=TRUE) - matrix(z, ncol=nc, nrow=nr, byrow=FALSE)
						# X <- 
						# Y <- 
						# Z <- 
						# surface3d(x, y, z, ..., normal_x=NULL, normal_y=NULL, normal_z=NULL)
						# points3d(ypos,zpos,xpos,col=col[i],...)
						lines3d(ypos,zpos,xpos, col=col[i],...)
					}
				}else{
					tracePos <- sapply( deli[[i]][,1], myWhich, x@traces)
					xpos <- x@coord[tracePos,1] - x@coordref[1]
					ypos <- x@coord[tracePos,2] - x@coordref[2]
					z0 <- max(coord(x)[,3])   -   x@coordref[3]
					zpos <- z0 + deli[[i]][,5] 
					# points3d(ypos,zpos,xpos,col=col[i],...)
					lines3d(ypos,zpos,xpos, col=col[i],...)
				}
			}
		}
	}
)
setMethod("plotDelineations", "GPR", function(x,sel=NULL,col=NULL,...){
		if(is.null(dev.list())){
			stop("You must first plot the GPR profile with the function \"plot\"!\n")
		}
		deli <- x@delineations
		n_d <- length(deli)
		if(n_d >0){
			x_dist <- lineDist(x@coord)
			if(is.null(col)){
				col <- 1:n_d
			}
			if(length(col)<=n_d){
				col <- rep(col, n_d)
			}
			for(i in 1:n_d){
				if(typeof(deli[[i]])=="list"){
					n_sub_d <- length(deli[[i]])
					for(j in 1:n_sub_d){
						tracePos <- sapply( deli[[i]][[j]][,1], myWhich, x@traces)
						xpos <- x_dist[tracePos]
						zpos <- deli[[i]][[j]][,5]
						lines(xpos, zpos,col=col[i],...)
					}
				}else{
					tracePos <- sapply( deli[[i]][,1], myWhich, x@traces)
					xpos <- x_dist[tracePos]
					zpos <- deli[[i]][,5]
					lines(xpos, zpos,col=col[i],...)
				}
			}
		}else{
			cat("No lines were delineated!\n")
		}
	}
)
setMethod("identifyDelineation", "GPR", function(x,sel=NULL,...){
		if(is.null(dev.list())){
			stop("You must first plot the GPR profile with the function \"plot\"!\n")
		}
		XY <- list()
		deli <- x@delineations
		n_d <- length(deli)
		it <- 0
		if(n_d >0){
			x_dist <- lineDist(x@coord)
			for(i in 1:n_d){
				if(typeof(deli[[i]])=="list"){
					n_sub_d <- length(deli[[i]])
					for(j in 1:n_sub_d){
						tracePos <- sapply( deli[[i]][[j]][,1], myWhich, x@traces)
						xpos <- x_dist[tracePos]
						zpos <- deli[[i]][[j]][,5]
						it <- it + 1
						XY[[it]] <- cbind(xpos,zpos,rep(it,length(xpos)))
						# it <- it + 1
						# lines(xpos, zpos,col=col[i],...)
					}
				}else{
					tracePos <- sapply( deli[[i]][,1], myWhich, x@traces)
					xpos <- x_dist[tracePos]
					zpos <- deli[[i]][,5]
					it <- it + 1
					XY[[it]] <- cbind(xpos,zpos,rep(it,length(xpos)))
					# lines(xpos, zpos,col=col[i],...)
				}
			}
			XY <- do.call(rbind,XY)
			A<-identify(XY, labels=XY[,3])
			return(XY[A,3])
		}else{
			cat("No lines were delineated!\n")
		}
	}
)

#---------------------- MIGRATION ---------------------#

setMethod("migration", "GPR", function(x,type=c("static","kirchhoff"),...){
		if(missing(type)){
			type=match.arg(type)
			suppl_args <- list("type"=type)
			# cat(type,"\n")
		}
		type=match.arg(type)
		if(type=="static"){	
			ntr <- ncol(x@data)
			if(ncol(x@coord) == 3 && length(x@coord[,3])>= ntr){
				topo <- x@coord[1:ntr,3]
			}else{
				topo <- rep.int(0L, ntr)
				cat("no topo!\n")
			}
			if(x@depthunit == "ns"){
				# "migration"
				cat("time to depth conversion with constant velocity",x@vel[[1]],"\n")
				x@dz <-  x@dz * x@vel[[1]]/ 2
				x@depthunit <- "m"
				time_0 <- mean(x@time0)
				# depth_0 <- time_0 * x@vel[[1]]/ 2
				depth_0 <- depthToTime(z=0, time_0 , v=x@vel[[1]], antsep=x@antsep) * x@vel[[1]]/ 2
				depth_all <- x@depth* x@vel[[1]]/ 2
				# shift to time0
				sel <- c(round((depth_0-depth_all[1])/x@dz):nrow(x))
				# sel <- c(round((depth_0)/x@dz):nrow(x))
				x <- x[sel,]
			}
			x@data <- topoShift(x@data,topo,dz = x@dz)
			x@depth <- seq(0,by=x@dz,length.out=nrow(x@data))	#x@depth * x@vel[[1]]/ 2
			x@time0 <- rep(0,length(x@time0))
			proc <- get_args()
			proc <- addArg(proc,suppl_args)
			proc <- paste(proc,"#v=",x@vel[[1]],sep="")
			x@vel=list()	# FIX ME!!
			x@time0 <- rep(0L,x@ntr)	# FIX ME!!
			x@proc <- c(x@proc, proc)
		}
		return(x)
	} 
)

#---------------------- INTERPOLATION ---------------------#	
setMethod("upsample", "GPR", function(x,n){
		n <- abs(round(n))
		x@data <- .upsample(x@data, n=n, type=c("DFT"))
		x@data <- x@data[,1:(ncol(x@data))]
		yvalues <-  (seq(0,by=x@dz,length.out=nrow(x@data)))
		
		xvalues  <- doubleVector(x@pos,n=n)
		yvalues  <- doubleVector(x@depth,n=n)
		#  
		# image(xvalues,yvalues,t(x@data))

		ntr <- ncol(x@data)	# number of traces
		if(ntr!=length(xvalues)) stop("ntr!=length(xvalues)")
		if(length(x@coord)>0){
			coord_new <- matrix(ncol=3,nrow=ntr)
			coord_new[,1] <- signal::interp1(x@pos, x@coord[,1], xi = xvalues,   method = c( "linear"), extrap = TRUE)
			coord_new[,2] <- signal::interp1(x@pos, x@coord[,2], xi = xvalues,   method = c( "linear"), extrap = TRUE)
			coord_new[,3] <- signal::interp1(x@pos, x@coord[,3], xi = xvalues,   method = c( "linear"), extrap = TRUE)
			x@coord <- coord_new
		}
		if(length(x@rec)>0){
			rec_new <- matrix(ncol=3,nrow=ntr)
			rec_new[,1] <- signal::interp1(x@pos, x@rec[,1], xi = xvalues,   method = c( "linear"), extrap = TRUE)
			rec_new[,2] <- signal::interp1(x@pos, x@rec[,2], xi = xvalues,   method = c( "linear"), extrap = TRUE)
			rec_new[,3] <- signal::interp1(x@pos, x@rec[,3], xi = xvalues,   method = c( "linear"), extrap = TRUE)
			x@rec <- rec_new
		}
		if(length(x@trans)>0){
			trans_new <- matrix(ncol=3,nrow=ntr)
			trans_new[,1] <- signal::interp1(x@pos, x@trans[,1], xi = xvalues,   method = c( "linear"), extrap = TRUE)
			trans_new[,2] <- signal::interp1(x@pos, x@trans[,2], xi = xvalues,   method = c( "linear"), extrap = TRUE)
			trans_new[,3] <- signal::interp1(x@pos, x@trans[,3], xi = xvalues,   method = c( "linear"), extrap = TRUE)
			x@trans <- trans_new
		}
		
		x@traces <- seq.int(1L,by=1L,length.out=ntr)
		#fiducial markers (fid, comments)
		if(length(x@com) >0 && sum(x@com != "")>0){
			newfid <- character(length(x@com)*n)
			newfidPos <- which(x@com!="")
			newfid[newfidPos*n] <- x@com[newfidPos]
			x@com <- newfid[1:ntr]
		}
		#annotations
		if(length(x@ann) >0 && sum(x@ann != "")>0){
			newAnn <- character(length(x@ann)*n)
			newAnnPos <- which(x@ann!="")
			newAnn[newAnnPos*n] <- x@ann[newAnnPos]
			x@ann <- newAnn[1:ntr]
		}
		# trace positions
		x@pos <- xvalues
		# depth/time
		x@depth <- doubleVector(x@depth,n=n)

		x@dz <- x@dz / n
		x@dx <- x@dx / n
		x@ntr <- ntr
		
		proc <- get_args()
		x@proc <- c(x@proc, proc)
		return(x)
	} 
)


#----------------------- SAVE/EXPORT ------------------------#
setMethod("writeGPR", "GPR", function(x,path, format=c("DT1","rds")){
		type <- match.arg(format)
		ext <-  tolower(substr(path,start=nchar(path)-3,stop=nchar(path)))
		if(type == "DT1"){
			if(".dt1" != ext ){
				stop("Extension should be '.DT1'")
			}
			stop("not implemented yet!")
		}else if(type == "rds"){
			if(".rds" != ext ){
				stop("Extension should be '.rds'")
			}
			x@filename <- as.character(path)
			saveRDS(x, path)
		}
		# 	mod2 <- readRDS("mymodel.rds")
	} 
)

setMethod("exportPDF", "GPR", function(x,filepath=NULL,add_topo=FALSE,clip=NULL,normalize=NULL,nupspl=NULL,...){
		if(is.null(filepath)){
			stop("filepath must be given\n")
		}
		if(any(dim(x) == 1)){
			stop("no export because dim = 1\n")
		}
		plot(x,clip=clip, add_topo=add_topo,type="wiggles",pdfName=filepath,normalize=normalize,nupspl=nupspl,...)
	}
)

setMethod("exportFID", "GPR", function(x,filepath=NULL){
		# Trace	Position	Comment	PNAME
		tr_start <- 1
		tr_end <- length(x)
		tr <- which(fid(x) != "" & fid(x) != "skip")
		trcom <- fid(x)[tr]

		if(!(tr_start %in% tr)){	
			tr <- c(tr_start,tr)
			trcom <- c("F0",trcom)
		}
		if(!(tr_end %in% tr)){
			tr <- c(tr,tr_end)
			lastF <-regmatches(trcom[length(trcom)],regexpr(pattern="[[:digit:]]+",trcom[length(trcom)]))
			if(length(lastF)>0){
				trcom <- c(trcom,paste("F",as.numeric(lastF)+1,sep=""))
			}else{
				trcom <- c(trcom,"Fend")
			}
		}
		trpos <- x@pos[tr]
		# trcom <- character(length(tr))
		FID <- data.frame("TRACE" = tr,"POSITION" = trpos, "COMMENT" = trcom)

		if(is.null(filepath)){
			return(FID)
		}else{
			write.table(FID, filepath, sep=",",row.names = FALSE, col.names = TRUE, quote=FALSE)
		}
	} 
)


setMethod("exportCoord", "GPR", function(x,filename=NULL,folder='.',type=c("points","lines")){
	type=match.arg(type)
	TOPO <- x@coord
	Names <- x@name
	if(type=="lines"){	
		topoLines <- sp::Lines(sp::Line(TOPO[,1:2]),x@name)
		mySpatLines <- SpatialLines(list(topoLines))
		if(crs(x) == '' || nchar(crs(x)) == 1){
			warning("no CRS defined!\n")
		}else{
			proj4string(mySpatLines) <- CRS(crs(x))
		}
		
		d.f <- data.frame(z=c(1), row.names = x@name)

		mySpatLinesdf <- SpatialLinesDataFrame(mySpatLines, d.f , match.ID = TRUE)

		writeOGR(mySpatLinesdf, folder, filename, driver="ESRI Shapefile")
	}else if(type=="points"){	
		# allNames <- sapply(rep(Names, each=sapply(TOPO, length))
		# A <- cbind(allTopo,allNames)
		# allTogether <- as.data.frame(cbind(allTopo,allNames))
		allTopo <- as.data.frame(TOPO)
		myTr <- c("tr"=x@traces)
		allTopo <- cbind(allTopo, myTr)
		colnames(allTopo) <- c(colnames(TOPO),"tr")
		coordinates(allTopo) = ~E + N
		if(crs(x) == '' || nchar(crs(x)) == 1){
			warning("no CRS defined!\n")
		}else{
			proj4string(allTopo) <- CRS(crs(x))
		}
		writeOGR(allTopo, folder, filename, driver="ESRI Shapefile")
	}
})

