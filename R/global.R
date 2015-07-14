
	
#----------- helper functions -------------------#
# FID <- choose.files(caption = " txt files",filters = c("txt","*.txt"))
# output = list of data frame (one for each file from FID) 
#		with c("E","N","Z","TRACE") structure
readFID <- function(FID,sep=","){
	myFid <- list() 
	for(i in seq_along(FID)){
		cat("read ", FID[[i]],"...\n",sep="")
		A <- read.table(FID[[i]],sep=",",stringsAsFactors=FALSE,header=TRUE)
		colnames(A) <- toupper(colnames(A))
		if(!all(c("E","N","Z","TRACE") %in% colnames(A))){
			stop("The headers should be \"E\",\"N\",\"Z\",\"TRACE\"!\n")
		}
		myFid[[i]] <- A[,c("E","N","Z","TRACE")]
	}
	return(myFid)
}

readTopo <- function(TOPO,sep=","){
	myTopo <- list() 
	for(i in seq_along(TOPO)){
		A <- read.table(TOPO[[i]],sep=",",stringsAsFactors=FALSE,header=TRUE)
		colnames(A) <- toupper(colnames(A))
		if(ncol(A) < 3){
			stop("The headers should be \"E\",\"N\",\"Z\"!\n")
		}
		myTopo[[i]] <- A[,1:3]
	}
	return(myTopo)
}
 	
plotTopo <- function(NEZ_file, add=TRUE){
	
	topo <- read.table(NEZ_file, header=TRUE, sep=",", stringsAsFactors = FALSE)
	# topo$N <- -topo$N

	PCODE <- unique(topo$PCODE)

	TS 		<- agrep("TS" , PCODE)		# TOTAL STATION
	REF 	<- agrep("REF" , PCODE)		# TOTAL STATION
	WATER 	<- agrep("WATER" , PCODE)		# TOTAL STATION
	CROSS 	<- which("CROSS" == PCODE)		# TOTAL STATION
	REVERSE	<- agrep("REVERSE", PCODE)	# 180° hor, ver
	LINES 	<- agrep("LINE", PCODE) 
	LINES 	<- LINES[!(agrep("LINE", PCODE) %in% REVERSE)]

	POINTS 	<- which(!(1:length(PCODE) %in% c(LINES,TS,REVERSE,WATER,CROSS, REF)))		# topo
	NOT_REVERSE <- !(1:length(PCODE) %in% agrep("REVERSE", PCODE))

	not_rev <- !(1:nrow(topo) %in% agrep("REVERSE",topo$PCODE))


	#----------------------------------------------------
	if(add==FALSE){
		plot(topo[not_rev,c("E","N")],type="n", asp=1)
	}
	# plot(0,0,type="n", asp=1,xlim=c(346485, 346579),ylim=c(5119381,5119433.57))
	# plot(c(-50,120),c(-190, 170),type="n", asp=1)

	# reverse
	for(i in 1:length(REVERSE)){
		points( - topo[topo[,"PCODE"]==PCODE[REVERSE[i]],c("E","N")],pch=20,col=1)
	}

	# Water
	points( topo[topo[,"PCODE"] %in% PCODE[WATER],c("E","N")],pch=10,col=1)

	# points
	for(i in 1:length(POINTS)){
		points(topo[topo[,"PCODE"]==PCODE[POINTS[i]],c("E","N")],pch=3,col=1,cex=0.7)
	}

	# ref
	points(topo[topo[,"PCODE"]%in% PCODE[REF],c("E","N")],pch=25,col=3,bg="green")

	# TOTAL STATION TS
	# points(topo[topo[,"PCODE"]==PCODE[TS[1]],c("E","N")],pch=11,col=1)

	# LINES
	# total_GPRLine_distance <- 0
	# for(i in 1:length(LINES)){
		# lines(topo[topo[,"PCODE"]==PCODE[LINES[i]],c("E","N")],col=i)
		# points(topo[topo[,"PCODE"]==PCODE[LINES[i]],c("E","N")],col="red",pch=20)
		# GPRLineDistance <- myDist(topo[topo[,"PCODE"]==PCODE[LINES[i]],c("E","N")],last=TRUE)
		# cat(PCODE[LINES[i]], ' : ', round(GPRLineDistance,2)," m\n")
		# total_GPRLine_distance <- total_GPRLine_distance +GPRLineDistance
	# }

	# abline(h=0,v=0, lty=3, col="grey")
	# cat("total distance =", total_GPRLine_distance,"\n")
	#----------------------------------------------------
	
}
	
#----------- helper functions -------------------#



# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

.filename <- function(x){
	unlist(lapply(strsplit(basename(x),"[.]"), head , 1 ))
}
extension <- function(x){
	cat("with caution... because split \'.\' may not be so good\n")
	unlist(lapply(strsplit(basename(x),"[.]"), tail , 1 ))
}

lineDist <- function(loc,last=FALSE){
  loc <- as.matrix(loc)
  all_dist <- cumsum(c(0,sqrt(apply(diff(loc)^2,1,sum))))
  if(last){
        return(all_dist[length(all_dist)])
  }else{
        return(as.numeric(all_dist))
  }
}
# save your object with saveRDS()
# example:
#	saveRDS(mod, "mymodel.rds")
# 	mod2 <- readRDS("mymodel.rds")
	

#--------------------------------------------#
#---------------- SETGENERIC ----------------#
# setGenericVerif <- function(x,y){if(!isGeneric(x)){setGeneric(x,y)}else{cat("setGeneric", x,"already exists!\n")}}
setGenericVerif <- function(x,y){setGeneric(x,y)}


#------------------------------
setGenericVerif("setCoordref", function(x) standardGeneric("setCoordref"))


#------------------------------
setGenericVerif("getLine", function(x,no) standardGeneric("getLine"))



# select a box on plot(mySurvey) and return list(xlim,ylim)
# that can be used in plot3D:
# 	plot(mySurvey,asp=1)
# 	bbox <- selectBBox()
# 	plot3D(mySurvey, add_topo=TRUE,clip=30, xlim=bbox$xlim,ylim=bbox$ylim, add=FALSE)

selectBBox <- function(border="red",lwd=2,...){
	bbox <- locator(type="p",n=2)
	LIM <- sapply(bbox, range)
	rect(LIM[1,"x"], LIM[1,"y"], LIM[2,"x"], LIM[2,"y"], border=border)
	return(list("xlim"=LIM[,"x"], "ylim" =LIM[,"y"]))
}


# fidpos(x@coords[[8]],x@fids[[8]])

fidpos <- function(xyz,fid){
	return(xyz[trim(fid)!="",,drop=FALSE])
}

plotLine <- function(xyz,...){
	# print(list(...))
	lines(xyz[,1:2],...)
	# arrows(x0=xyz[nrow(xyz)-1,1],y0=xyz[nrow(xyz)-1,2],x1=xyz[nrow(xyz),1],y1=xyz[nrow(xyz),1])
}
plotArrows <- function(xyz,...){
	arrows(xyz[nrow(xyz)-1,1],xyz[nrow(xyz)-1,2],xyz[nrow(xyz),1],xyz[nrow(xyz),2],length = 0.1,col="red",...)
	# arrows(x0=xyz[nrow(xyz)-1,1],y0=xyz[nrow(xyz)-1,2],x1=xyz[nrow(xyz),1],y1=xyz[nrow(xyz),1])
}




setGenericVerif("surveyIntersections", function(x) standardGeneric("surveyIntersections"))


setGenericVerif("intersections", function(x) standardGeneric("intersections"))


# update the GPR FILES!!!
setGenericVerif("interpTraces", function(x, topo) standardGeneric("interpTraces"))

# already defined!!
# setGenericVerif("interpTraces", function(x, topo) standardGeneric("interpTraces"))


setGenericVerif("coords<-",function(x,values){standardGeneric("coords<-")})


setGenericVerif("writeGPR", function(x,path, format=c("DT1","rds")) standardGeneric("writeGPR"))



setGenericVerif("exportCoord",  function(x,filepath=NULL,type=c("points","lines"),driver="ESRI Shapefile",...) standardGeneric("exportCoord"))

setGenericVerif("exportProc",  function(x,filepath=NULL,sep="\t", row.names=FALSE,
	col.names=FALSE, ...) standardGeneric("exportProc"))

setGeneric("reverse", function(x) standardGeneric("reverse"))



 

timeToDepth <- function(tt, time_0, v=0.1, antsep=1){
	t0 <- time_0 - antsep/0.299
	sqrt(v^2*(tt-t0)- antsep^2)/2
}
depthToTime <- function(z, time_0, v=0.1, antsep=1){
	t0 <- time_0 - antsep/0.299
	sqrt((4*z^2 + antsep^2)/(v^2)) + t0
}
depth0 <- function(time_0, v=0.1, antsep=1){
	time_0 - antsep/0.299 + antsep/v
}

setGeneric("plot3D", function(x,add_topo=FALSE,clip=NULL,normalize=NULL,nupspl=NULL,add=TRUE,xlim=NULL,ylim=NULL,zlim=NULL,...) 
standardGeneric("plot3D"))



.plot3D <- function(A,x,y,z,z0,col=diverge_hcl(101, h = c(246, 10), c = 120, l = c(30, 90)),back="fill", smooth = TRUE, lit=FALSE, lwd=0,empty=FALSE,...){
	nr = nrow(A)
	nc = ncol(A)
	if(empty==TRUE){
		X <- matrix(x, ncol=nc, nrow=2, byrow=TRUE)
		Y <- matrix(y, ncol=nc, nrow=2, byrow=TRUE)
		Z <-  matrix(z0, ncol=nc, nrow=2, byrow=TRUE) - matrix(z[c(1,nr)], ncol=nc, nrow=2, byrow=FALSE)
		colA <- col[1]
		if(!is.null(list(...)$alpha) && (list(...)$alpha==0 || is.null(col))){
		
		}else{
			rgl.surface(Y, X, Z, color=colA, back=back, smooth = smooth, lit=lit, lwd= lwd,...) 
		}
		lines3d(y,z0,x, col="black",alpha=1,lwd=lwd)	 
		lines3d(y,(z0-z[length(z)]),x, col="black",alpha=1,lwd=lwd)	 
		lines3d(rep(y[1],2),(z0[1]-z),rep(x[1],2), col="black",alpha=1,lwd=lwd)	 
		lines3d(rep(y[length(y)],2),(z0[length(z0)]-z),rep(x[length(x)],2), col="black",alpha=1,lwd=lwd)	 

	}else{
		X <- matrix(x, ncol=nc, nrow=nr, byrow=TRUE)
		Y <- matrix(y, ncol=nc, nrow=nr, byrow=TRUE)
		Z <-  matrix(z0, ncol=nc, nrow=nr, byrow=TRUE) - matrix(z, ncol=nc, nrow=nr, byrow=FALSE)
		A = (A-min(A))/(max(A)-min(A))
		colA <- col[ (A)*100+1 ] # assign colors to heights for each point 
		rgl.surface(Y, X, Z, color=colA, back=back, smooth = smooth, lit=lit, lwd= lwd,...) 
	}
}

# NICHT BENUTZT!!!
setCol <- function(A , col = diverge_hcl(101, h = c(246, 10), c = 120, l = c(30, 90))){
	CCY = (A-min(A,na.rm=TRUE))/(max(A,na.rm=TRUE)-min(A,na.rm=TRUE))
	ClimY <- range(CCY,na.rm=TRUE)
	ClenY <- ClimY[2] - ClimY[1] + 1
	# col <- tim.colors(101) # height color lookup table
	#col = palette(gray(0:101 / 101))
	col[ (CCY)*100+1 ] 
}

plot3DSlice <- function(XYZ,slice=c("x","y","z"),section=1,col=diverge_hcl(101, h = c(246, 10), c = 120, l = c(30, 90)), sampling = c(0.25,0.25,0.04),rmStripes = TRUE){
	# k=100
	# j=25
	# i=40
	# col <- tim.colors(101) # height color lookup table
	slice = match.arg(slice)
	if(length(slice)>1){
		slice = slice[1]
	}
	
	dimXYZ = dim(XYZ)
	vz = seq(0,dimXYZ[3]-1,by=1)*sampling[3]	# dtime / 2 * v
	vx = seq(0,dimXYZ[1]-1,by=1)*sampling[1]
	vy = seq(0,dimXYZ[2]-1,by=1)*sampling[2]
	if(rgl.cur()==0){	# si la fenêtre rgl est ouverte, on plot dedans...
		rgl.open()
		rgl.bg( color=c("white"))
	}
	i = section
	j=i
	k=i
	if(slice=="x"){
		if(rmStripes == TRUE){ Xside = normalizeGPR(removeStripes(t(XYZ[,j,])))
		}else{  Xside = normalizeGPR((t(XYZ[,j,])))	}
		
		Xside_x = matrix(vx,nrow=dimXYZ[3],ncol=dimXYZ[1],byrow=TRUE)
		Xside_y = matrix( vy[j],nrow=dimXYZ[3],ncol=dimXYZ[1],byrow=TRUE)
		Xside_z = matrix( max(vz)-vz,nrow=dimXYZ[3],ncol=dimXYZ[1],byrow=FALSE)

		CCX = (Xside-min(Xside))/(max(Xside)-min(Xside))
		ClimX <- range(CCX)
		ClenX <- ClimX[2] - ClimX[1] + 1
		# col <- tim.colors(101) # height color lookup table
		#col = palette(gray(0:101 / 101))
		colCX <- col[ (CCX)*100+1 ] 
		
		surface3d(Xside_x, Xside_z, Xside_y, col= setCol(Xside), lit=FALSE,front="fill",back="fill")#, alpha=0.5)
	}else if(slice=="z"){
		if(rmStripes == TRUE){ Zside = (removeStripes(t(XYZ[,,k])))
		}else{  Zside = ((t(XYZ[,,k])))	}
		
		Zside_x = matrix(vx,nrow=dimXYZ[2],ncol=dimXYZ[1],byrow=TRUE)
		Zside_y = matrix( vy,nrow=dimXYZ[2],ncol=dimXYZ[1],byrow=FALSE)
		Zside_z = matrix(max(vz) - vz[k],nrow=dimXYZ[2],ncol=dimXYZ[1],byrow=FALSE)

		CCZ = (Zside-min(Zside))/(max(Zside)-min(Zside))
		ClimZ <- range(CCZ)
		ClenZ <- ClimZ[2] - ClimZ[1] + 1
		#col = palette(gray(0:101 / 101))
		colCZ <- col[ (CCZ)*100+1 ]
		
		surface3d(Zside_x, Zside_z, Zside_y, col= setCol(Zside), lit=FALSE,front="fill",back="fill")#, alpha=0.5)
	}else if(slice=="y"){
		if(rmStripes == TRUE){ Yside = normalizeGPR(removeStripes(t(XYZ[i,,])))
		}else{  Yside = normalizeGPR((t(XYZ[i,,])))	}
		
		Yside_x = matrix(vx[i],nrow=dimXYZ[3],ncol=dimXYZ[2],byrow=TRUE)
		Yside_y = matrix( vy,nrow=dimXYZ[3],ncol=dimXYZ[2],byrow=TRUE)
		Yside_z = matrix( max(vz)-vz,nrow=dimXYZ[3],ncol=dimXYZ[2],byrow=FALSE)

		CCY = (Yside-min(Yside))/(max(Yside)-min(Yside))
		ClimY <- range(CCY)
		ClenY <- ClimY[2] - ClimY[1] + 1
		# col <- tim.colors(101) # height color lookup table
		#col = palette(gray(0:101 / 101))
		colCY <- col[ (CCY)*100+1 ] 
		
		surface3d(Yside_x, Yside_z, Yside_y, col= setCol(Yside), lit=FALSE,front="fill",back="fill")#, alpha=0.5)
	}
}


setGeneric("exportPDF", function(x,filepath=NULL,add_topo=FALSE,clip=NULL,normalize=NULL,nupspl=NULL,...) standardGeneric("exportPDF"))



#---------------------- DELINEATIONS ---------------------#

setGeneric("delineate", function(x,name=NULL,type=c("raster","wiggles"),add_topo=FALSE,nupspl=NULL,n=10000,...) standardGeneric("delineate"))

setGeneric("rmDelineations<-", function(x,values=NULL) standardGeneric("rmDelineations<-"))

setGeneric("delineations", function(x,sel=NULL,...) standardGeneric("delineations"))
setGeneric("addDelineation", function(x,...) standardGeneric("addDelineation"))

setGeneric("showDelineations", function(x,sel=NULL,...) standardGeneric("showDelineations"))

setGeneric("exportDelineations", function(x, path="") standardGeneric("exportDelineations"))

setGeneric("plotDelineations3D", function(x,sel=NULL,col=NULL,add=TRUE,...) standardGeneric("plotDelineations3D"))
	
setGeneric("plotDelineations", function(x,sel=NULL,col=NULL,...) standardGeneric("plotDelineations"))

setGeneric("identifyDelineation", function(x,sel=NULL,...) standardGeneric("identifyDelineation"))

myWhichMin <- function(x,y){
	which.min(abs(x-y))
}
myWhich <- function(x,y){
	which(x==y)
}
lengthList <- function(x){
	if(typeof(x)=="list"){
		return(length(x))
		# print(typeof(x))
	}else{
		return(1)
	}
}


#---------------------- MIGRATION ---------------------#

setGeneric("migration", function(x,type=c("static","kirchhoff"), ...) standardGeneric("migration"))
	



topoShift <- function(A,topo,dz){
	zShift <- (max(topo) - topo)
	old_t <- seq(0,length.out=nrow(A),by=dz)
	A_topoShift <- matrix(0,nrow=nrow(A)+floor(max(zShift)/dz),ncol=ncol(A))
	n <- 1:(nrow(A)-2)
	for(i in 1:ncol(A)){
		# compute new t-vector for each trace
		new_t <- old_t + zShift[i]
		xit <- seq(ceiling(new_t[1]/dz), ceiling(new_t[nrow(A)-2]/dz))
		# interpolate
		A_topoShift[xit+1,i] = signal::interp1(new_t, A[,i], xi = xit*dz, method = "cubic",extrap = TRUE)	# FIX ME! does not work well (not nice interpolation)
	}
	return(A_topoShift)
}

#---------------------- INTERPOLATION ---------------------#		
		
setGenericVerif("upsample", function(x,n) standardGeneric("upsample"))


doubleVector <- function(v,n=2L){
	if(n > 1){
		m <- length(v)
		dxpos <- rep( diff(v)/n,n-1)
		vv <- v[-m] + rep(seq(1,n-1),each=m-1)*dxpos
		xvalues  <- sort(c(v,vv ,v[m] + cumsum(rep(dxpos[length(dxpos)],n-1))))
		xvalues <- xvalues[1:(length(xvalues))]
	}
}
# triangle <- function(x,y,w,...){
	# polygon(c(x-w/2,w,w/2+x),c(y+1.5*w,y,y+1.5*w),...)
# }

# plotWig(x@data,x=xvalues, y= -rev(x@depth), main=x@name, xlab=x@posunit, ylab=ylab, topo= topo,
					# note=x@filename,col="black",time_0=x@time0,antsep=x@antsep, v=vel,fid=x@com,ann=x@ann,
					# depthunit=x@depthunit,dots)
plotWig <- function(A, x=NULL, y=NULL, xlim = NULL, ylim=NULL, topo=NULL, main ="", note=NULL,  
					fid=NULL,ann = NULL, add_ann=TRUE,pdfName=NULL, ws =1, side=1, dx=0.25, dz=0.4, ratio=1,
					col=black, time_0=0, antsep=1, v=0.1,depthunit="ns",lwd=0.5,...){
	dx <- mean(diff(x)) # estimated x-step
	A[is.na(A)]=0
	A =  A/max(abs(A))*dx
	# A =  as.matrix(A)/max(abs(A))
	# v <- ifelse(is.null(v),1,v/2)
	nr = nrow(A)
	nc = ncol(A)
	A <- A[nr:1,]
	time_0 <- mean(time_0)
	if(is.null(y)){
		y <- -(ncol(GPR):1)
	}
	y0 <- 0
	if(is.null(topo)){
		topo <- rep(0L,nc)
	}else{
		# conversion ns to m!
		if(grepl("[s]$",depthunit)){
			# timeToDepth <- function(tt, time0, v=0.1, antsep=1){
				# t0 <- time0 - antsep/0.299
				# sqrt(v^2*(tt-t0)- antsep^2)/2
			# }
			y <-  y * v/ 2
			depthunit <- "m"
		}
		topo <- topo - max(topo)
	}
	if(grepl("[s]$",depthunit)){
		# y <- y + time0
	}else if(grepl("[m]$",depthunit)){
		depth_0 <- depthToTime(z=0, time_0 , v=v, antsep=antsep) * v/ 2
		y <- y + depth_0
	}
	
	if(is.null(xlim) ){
       # xlim <- range(x) 
       xlim <- range(x)  + c(-1,1)*dx
	   test <- rep(TRUE,length(x))
	}else{
		test <- ( x >= xlim[1] & x <= xlim[2] )
		xlim <- xlim + c(-1,1)*dx
	}
	if(is.null(ylim) ){
       ylim <-  range(y) + range(topo)
	}
	
	omi=c(0,0,0.6,0)
	mgp=c(2.5, 0.75, 0)
	fac <- 0.2
	# if the depthunit are "meters"
	if(grepl("[m]$",depthunit)){
		mai=c(1,0.8,0.6,0.4)+0.02
		heightPDF <- fac*diff(ylim) + sum(omi[c(1,3)] + mai[c(1,3)])
		widthPDF <- fac*diff(xlim)*ratio +  sum(omi[c(2,4)]+ mai[c(2,4)])
	}else{
		mai=c(1,0.8,0.6,0.8)+0.02 
		heightPDF <- fac*(ylim[2] - ylim[1])*v/ 2 + sum(omi[c(1,3)] + mai[c(1,3)])
		widthPDF <- fac*(xlim[2] - xlim[1])*ratio + sum(omi[c(2,4)] + mai[c(2,4)])
	}
	if(!is.null(pdfName)){
		CairoPDF(file = paste(pdfName,".pdf",sep=""),
			  # pointsize=10,
			  width = widthPDF, 
			  height = heightPDF,
			  # dpi=75,	# 75
			  bg = "white",
			  pointsize=10,
			  # units = "in",
			  title = pdfName)	
	}
	par(mai=mai,omi=omi,mgp=mgp)
		  
	plot(0,0, type="n", xaxs="i", yaxs="i", axes=FALSE,
			xlim=xlim, ylim=ylim, ...)
	title(main,outer=TRUE,line=1)
	if(!is.null(fid) && length(fid)>0 && any(fid!="")){
		pin <- par("pin")
		usr <- par("usr")
		cin <- par()$cin[2]
		posfid <- x[test]
		fid <- fid[test]
		testfid <- (fid != "")
		yr <- diff(usr[3:4])/(pin[2])
		if(sum(testfid)>0){	
			par(xpd=TRUE)
			cst <- yr*cin
			points(posfid[testfid],cst/2*0.75+rep(ylim[2],sum(testfid)),pch=25,col="red",bg="yellow",cex=1)
			text(posfid[testfid],cst+rep(ylim[2],sum(testfid)),fid[testfid],cex=0.6)#,pos=3,offset =0)
			par(xpd=FALSE)
		}
	}
	if(side==1){
		for(i in rev(seq_along(x))){
			y2 <- y + topo[i]
			wig = cbind(ws*A[,i]+x[i],y2)
			wig1 = rbind(c(x[i],y2[1]),wig,c(x[i],y2[nr]))
			polygon(wig1, col = col, border=NA)
			rect(min(wig1[,1]), ylim[1], x[i], ylim[2],col="white",border=NA)
			# lines(x[i]+ws*A[,i],y2,lwd=lwd)
		}
	}else{
		for(i in (seq_along(x))){
			y2 <- y + topo[i]
			wig = cbind(ws*A[,i]+x[i],y2)
			wig1 = rbind(c(x[i],y2[1]),wig,c(x[i],y2[nr]))
			polygon(wig1, col = col, border=NA)
			rect(max(wig1[,1]), ylim[1], x[i], ylim[2],col="white",border=NA)
		}
	}
	for(i in (seq_along(x))){
		y2 <- y + topo[i]
		lines(x[i]+ws*A[,i],y2,lwd=lwd)	
	}
	if(add_ann && !is.null(ann) && length(ann)>0){
		posann <- x[test]
		ann <- ann[test]
		testann <- (ann != "")
		ann <- gsub("#","\n",ann)
		if(sum(testann)>0){
			abline(v=posann[testann],col="red",lwd=0.5)
			mtext(ann[testann], side = 3, line = 1.7, at=posann[testann], col="red",cex=0.9)
		}
	}
	
	
	axis(side=1,tck=-0.02)
	if(grepl("[s]$",depthunit)){
		abline(h=-time_0,col="red",lwd=0.5)
		depth <- (seq(0,by=2.5,max(abs(y))*v))
		depth2 <- seq(0.1,by=0.1,0.9)
		depthat <- depthToTime(depth, time_0, v, antsep)
		depthat2 <- depthToTime(depth2,time_0, v, antsep)
		axis(side=4,at=-depthat, labels=depth,tck=-0.02)
		axis(side=4,at=-depthat2, labels=FALSE,tck=-0.01)
		axis(side=4,at= -1* depthToTime(1, time_0, v, antsep), labels=FALSE,tck=-0.02)
		axis(side=2,at=pretty(y)-time_0,labels=-pretty(y),tck=-0.02)
		mtext(paste("depth (m),   v=",v,"m/ns",sep="") ,side=4, line=2)
	}else{
		abline(h=0,col="red",lwd=0.5)
		axis(side=2,at=pretty(y),labels=-pretty(y),tck=-0.02)
		axis(side=4,at=pretty(y),labels=-pretty(y),tck=-0.02)
	}
	box()
	if(!is.null(note)){
		 mtext(note, side = 1, line = 4, cex=0.6)
	}
	
	if(!is.null(pdfName)){
		dev.off()
	}
}



# col, main, xlab, ylab, mar, barscale
plotRaster <- function(A,x=NULL,y=NULL,plot_raster=TRUE,barscale=TRUE, add= FALSE, 
					mai=c(1, 0.8, 0.8, 1.8),  col=heat.colors(101),note=NULL,
					main="", time_0=0, antsep=1, v=0.1,ann=NULL,add_ann=TRUE,fid=NULL,depthunit="ns", ...){
	GPR =  as.matrix(A)
	GPR[is.na(GPR)]=0
	time_0 <- mean(time_0)
	zlim = range(GPR)
	if( length(list(...)) ){
		Lst <- list(...)
		if( !is.null(Lst$zlim)){
			zlim <- Lst$zlim
		}
	}
	if(grepl("[m]$",depthunit)){
		mai <- c(1, 0.8, 0.8, 0.5)
	}
	# cat("number of traces",ncol(GPR),"\n")
	reverse <- nrow(GPR) : 1
	GPR <- t(GPR[reverse,])

	if(is.null(x)){
		x <- (1:nrow(GPR))
	}	
	if(is.null(y)){
		y <- -(ncol(GPR):1)
	}
	if(add == TRUE){ 
		par(new = TRUE)
	}else{
		par( mai = mai,oma=c(0,0,3,0))
	}
	y <- y + time_0
	image(x,y,GPR,col=col,zlim=zlim,xaxs="i", yaxs="i", yaxt="n",...)	# matlab color
	title(main,outer=TRUE,line=1)
	usr <- par()$usr
	pin <- par()$pin	# inch
	dxin <- diff(usr[1:2])/(pin[1])
	dylim <- diff(usr[3:4])
	dusr <- dylim/length(y)
	pretty_y <- pretty(y)
	if(!is.null(fid) && length(fid)>0 && any(fid!="")){
		cin <- par()$cin[2]
		posfid <- x
		testfid <- (fid != "")
		ylim=range(y)
		yr <- diff(usr[3:4])/(pin[2])
		if(sum(testfid)>0){	
			par(xpd=TRUE)
			cst <- yr*cin
			points(posfid[testfid],cst/2*0.75+rep(ylim[2],sum(testfid)),pch=25,col="red",bg="yellow",cex=1)
			text(posfid[testfid],cst+rep(ylim[2],sum(testfid)),fid[testfid],cex=0.6)#,pos=3,offset =0)
			par(xpd=FALSE)
		}
	}
	if(add_ann && !is.null(ann) && length(ann)>0){
		posann <- x
		testann <- (ann != "")
		ann <- gsub("#","\n",ann)
		if(sum(testann)>0){
			abline(v=posann[testann],col="red",lwd=1)
			mtext(ann[testann], side = 3, line = 1.7, at=posann[testann], col="red",cex=0.9)
		}
	}
	axis(side=2, at=pretty_y + dusr/2, labels= -pretty_y)
	
	abline(h=0,col="red",lwd=0.5)
	if(grepl("[s]$",depthunit)){
		depth <- (seq(0,by=2.5,max(abs(y))*v))
		depth2 <- seq(0.1,by=0.1,0.9)
		depthat <- depthToTime(depth, 0, v, antsep)
		depthat2 <- depthToTime(depth2,0, v, antsep)
		axis(side=4,at=-depthat, labels=depth,tck=-0.02)
		axis(side=4,at=-depthat2, labels=FALSE,tck=-0.01)
		axis(side=4,at= -1* depthToTime(1, 0, v, antsep), labels=FALSE,tck=-0.02)
		mtext(paste("depth (m),   v=",v,"m/ns",sep="") ,side=4, line=2)
	}else{
		axis(side=4, at=pretty_y + dusr/2 , labels= -pretty_y)
	}	
	
	
	if(!is.null(note)){
		mtext(note, side = 1, line = 4, cex=0.6)
	}
	box()
	op <- par(no.readonly = TRUE)
	if(barscale && grepl("[s]$",depthunit)){
		fin <- par()$fin
		# par(new=TRUE)
		mai2 <- c(1, 0.8+pin[1]+1, 0.8, 0.6)
		par(mai=mai2)
		fin2 <- par()$fin
		wstrip <- fin2[1] - mai2[2] - mai2[4]
		xpos <- diff(usr[1:2])*(mai2[2] - mai[2])/pin[1]
		zstrip <- matrix(seq(zlim[1],zlim[2],length.out=length(col)),nrow=1)
		xstrip <- c( xpos,  xpos+wstrip*dxin)*c(0.9,1.1)
		ystrip <- seq(min(y),max(y),length.out=length(col))
		pretty_z <- pretty(as.vector(zstrip))
		dzlim <- zlim[2]-zlim[1] 
		pretty_at <- usr[3] - dylim * (zlim[1] - pretty_z)/dzlim
		axis(side=4,las=2, at=pretty_at, labels=pretty_z)
		image(xstrip,ystrip,zstrip,zlim=zlim,add=TRUE, col=col, axes=FALSE, xlab="", ylab="", xaxs="i", yaxs="i")
		# axis(side=4, las=2)
		box()
	}
	par(op)
}
#---



setGenericVerif("exportFID", function(x,filepath=NULL) standardGeneric("exportFID"))



#---------------------------------
# Defining the slot getters
setGenericVerif("gethd", function(x,hd=NULL) standardGeneric("gethd"))



setGenericVerif("filename", function(x) standardGeneric("filename"))


setGenericVerif("ann", function(x) standardGeneric("ann"))


setGenericVerif("ann<-",function(x,values){standardGeneric("ann<-")})



setGenericVerif("coord", function(x) standardGeneric("coord"))

setGenericVerif("coord<-",function(x,values){standardGeneric("coord<-")})

setGenericVerif("crs", function(x) standardGeneric("crs"))

setGenericVerif("crs<-",function(x,value){standardGeneric("crs<-")})


setGenericVerif("time0", function(x) standardGeneric("time0"))


setGenericVerif("time0<-",function(x,value){standardGeneric("time0<-")})


setGenericVerif("fid<-",function(x,values){standardGeneric("fid<-")})

setGenericVerif("fid", function(x) standardGeneric("fid"))


setGenericVerif("getData", function(x) standardGeneric("getData"))


setGenericVerif("setData<-", function(x,value) standardGeneric("setData<-"))


#==============================#
#========== DC-SHIFT =========#
setGenericVerif("dcshift", function(x, u) standardGeneric("dcshift"))


#==============================#
#========== FIRST BREAK =========#
setGenericVerif("firstBreack", function(x,nl=11, ns=NULL, bet=NULL) standardGeneric("firstBreack"))


# Jaun I. Sabbione and Danilo Velis (2010). Automatic first-breaks picking: 
# New strategies and algorithms. Geophysics, 75 (4): v67-v76
# -> modified Coppens's Method
# nl = length leading window: about one period of the firs-arrival waveform
# ns = length eps (edge preserving smoothing) window: good results with ns between one and two signal periods
#				-> default values ns= 1.5*nl
# bet = stabilisation constant, not critical, set to 0.2*max(amplitude) 
firstBreackPicking <- function(s, nl=11, ns=23, bet=0.2){
	# if( (nl%%2)==0) nl <- nl+1
	# if(is.null(ns)) ns <- round(1.5*nl)
	# if( ns%%2 == 0) ns<- ns+1
	# # H <- EMD::hilbert(x)
	# # A <- Mod(H)
	# A <- x^2
	# if(is.null(bet)) bet <- 0.2*max(A)
	
	E1 <- c(wapply(s,width=nl,by=1, FUN=sum),rep(0,2*floor(nl/2)))
	E2 <- cumsum(s)
	Er <- E1/(E2 + bet)
	Er_fil <- eps(Er,ns=ns)
	first_break <- which.max(abs(diff(Er_fil)))
	return(first_break)
}

# edge preserving smoothing
# luo et al. (2002): Edge preserving smoothing and applications: The Leading edge, 21: 136-158
eps <- function(x,ns){
	xmean <-  c(rep(0,floor(ns/2)), wapply(x,width=ns,by=1, FUN=mean),rep(0,floor(ns/2)))
	xsd <- c(rep(0,floor(ns/2)), wapply(x,width=ns,by=1,FUN=sd),rep(0,floor(ns/2)))
	xtest <- wapply(xsd,width=ns,by=1,FUN=which.min) + (0):(length(xmean)- 2*floor(ns/2)-1)
	return(c(rep(0,floor(ns/2)), xmean[xtest],rep(0,floor(ns/2))))
}

#==============================#
#========== SPATIAL FILTER =========#

setGenericVerif("adimproSmooth", function(x,hmax=2,...) standardGeneric("adimproSmooth"))

 

setGenericVerif("medianFilter", function(x) standardGeneric("medianFilter"))

.medianFilter <- function(A){
	B <- A	# <- matrix(0, 364,364)
	for(i in 1:(nrow(A)-2)) {
       for(j in 1:(ncol(A)-2) ) {
          xm <- A[i+0:2, j+0:2]
           	B[i+1, j+1] <- xm[order(xm)[5]]
		}
	}	
	return(B)
}

setGenericVerif("medianFilter1D", function(x,w) standardGeneric("medianFilter1D"))

.medianFilter1D <- function(a,w){
	b <- a	# <- matrix(0, 364,364)
	for(i in (w+1):(length(a)-w-1)){
		xm <- a[i+(-w:w)]
		b[i] <- xm[order(xm)[w+1]]
	}
	return(b)
}

setGenericVerif("hampelFilter", function(x, w=10,x0=0.1) standardGeneric("hampelFilter"))




#==============================#
#========== DEWOW =========#

setGenericVerif("dewow", function(x,type=c("MAD","Gaussian"),... ) standardGeneric("dewow"))

# deprecated !!
setGenericVerif("dewow2", function(x, sig=100) standardGeneric("dewow2"))


#==============================#
#======= GAIN FUNCTIONS ========#

setGenericVerif("gain", function(x, type=c("geospreading","exp","agc"),...) standardGeneric("gain"))



# CF Yilmaz, p85
gain_geospreading <- function(A,alpha,d_t,t_0=NULL,t_end=NULL,t_cst=NULL){
	g <- .gain_geospreading(A[,1],alpha,d_t,t_0,t_end,t_cst)
	Anew <- (A)*g
	# s1 = ((max(A))-(min(A)));	# scale factor
	# s1 = apply(A,2,max)-apply(A,2,min)	# scale factor
	# # s2 = ((quantile(Anew, 0.99))-(quantile(Anew, 0.01)))	# scale factor
	# s2 = apply(Anew,2,max)-apply(Anew,2,min)		# scale factor
	s1 = ((max(A))-(min(A)));	# scale factor
	# s2 = ((quantile(Anew, 0.99))-(quantile(Anew, 0.01)))	# scale factor
	s2 = ((max(Anew))-(min(Anew)));	# scale factor
	return(Anew/s2*s1 )
}

.gain_geospreading <- function(d,alpha,d_t,t_0=NULL,t_end=NULL,t_cst=NULL){
	if(is.null(t_0)) t_0 <-0
	if(is.null(t_end)) t_end <-(length(d)-1)*d_t
	if(!is.null(t_cst) && !(t_cst > t_0 && t_cst < t_end)){
		stop("you need t_cst > t_0 && t_cst < t_end\n")
	}
	x <- (seq_along(d) - 1) *d_t
	test <- x >= t_0 & x <= t_end
	g <- rep(1L,length(d))
	# g[test] <- ((seq_along(d[test])+1/d_t)*d_t + t_0)^alpha
	g[test] <- 1 + (seq_along(d[test])*d_t )^alpha
	g[x > t_end] <- max(g)
	if(!is.null(t_cst) && any(x < t_cst)) g[x < t_cst] <- g[1+round(t_cst/d_t)]
	return( g)
	# return( d_new)
}

gain_exp <- function(A,alpha,d_t,t_0=NULL,t_end=NULL){
	g <- .gain_exp(A[,1],alpha,d_t,t_0,t_end)
	Anew <- (A)*g
	# s1 = ((max(A))-(min(A)));	# scale factor
	# s1 = apply(A,2,max)-apply(A,2,min)	# scale factor
	# # s2 = ((quantile(Anew, 0.99))-(quantile(Anew, 0.01)))	# scale factor
	# s2 =  apply(Anew,2,quantile,0.999)-apply(Anew,2,quantile, 0.001)	# scale factor
	# # s2 = apply(Anew,2,max)-apply(Anew,2,min)		# scale factor
	s1 = ((max(A))-(min(A)));	# scale factor
	# s2 = ((quantile(Anew, 0.99))-(quantile(Anew, 0.01)))	# scale factor
	s2 = ((max(Anew))-(min(Anew)));	# scale factor
	
	s12 <- s1/s2
	A3 <- (Anew * s12)
	return(	Anew)
}

.gain_exp <- function(d,alpha,d_t,t_0=NULL,t_end=NULL){
	if(is.null(t_0) || t_0==0) t_0 <-0
	if(is.null(t_end)) t_end <-(length(d)-1)*d_t
	# tx <- seq(0,t_end-t_0,by=d_t)+1/d_t
	x <- (seq_along(d) - 1) * d_t
	test <- (x >= t_0 & x <= t_end)
	test_max <- x > t_end
	g <- rep(1L,length(d))
	g[test] <-  exp((x[test] - t_0) * d_t * alpha)
	g[test_max] <-  exp(max(x[test] - t_0)*d_t*alpha)
	return( g)
}


gain_agc <- function(A,d_t,sig=10,p=2,r=0.5){
	sig <- sig/d_t
	Anew <- apply(A,2,.gain_agc,sig,p,r)
	s1 = ((max(A))-(min(A)));	# scale factor
	# s2 = ((quantile(Anew, 0.99))-(quantile(Anew, 0.01)))	# scale factor
	s2 = ((max(Anew))-(min(Anew)));	# scale factor
	return(Anew * s1/s2)
}

.gain_agc <- function(d,sig=10,p=2,r=0.5,plot=FALSE,...){
	# convert NA into 0
	d[is.na(d)] <- 0
	# Get local mean by smoothing the image with a Gaussian filter
	dAmp 	<- gaussianSmooth(d, sig)
	# Subtract image from local mean, raise to power 'p' then apply Gaussian
	# smoothing filter to obtain a local weighted sum. 
	# Finally raise the result
	# to power 'r' to obtain the 'gain'.  Typically p = 2 and r = 0.5 which will
	# make gain equal to the local RMS.  The abs() function is used to allow
	# for arbitrary 'p' and 'r'.
	dGain <- (gaussianSmooth(abs(d-dAmp)^p, sig))^r
	# Apply inverse gain to the difference between the image and the local
	# mean to obtain the final AGC image. 
	dnew <- d/dGain
	return(dnew)
}
setGenericVerif("plotAmpl", function(x, FUN=mean, add=FALSE, ylim=NULL,xlim=NULL,col=1,all=FALSE,...) standardGeneric("plotAmpl"))


setGenericVerif("getAmpl", function(x, FUN=mean, ...) standardGeneric("getAmpl"))



#=============================================#
#======== CLIP/GAMMA/NORMALIZE ================#

setGenericVerif("clip", function(x, Amax=NULL,Amin=NULL) standardGeneric("clip"))


setGenericVerif("gammaCorrection", function(x, a=1,b=1) standardGeneric("gammaCorrection"))



.gammaCorrection <- function(A,a,b){
	return(a*sign(A)*abs(A)^b)
}

.clip <- function(A,Amax=NULL,Amin=NULL){
        if(!is.null(Amin)){
                A[(A)< Amin] <- Amin
        }
        if(!is.null(Amax)){
                A[(A) > Amax] <- Amax
        }
		if(!is.null(Amax) && is.null(Amin)){
			A[(A)< -Amax] <- -Amax
		}
        return(A)
}


normalize <- function(A,type = c("stat","min-max","95","eq","sum")){
	A =  as.matrix(A)
	type = match.arg(type)
	if(type == "stat"){
		Anorm <- scale(A, center=.colMeans(A, nrow(A), ncol(A)), scale=apply(A, 2, sd, na.rm = TRUE))
	}else if(type == "sum"){
		Anorm <- scale(A, center=FALSE, scale=colSums(abs(A)))
	}else if(type == "eq"){
		# equalize: equalize line such each trace has same value for sqrt(\int (A(t))^2 dt)
		amp <- apply((A)^2,2,sum)
		Anorm <- A * sqrt(amp)/sum(sqrt(amp))
	}else if(type == "95"){
		A_q95 = (apply((A),2,quantile,0.99,na.rm=TRUE))
		A_q05 = (apply((A),2,quantile,0.01,na.rm=TRUE))
		Anorm = (A)/(A_q95-A_q05)
	}else{	# min-max
		Anorm <- scale(A, center=FALSE, scale=(apply((A),2,max,na.rm=TRUE)) - (apply((A),2,min,na.rm=TRUE)))
	}
	return(Anorm)
}



#=============================================#
#======== SPECTRAL FUNCTIONS ================#
setGenericVerif("spec", function(x, type=c("f-x","f-k"), return_spec=FALSE,plot_spec=TRUE, ...) standardGeneric("spec"))

setGenericVerif("freqFilter", function(x, f=100, type=c('low','high','bandpass'),L=257,plot_spec=FALSE) standardGeneric("freqFilter"))

setGenericVerif("fkFilter", function(x, fk=NULL, L=c(5,5),npad=1) standardGeneric("fkFilter"))


# @param [matrix]/[vector] 	A 		(each column represent a trace / a trace)
# @param [double] 			T 		(sampling time in nanoseconde)	
# @param [double]			fac		(result multiplied by a factor, e.g. to get MHz instead of Hz)

# @return power spectrum (frequency, power, phase)
# -------------------------------------------
powSpec <- function(A,T = 0.8, fac = 1000000, plot_spec=TRUE, return_spec=FALSE,title_spec=NULL){
	A = as.matrix(A)
	nr = nrow(A)
	nc = ncol(A)
	N = 2^(ceiling(log2(nr)))
	A = rbind(A,matrix(0,nrow=N-nr,ncol=nc))
	
	# if y <- fft(z), then z is 
	# fft(y, inverse = TRUE) / length(y).
	# By contrast, mvfft takes a real or complex matrix as argument,
	# and returns a similar shaped matrix, but with each column 
	# replaced by its discrete Fourier transform. 
	fft_A = mvfft(A)
	# extract the power spectrum (power is sometimes referred to as "magnitude")
	# power=sqrt(Re(fourier)*Re(fourier)+Im(fourier)*Im(fourier))
	pow = as.matrix(Mod(fft_A))
	pow = as.matrix(Mod(fft_A))
	# extract the phase which is atan(Im(fourier)/Re(fourier))
	pha = as.matrix(Arg(fft_A))
	# si matrix -> moyenne sur les colonnes
	
	nfreq <- N/2 + 1
	# select only first half of vectors
	pha = pha[1:nfreq,,drop=FALSE] 
	# select only first half of vectors
	pow = pow[1:nfreq,,drop=FALSE] 
	pow_mean = apply(pow,1, mean, na.rm=TRUE)
	unwrap_pha <- apply(pha,2, unwrap)
	pha_mean = apply(unwrap_pha,1, mean, na.rm=TRUE)
	
	# samping interval GPR = 0.8 ns
	Ts = T*(10^(-9))		# [s] Sample time
	Fs = 1/Ts			# [Hz] Sampling frequency
	Fc = 1/(2*Ts)		# Nyquist frequency
 
	# frequence
	# the Nyquist frequency is fc = 1/(2*d), where d is the sampling
	# interval in units of time
	# fre = (0:(length(pow_mean)-1))/(2*length(pow_mean) * Ts)/fac	#[MHz]
	fre = Fs*seq(0,N/2)/N/fac
	# plot the power spectrum
	if(plot_spec){
		m = seq(0,10000,by=50)
		par(mfrow=c(2,1))
		par(mar=c(0, 4, 4, 2) + 0.1, oma=c(1,1,1,1) )
		plot(fre,pow_mean, type="n",xaxt = "n",ylim=c(0,max(pow)),
					ylab="amplitude",xlab="")
			if(!is.null(dim(A))){
				nothing <- apply(pow,2,lines,x=fre, col=rgb(0.2,0.2,0.2,7/max(ncol(A),7)))
			}
			lines(fre,pow_mean,col="red")
			Axis(side = 1, tcl = +0.3,  labels=FALSE ,at=m)
			if(!is.null(title_spec)){
				title(title_spec)
			}
		par(mar=c(4, 4, 0.3, 2))
		plot(fre,pha_mean, type="n",xaxt = "n",ylim=range(unwrap_pha), xlab = "frequency MHz", ylab="phase") 
			if(!is.null(dim(A))){
				nothing <- apply(unwrap_pha,2,lines,x=fre,col=rgb(0.2,0.2,0.2,7/max(ncol(A),7)))
			}
			lines(fre,pha_mean,col="red")
			Axis(side = 1, tcl = +0.3,  labels=m ,at=m)
	}
	if(return_spec){
		return(list(freq = fre, pow = pow, pha = pha))
	}
}

# @param [matrix]/[vector] 	A 		(each column represent a trace / a trace)
# @param [double] 			T 		(sampling time in nanoseconde)	


# @return power spectrum (frequency, power, phase)
# -------------------------------------------

freqFilter1D <- function(A, f=c(100),  type = c('low','high','bandpass'),L=257, T = 0.8, plot_spec = FALSE){
	
	type = match.arg(type)
	A <- as.matrix(A)
	M = nrow(A)			# signal length
	# samping interval GPR = 0.8 ns
	Ts = T*10^(-9)		# [s] Sample time
	Fs = 1/Ts			# [Hz] Sampling frequency
	# cut-off frequency/ies fc in (MHz)
	f = sort(f) * 10^6		# cut-off frequency in Hz
	
	if(type == "low" || type == "high"){
		# Design the filter using the window method:
		if(length(f)>1) {
				BW = (f[2] - f[1])/Fs	# bandwidth of the filter
				fc = f[1] + (f[2] - f[1])/2
				L = 4 / BW
				L = round(L)
				if(L %% 2 == 0) L = L + 1	
				# cat(paste("filter length ",L,"\n"))

		}else if(length(f)==1){
			fc = f[1]
		}
		h <- winSincKernel(L,fc/Fs,type)
	}else if(type == "bandpass"){
		if(length(f)==2 ) {
			h1 <- winSincKernel(L,f[1]/Fs,"low")
			h2 <- winSincKernel(L,f[2]/Fs,"high")
		}else if(length(f)==4 ){
			BW = (f[2] - f[1])/Fs	# bandwidth of the filter
			fc = f[1] + (f[2] - f[1])/2
			L = 4 / BW
			L = round(L)
			if(L %% 2 == 0) L = L + 1
			# cat(paste("filter length ",L,"\n"))
			h1 <- winSincKernel(L,fc/Fs,"low")
			BW = (f[4] - f[3])/Fs	# bandwidth of the filter
			fc = f[3] + (f[4] - f[3])/2
			L = 4 / BW
			L = round(L)
			if(L %% 2 == 0) L = L + 1
			# cat(paste("filter length ",L,"\n"))
			h2 <- winSincKernel(L,fc/Fs,"high")
		}
		L = max(length(h1),length(h2))
		# if(L %% 2 == 0) L = L + 1
		cat("lenght max",L,"\n")
		if(length(h2) < L ){
			h2 = c(rep(0,(L-length(h2))/2),h2,rep(0,(L-length(h2))/2))
		}
		if(length(h1) < L ){
			h1 = c(rep(0,(L-length(h1))/2),h1,rep(0,(L-length(h1))/2))
		}
		
		# change the band-reject filter kernel into a band-pass 
		h = -h1 - h2
		h[(L+1)/2] = h[(L+1)/2] + 1
	}

	# Choose the next power of 2 greater than L+M-1 
	Nfft = 2^(ceiling(log2(L+M-1)))	# -1)))		# or 2^nextpow2(L+M-1)
	# Zero pad the signal and impulse response:
	h_long = c( h, rep(0,Nfft-L) )
	A = rbind(as.matrix(A) , matrix(0,nrow=Nfft-M,ncol=ncol(A)) )

	fft_A = mvfft(A)		# signal
	fft_h = fft(h_long)				# filter

	#Now we perform cyclic convolution in the time domain using pointwise multiplication in the frequency domain:
	Y = fft_A * fft_h
	if(type == "bandpass"){
		
	}
	
	pow_A = Mod(fft_A)
	pow_h = Mod(fft_h)
	pow_y = Mod(Y)
	# si matrix -> moyenne sur les colonnes
	if(!is.null(dim(A))){
		pow_A = apply(pow_A,1, mean, na.rm=T)
		pow_y = apply(pow_y,1, mean, na.rm=T)
	}
	# select only first half of vectors
	pow_A = pow_A[1:(Nfft/2+1)] 
	pow_y = pow_y[1:(Nfft/2+1)] 
	pow_h = pow_h[1:(Nfft/2+1)] 
	
	
	# fre = 1:length(pow_A)/(2*length(pow_A) * T)/1000000	#[MHz]
	fre = Fs*(0:(Nfft/2))/Nfft/1000000	#[MHz]
	
	if(plot_spec == TRUE){
		# plot the power spectrum
		m = seq(0,900,by=50)
		#par(mfrow=c(2,1), mar=c(5, 4, 4, 6) + 0.1 )
		par( mar=c(0, 4, 0.3, 2) + 0.1, oma=c(3,2,1,2) )
		plot(fre,pow_A, type="l",xaxt = "n",
				#	yaxt = "n", 
					ylim=c(0,max(pow_A,pow_y)),
					ylab="power",lwd=2)
			lines(fre,pow_y, type="l",col="blue",lwd=2)
			Axis(side = 1, tcl = +0.3,  labels=m ,at=m)
			par(new=TRUE)
			plot(fre,pow_h,type="l", col="red",
				yaxt = "n",
				ylab="")
				legend("topright",c("input signal","filter","filtered signal"),col = c("black", "red", "blue"), lwd=c(2,1,2),bg = "white")
			abline(v=f/1000000, col="grey",lty=2)
			#title("Power spectrum")
			#title("Power spectrum")
	}
	a = (L-1)/2
	y = mvfft(Y, inverse = TRUE)
	y = y[a:(a+M-1),]/nrow(y)
	return(Re(y))
	#list(freq = fre, pow = pow, pha = pha)
}

winSincKernel <- function(L,f,type=c("low","high")){
	type = match.arg(type)			# if L is even (because L - filter length - must be odd)
	x = (-(L-1)/2):((L-1)/2)
	# low-pass
	h = hammingWindow(L) * sincMod(x,2*pi*f)	# h is our filter
	h = h/sum(h)

	# high-pass
	if(type == "high"){
		h = -h
		h[(L+1)/2] = h[(L+1)/2] + 1
	}
	return(h)
}

sincMod <- function(x,ff){
	r = length(x)
	n0 = which(x == 0)
	v = rep(0,r)
	ww <- c(1:(n0-1),(n0+1):r)
	#x = x[c(1:(n0-1),(n0+1):r)]
	v[ww] = sin(ff*x[ww])/(x[ww])
	v[n0] = ff
	return(v)
}
hammingWindow <- function(L){
	N = L-1
	n <- 0:N
	return(0.54 - 0.46*cos(2*pi*n/N))
}

# Choose the next power of 2 greater than L+M-1 
	# Nfft = 2^(ceiling(log2(L+M-1)))	# -1)))		# or 2^nextpow2(L+M-1)
nextpower2 <- function(x){
	return(2^(ceiling(log2(x))))
}

# shift the phase of signal by phi (in radian)
phaseRotation <- function(x,phi){
	nf <- length(x)
	X = fft(x)
	phi2 <- numeric(nf)
	phi2[2:(nf/2)] <- phi
	phi2[(nf/2+1):(nf)] <- -phi
	Phase = exp(-complex(imaginary=-1)*phi2)
	xcor = fft(X*Phase, inverse=TRUE)/nf
	return(Re(xcor))
}

# # readGPR = read DT1 FORMAT and return object from class "GPR"
# readGPR <- function(filename,description=""){
		# A <- readDT1(filename)
		# name=strsplit(basename(filename),'[.]')[[1]][1]
		# return(GPR(A,name=name,filename=filename,description=description))
# }

setGeneric("readGPR", function(filename,description="", coordfile=NULL,crs="",intfile=NULL) standardGeneric("readGPR"))


# setGenericVerif("writeGPR", function(x,filename, format=c("DT1","rds")) standardGeneric("writeGPR"))

setGenericVerif("name", function(x) standardGeneric("name"))

setGenericVerif("description", function(x) standardGeneric("description"))


# #----------------------------------
# setGenericVerif("setdata<-",function(object,value){standardGeneric("setdata<-")})
# setReplaceMethod(
	# f="setdata",
	# signature="GPR",
	# definition=function(object,value){
	# object@data <- as.matrix(value)
	# object@ntr <- ncol(object@data)
	# object@hd$endpos <- (ncol(object@data)-1)*object@dx
	# object@hd$startpos <- 0
	# return(object)
	# }
# )
#---------------------------------------


# -------------------------------------------
# ------------addProfile3D--------------------------
# -------------------------------------------
# @name	addProfile3D (plot/add a 2D profile in rgl)
# @description read one or several DT1 file and plot/add 
# the 2D profiles in a rgl plot

# # @date 14.10.2013 15:15
# # @auteur Emanuel Huber
# require(rgl)
# # @require function load_install_package()
# source('load_install_package.R')
# cat('> Function(s) loaded: "load_install_package.R" ')
# # @require function normalizeGPR
# source('GPR_normalize.R')
# source('GPR_readDT1.R')
# source('GPR_gain.R')
# cat('"GPR_normalize.R"')
# cat('"GPR_readDT1.R" \n')
# cat('"GPR_gain.R" \n')

# @param [list] 	LINE 			(list containing several filepath of the DT1 file)
# @param [c(1)] 	col=NULL 	(palette of color)	
# @param [boolean] 	plotNew=FALSE	(if true, open a new rgl window)
# @return void
# -------------------------------------------



# plot/add a 2D profile in rgl

addProfile3D <- function(LINES, col=diverge_hcl(101, h = c(246, 10), c = 120, l = c(30, 90)),plotNew=FALSE, normalize=TRUE, v=1, zlim=NULL, AGC=FALSE, sig=10){
	if(plotNew){
		# rgl.open()
		open3d()
	}
	for(i in seq_along(LINES)){
		#------------- READ DATA ------------------#
		lineName2 	<- strsplit(LINES,split="[.]")
		lineName 	<- lineName2[[i]][1]
		fileNameHD 	<- paste(lineName,".HD",sep="")
		fileNameDT1 <- paste(lineName,".DT1",sep="")
		cat(basename(lineName),"\n")
		GPR <- readDT1(LINES[[i]])
		#------------- read data ------------------#

		myGPRdZ <- as.numeric(as.character(GPR$hd[7,2]))/as.numeric(as.character(GPR$hd[5,2]))
		HD <- GPR$dt1hd
		
		A <- GPR$data
		A[is.na(A)] <- 0
		if(!is.null(zlim)){
			sel <- seq(1, zlim/myGPRdZ/v,by=myGPRdZ)
			A <- A[sel,]
		}
		if(normalize){
			A <- normalizeGPR(A)
		}
		if(AGC){
			A <- apply(A,2,gain,sig=sig)
		}
		 # example with GPR profile A 
		nr = nrow(A)
		nc = ncol(A)
		X <- matrix(HD$recx, ncol=nc, nrow=nr, byrow=TRUE)
		Y <- matrix(HD$recy, ncol=nc, nrow=nr, byrow=TRUE)
		Z <-  matrix(HD$topo, ncol=nc, nrow=nr, byrow=TRUE) - matrix(myGPRdZ*v*(0:(nr-1)), ncol=nc, nrow=nr, byrow=FALSE)
		if(all(HD$topo==0)){
			warning("No topography \n")
		}
		if(all(HD$recx==0)){
			warning("No x-coordinates \n")
		}
		if(all(HD$recy==0)){
			warning("No y-coordinates \n")
		}
		A = (A-min(A))/(max(A)-min(A))
		Alim <- range(A)
		Alen <- Alim[2] - Alim[1] + 1
		
		# if(is.null(col)) 		col <- tim.colors(101)	# height color lookup table

		colA <- col[ (A)*100+1 ] # assign colors to heights for each point 
		rgl.surface(X, Y, Z, color=colA, back="fill", smooth = TRUE, lit=FALSE, lwd=0) 
		# surface3d(X, Y, Z, color=colA, back="fill", smooth = FALSE, lit=FALSE, lwd=0) 
	}
}




#-------------------------------------
#------- PRIVAT FUNCTION --------#
byte2volt <- function ( V=c(-50,50), nBytes = 16) {
	abs(diff(V))/(2^nBytes)
}




minCommon10 <- function(xmin,xmax){
	xmin <- as.numeric(xmin)
	xmax <- as.numeric(xmax)
	D <- xmax-xmin
	n <- nchar(D)
	if( as.numeric(substr(xmin,nchar(xmin)-n+1,nchar(xmin))) + D < 10^(n)){
		return(as.numeric(substr(xmin,1,n+1))*10^(nchar(xmin)-n-1))
	}else{
		return(xmin)
	}
}



 
.upsample <- function(A, n=2, type=c("DFT","bicubic")){
	# bi cubic---
	# library(fields)
	# interp2d <- function(old, newx, newy) {
	  # interp.surface.grid(list(x=seq(nrow(old)),y=seq(ncol(old)),z=old),
						  # list(x=seq(1,nrow(old),length=newx),
							   # y=seq(1,ncol(old),length=newy)))$z
	# }
	# A_bi <- interp2d(A,nrow(A)*2,ncol(A)*2)
	# or akima::bicubic.grid(x,y,z,xlim,ylim,dx,dy)
	if(is.matrix(A)){
		nr <- nrow(A)	# time	
		nc <- ncol(A)	# x	

		nk <- (nextpower2(nc))
		nf <- (nextpower2(nr))
		A1 <- matrix(0,nrow=nf,ncol=nk)
		A1[1:nr,1:nc] <- A
		A1_fft <- fft(A1)
		
		A_fftint <- matrix(0,nrow=n*nf,ncol=n*nk)
		A_fftint[1:(nf/2),1:(nk/2)] <- A1_fft[1:(nf/2),1:(nk/2)]
		A_fftint[((n-1)*nf + nf/2+1):(n*nf),((n-1)*nk + nk/2 + 1):(n*nk)] <- A1_fft[(nf/2+1):(nf),(nk/2 + 1):nk]
		A_fftint[1:(nf/2),((n-1)*nk + nk/2 + 1):(n*nk)]<- A1_fft[1:(nf/2),(nk/2 + 1):nk]
		A_fftint[((n-1)*nf + nf/2+1):(n*nf),1:(nk/2)] <- A1_fft[(nf/2+1):(nf),1:(nk/2)]
		
		A_int = fft(A_fftint, inverse = TRUE)
		A_int <- A_int[1:(n*nr),1:(n*nc)]/(nk*nf)
	}else if(is.vector(A)){
		# FTA = fft(A);
		n_A = length(A)
		# Choose the next power of 2 greater than L+M-1 
		Nfft = 2^(ceiling(log2(n_A)))		# 
		# Zero pad the signal and impulse response:
		A0 = c( A, rep(0,Nfft-n_A) )
		n_A0 <- length(A0)
		
		FTA <- fft(A0)
		
		# % now insert enough zeros into the dft to match the desired density 'n'
		FTA = c(FTA[1:(n_A0/2)], rep.int(0,floor((n-1)*n_A0)), FTA[(n_A0/2+1):n_A0])

		A_int = fft(FTA, inverse = TRUE)
		A_int <- A_int[1:(n_A * (n))]/n_A0
	}
	return(Re(A_int))
}


#---------------- CONVOLUTION --------------------#

# linear convolution with fft
convolution <- function(a,b){
	na <- length(a)
	nb <- length(b)
	L <- na + nb - 1
	a0 <- c(a,rep(0,nb-1))
	b0 <- c(b, rep(0,na-1))
	y <- Re(fft(fft(a0)*fft(b0),inverse=TRUE))/L
	return(y[1:(max(na,nb))])
}

# cf. matlab
# A convolution matrix is a matrix, formed from a vector, whose product with another vector 
# is the convolution of the two vectors.

# A = convmtx(y,nf) returns the convolution matrix, A, such that the product of A and a vector, x, 
# is the convolution of y and x. 
# If y is a column vector of length m, A is (m+nf-1)-by-nf and the 
# product of A and a column vector, x, of length n is the convolution of y and x. 
convmtx <- function(y, nf){
	ny <- length(y)
	L <- nf + ny -1
	# convolution matrix Y
	yext <- rep(c(y,rep(0,L-ny+1)),nf)
	yext <- yext[1:(L*nf)]
	return( matrix(yext,nrow=L,ncol=nf))
}


#---------------- DECONVOLUTION --------------------#
# spectral deconvolution with known wavelet
# convolution model: y = h*x 
# h and y are known, x is unknown
# x ~ H^h * Y / (H^h * H + mu)
deconvFreq <- function(y,h,mu=0.0001){
	ny <- length(y)
	nh <- length(h)
	L  <- ny + ny - 1
	H  <- fft(c(h,rep(0,ny-1)))
	Y  <- fft(c(y, rep(0,nh-1)))
	Re(fft( t(Conj(H))*Y/(t(Conj(H))*H + mu) ,inverse=TRUE))[1:ny]/L
	# Re(fft( Y/(H + mu) ,inverse=TRUE))[1:ny]/L
}


# TO CHECK!!!!
# deconvolution with known wavelet
# convolution model: y = h*x 
# h and y are known, x is unknown
# x ~ H^h * Y / (H^h * H + mu)
deconv <- function(y,h,nf,mu=0.0001){
	# ny <- length(y)
	# nh <- length(h)
	# L  <- ny + ny - 1
	H  <- convmtx(h,nf)
	y_acf <- as.numeric(acf(y,lag=nf-1,plot=FALSE)[[1]])
	y_acf[1] <- y_acf[1] + mu
	HtH <- toeplitz(y_acf)
	x <-  solve(HtH) %*% (t(H[1:nf,1:nf]) %*% y)
	return(x)
}

.RMS <- function(num) sqrt(sum(num^2)/length(num))

setGenericVerif("rmsScaling", function(x) standardGeneric("rmsScaling"))



setGenericVerif("deconvSpiking", function(x, W,wtr,nf,mu) standardGeneric("deconvSpiking"))


setGenericVerif("rotatePhase", function(x, phi) standardGeneric("rotatePhase"))



optPhaseRotation <- function(gpr,rot=0.01,plot=TRUE){
	# x_dec <- as.vector(gpr/apply(as.matrix(gpr),2,RMS))
	x_dec <- as.vector(gpr)
	pi_seq <- seq(0,pi,by=rot)
	kurt <- numeric(length(pi_seq))
	nx <- length(x_dec)
	for(i in seq_along(pi_seq)){
		xrot <- phaseRotation(x_dec, pi_seq[i])
		# xrot_scaled2 <- (xrot - 	mean(xrot))^2
		# kurt[i] <- ((1/nx) * sum( xrot_scaled2^2)) / ( (1/nx) *sum( xrot_scaled2))^2 
		kurt[i] <- kurtosis( xrot)
	}
	phi_max <- pi_seq[which.max(kurt)]
	cat("rotation angle =",phi_max/pi*180, "°\n",sep="")
	# dev.off(); windows()
	if(plot==TRUE){
		plot(pi_seq/pi*180,kurt,type="l")
		abline(v=phi_max/pi*180,col="red")
	}
	return(phi_max)
	# x_dec <- phaseRotation(x_dec, phi_max)
}

# y is the wavelet and we want 
# a filter f s.t. f*y = d 
# with d = [0 ... 0 1 0 ...0]
# 1 at the postion i = shft
# if shft = NULL, the shift is chosen by the
# 		algorithm and also returned
# if shift is not NULL, case of wavelet estimation
# 		from the trace via the autocorrelation matrix.
# mu = percent of pre-whitening
spikingFilter <- function(y,nf=32,mu=0.1,shft=1){
	# R = t(Y)%*%Y = Toepliz matrix of ACF
	y_acf <- as.numeric(acf(y,lag=nf-1,plot=FALSE)[[1]])
	taper <- hammingWindow(2*nf)
	y_acf <- y_acf*taper[(nf+1):(2*nf)] 
	y_acf[1] <- y_acf[1] + mu
	YtY <- toeplitz(y_acf)
	# all the spiking filters
	if(is.null(shft)){
		ny <- length(y)
		L <- nf + ny -1
		# convolution matrix Y
		Y <- convmtx(y,nf)
		H <- solve(YtY) %*% t(Y) 
		v <- numeric(L)
		# performance matrix: all spiking filter outputs
		P <- Y %*% H
		# optimal delay (smallest error)
		i <- which.max(diag(P))
		v[i] <- 1
		h <- H%*%v
		return(list("h"=h,"delay"=i))
	}else{
		v <- numeric(nf)
		v[shft] <- 1
		h <- solve(YtY) %*% v 
		return(h)
	}
}


# return a matrix with columns that are the auto-correlation
# of the column data.
# lag.max =...
# plot=FALSE...
acfmtx <- function(Y, ...){
	myACF <- apply(ym, 2, acf,...)
	myACF2 <- do.call(cbind,lapply(myACF,function(x)x$acf))
	return(myACF2)
}


setGenericVerif("traceShift", function(x, fb,kip=10) standardGeneric("traceShift"))


ar_fb <- function(y, nf, mu=0.1, type=1){
	if(type==-1){
		ny <- length(y)
		H <- convmtx(y,nf)[,nf:1]
		d <- numeric(nf+ny-1)
		d[(nf+1):(ny+nf-1)] <- y[1:(ny-1)]
		f <- solve(t(H)%*%H + mu*diag(nf)) %*% t(H) %*% d
		y_pred <- numeric(ny)
		y_pred[1:(ny-1)] <- (H%*%f)[(nf+1):(nf+ny-1)]
	# forward prediction
	}else if(type==1){
		ny <- length(y)
		H <- convmtx(y,nf)
		d <- numeric(nf+ny-1)
		d[1:(ny-1)] <- y[2:ny]
		f <- solve(t(H)%*%H + mu*diag(nf)) %*% t(H) %*% d
		y_pred <- numeric(ny)
		y_pred[2:ny] <- (H%*%f)[1:(ny-1)]
	}
	return(y_pred)
}


fx_deconv <- function(Y, nf, mu=0.1, flow=NULL,fhigh=NULL, dz, type=1){
	npts <- nrow(Y)
	npos <- ncol(Y)
	nfft <- nextpower2(npts)
	Y0 <- matrix(0, nrow=nfft,ncol=npos)
	FX_pred <- Y0
	FX_pred_b <- Y0
	Y0[1:npts,1:npos] <- Y
	FX <- mvfft(Y0)
	
	if(is.null(flow)){
		ilow <- 1
	}else{
		ilow <- floor(flow*dz*nfft)+1
		ilow <- ifelse(ilow <1, 1, ilow)
	}
	if(is.null(fhigh)){
		ihigh <- floor(nfft/2) +1
	}else{
		ihigh <- floor(fhigh*dz*nfft)+1
		ihigh <- ifelse(ihigh > floor(nfft/2) +1, floor(nfft/2) +1, ihigh)
	}
	# FX_pred <- matrix(0,nrow=nrow(FX),ncol=ncol(FX))
	for(k in ilow:ihigh){
		FX_pred[k,] <- ar_fb(FX[k,], nf=nf, mu=mu, type=1)
		FX_pred_b[k,] <- ar_fb(FX[k,], nf=nf, mu=mu, type=-1)
	}
	# Honor symmetries
	for(k in (nfft/2 + 2):nfft){
		FX_pred[k,] <- Conj(FX_pred[nfft-k +2,])
		FX_pred_b[k,] <- Conj(FX_pred_b[nfft-k +2,])
	}
	
	Y_pred_f <- Re(mvfft(FX_pred, inverse=TRUE))/nfft
	Y_pred_b <- Re(mvfft(FX_pred_b, inverse=TRUE))/nfft
	Y_pred <- Y_pred_f[1:npts,] + Y_pred_b[1:npts,]
	Y_pred[,(nf+1):(npos-nf)] <- Y_pred[,(nf+1):(npos-nf)]/2 
	return(Y_pred)
}

# function DATA_f = fx_decon(DATA,dt,lf,mu,flow,fhigh)
# %
# %FX_DECON: SNR enhancement using fx-deconvolution.
# %
# %  [DATA_f] = fx_decon(DATA,dt,lf,mu,flow,fhigh);
# % 
# %  IN   DATA   :  the data matrix, columns are traces
# %       dt     :  sampling interval in sec
# %       lf     :  length of operator (length of the filter)
# %       mu     :  pre-whitening 
# %       flow   :  min  freq. in the data in Hz
# %       fhigh  :  max  freq. in the data in Hz
# % 
# %  OUT  DATA_f : filtered data 
# %
# % %  Reference: Canales, 1984, Random noise reduction, 54.th. Ann. Internat. 
# %             Mtg., Soc. Expl. Geophys., Expanded Abstracts, pp. 525-527
# %
# %  Note: Canales method is modified to use non-Toeplitz system of equations
# %        with backward and foward prediction filters
# %       
# %  Example: see fx_decon_demo.m
# %
# %  Copyright (C) 2008, Signal Analysis and Imaging Group
# %  For more information: http://www-geo.phys.ualberta.ca/saig/SeismicLab
# %  Author: M.D.Sacchi
# %
# %  This program is free software: you can redistribute it and/or modify
# %  it under the terms of the GNU General Public License as published
# %  by the Free Software Foundation, either version 3 of the License, or
# %  any later version.
# %
# %  This program is distributed in the hope that it will be useful,
# %  but WITHOUT ANY WARRANTY; without even the implied warranty of
# %  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# %  GNU General Public License for more details: http://www.gnu.org/licenses/
# %
# %

# [nt,ntraces] = size(DATA);
# nf           = 2^nextpow2(nt);
# DATA_FX_f    = zeros(nf,ntraces);
# DATA_FX_b    = zeros(nf,ntraces);

# % First and last samples of the DFT.
# ilow  = floor(flow*dt*nf)+1;
# if ilow<1;
    # ilow=1;
# end;
# ihigh = floor(fhigh*dt*nf)+1;

# if ihigh > floor(nf/2)+1;
    # ihigh=floor(nf/2)+1;
# end

# % Transform to FX
# wh = waitbar(0,'Processing... Please wait!');
# DATA_FX = fft(DATA,nf,1);
# for k = ilow:ihigh;
    # aux_in  = DATA_FX(k,:)';
    # [aux_out_f,aux_out_b] = ar_modeling(aux_in,lf,mu);
    # DATA_FX_f(k,:) = aux_out_f';
    # DATA_FX_b(k,:) = aux_out_b';
    # waitbar(k/(ihigh-ilow),wh);
# end;
# close(wh);

# % Honor symmetries
# for k=nf/2+2:nf
    # DATA_FX_f(k,:) = conj(DATA_FX_f(nf-k+2,:));
    # DATA_FX_b(k,:) = conj(DATA_FX_b(nf-k+2,:));
# end

# % Back to TX (the output)
# DATA_f = real(ifft(DATA_FX_f,[],1));
# DATA_f = DATA_f(1:nt,:);
# DATA_b = real(ifft(DATA_FX_b,[],1));
# DATA_b = DATA_b(1:nt,:);

# % Average predictions (forward and backward)
# DATA_f = (DATA_f + DATA_b);
# DATA_f(:,lf+1:ntraces-lf)= DATA_f(:,lf+1:ntraces-lf)/2;

# return

# function [yf,yb] = ar_modeling(x,lf,mu)
# %
# %AR_MODELING: autoregressive modeling of 1D spatial data
# %
# %  IN    x:   data 
# %        lf:  length of the operator
# %        mu:  pre-whitening in %
# %      
# %  OUT   yf:  prediction of the data using forward AR modeling
# %        yb:  prediction of the data using backward AR modeling
# % 
# %  Copyright (C) 2008, Signal Analysis and Imaging Group
# %  For more information: http://www-geo.phys.ualberta.ca/saig/SeismicLab
# %  Author: M.D.Sacchi
# %
# %  This program is free software: you can redistribute it and/or modify
# %  it under the terms of the GNU General Public License as published
# %  by the Free Software Foundation, either version 3 of the License, or
# %  any later version.
# %
# %  This program is distributed in the hope that it will be useful,
# %  but WITHOUT ANY WARRANTY; without even the implied warranty of
# %  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# %  GNU General Public License for more details: http://www.gnu.org/licenses/
# %

# nx = length(x);

# % backward ar-modeling
# y  = x(1:nx-lf,1);
# C  = x(2:nx-lf+1,1);
# R  = x(nx-lf+1:nx,1);
# M = hankel(C,R);

# B = M'*M;  beta = B(1,1)*mu/100;
# ab = (B + beta*eye(lf))\M'*y;
# temp = M*ab;
# temp = [temp;zeros(lf,1)];
# yb = temp;

# y  = x(lf+1:nx,1);
# C  = x(lf:nx-1,1);
# R = flipud(x(1:lf,1));
# M = toeplitz(C,R);

# B = M'*M;  beta = B(1,1)*mu/100;

# af = (B + beta*eye(lf))\M'*y;
# temp = M*af;
# temp = [zeros(lf,1);temp];
# yf = temp;

# return


# version vectoriel!!!!
inPoly <- function(x,y, vertx, verty){
	inPo <- rep(0L, length(x))
	nvert <- length(vertx)
	for(i in 1:nvert){
		j <- ifelse(i==1, nvert,i-1)
		myTest <- ((verty[i] > y) != (verty[j]>y)) &
			(x < (vertx[j]-vertx[i]) * (y-verty[i]) / (verty[j]-verty[i]) + vertx[i])
		inPo[myTest] <- !inPo[myTest]
	}
	return(inPo)
}

FKSpectrum <- function(A,dx=0.25,dz=0.8, npad=1, p=0.01,plot_spec=TRUE,return_spec=FALSE){
	# A <- GPR$data		#[90:1000,]
	nr <- nrow(A)	# time	
	nc <- ncol(A)	# x	

	#============== PLOT F-K SPECTRUM ===============#
	# padding (try also 2*(2^nextpow2(nc))
	nk <- npad*(nextpower2(nc))
	nf <- npad*(nextpower2(nr))
	A1 <- matrix(0,nrow=nf,ncol=nk)
	A1[1:nr,1:nc] <- A

	# function to center the spectrum!! (no need of fttshift!)
	#centres spectrum: Gonzalez & Wintz (1977) Digital Image Processing p.53
	A1	<- A1 * (-1)^(row(A1) + col(A1))
	A1_fft <- fft(A1)
	A1_fft_pow <- Mod(A1_fft)
	A1_fft_phase <- Arg(A1_fft)
	# plotGPR((A1_fft_phase[1:(nf/2),])^0.05)

	# frequency
	T = dz*10^(-9)		# [s] Sample time
	# Fs = 1/T			# [Hz] Sampling frequency
	fre <- 1:(nrow(A1_fft_pow)/2)/(2*(nrow(A1_fft_pow)/2) * T)/1000000	#[MHz]

	# wavenumber
	Ks = 1/dx			# [1/m] Sampling frequency
	knu <- 1:(ncol(A1_fft_pow)/2)/(2*(ncol(A1_fft_pow)/2) * dx)	#[1/m]
	knutot <- c(-rev(knu),knu)

	# labels
	xat 	<- c(1,nk/2,nk)
	xLabels <- c(min(knutot), 0, max(knutot))
	yat		<- c(1,nf/2,nf)
	yLabels	<- c(0, max(fre)/2, max(fre))

	# Note: when plotting spectra (S)  use log(S) or S.^alpha (alpha=0.1-0.3) to
	#       increase the visibility of small events 
	# p = 0.05
	if(plot_spec){
		image((t(A1_fft_pow[1:(nf/2),])^p), xat=xat, xLabels=xLabels, yat=yat,yLabels=yLabels,xlab="wavenumber (1/m)",ylab="frequency MHz")
	}
	if(return_spec){
		return(list(pow=A1_fft_pow[1:(nf/2),], pha=A1_fft_phase[1:(nf/2),]))
	}
}




FKFilter <- function(A,fk,L=c(5,5),npad=1){
	nr <- nrow(A)	# time	
	nc <- ncol(A)	# x	

	#============== PLOT F-K SPECTRUM ===============#
	# padding (try also 2*(2^nextpow2(nc))
	nk <- npad*(nextpower2(nc))
	nf <- npad*(nextpower2(nr))
	A1 <- matrix(0,nrow=nf,ncol=nk)
	A1[1:nr,1:nc] <- A

	# function to center the spectrum!! (no need of fttshift!)
	#centres spectrum: Gonzalez & Wintz (1977) Digital Image Processing p.53
	# A1	<- A1 * (-1)^(row(A1) + col(A1))
	A1_fft <- fft(A1)
	
	# plotGPR(Mod(A1_fft)^0.05)
	# plotGPR(Re(fft(A1_fft,inv=TRUE))[1:nr,1:nc])
	# plotGPR(A)
	
	#============== FILTER F-K SPECTRUM ===============#
	myFlong <- matrix(0,nrow=nf,ncol=nk)
	myFlong[1:(nf/2),1:(nk/2)] <- fk[(nf/2):1,(nk/2):1]
	# myFlong	<- myFlong * (-1)^(row(myFlong) + col(myFlong))
	myFlong[(nf/2+1):(nf),(nk/2 + 1):nk] <- fk[1:(nf/2),1:(nk/2)]
	myFlong[1:(nf/2),(nk/2 + 1):nk] <- fk[(nf/2):1,(nk):(nk/2 + 1)]
	# myFlong[(nf/2+1):(nf),1:(nk/2)] <- fk[1:(nf/2),(nk/2 + 1):(nk)]
	myFlong[(nf/2+1):(nf),1:(nk/2)] <- fk[1:(nf/2),(nk/2 + 1):nk]
	# plotGPR(myFlong)


	# hamming window
	if(length(L)==1) L <- c(L,L)
	if(all(L!=0)){
		ham2D = hammingWindow(L[1])%*%t(hammingWindow(L[2]))
		ham2Dlong = matrix(0,nrow=nf,ncol=nk)
		ham2Dlong[1:L[1],1:L[2]] <- ham2D
		# plotGPR(ham2Dlong)
		FF <-  Re(fft(fft(myFlong) * fft(ham2Dlong),inv=TRUE))
	}else{
		FF <- myFlong
	}
	FF <- FF/sum(FF)
	
	# plotGPR(Re(fft(fft(myFlong) * fft(ham2Dlong),inv=TRUE))[1:nr,1:nc])
	
	A_back <- Re(fft(A1_fft * FF,inv=TRUE))[1:nr,1:nc]
	# plotGPR(A_back)
	# plotGPR(A_back)
	# scaling
	return(A_back/(max(A_back)-min(A_back))*(max(A)-min(A)))
}

#========================================================#
#================= LOCAL ORIENTATION ====================#
#========================================================#

# -------------------------------------------
# ------------localOrientation--------------------------
# -------------------------------------------
# @name	localOrientation (Compute the local orientation)
# @description Compute the local orientation based on the tensor of the
# gradient field and on SVD decomposition.

# @date 04.09.2012 12:45
# @auteur Emanuel Huber

# @require source("convolution2D.R") -> tim.colors + image.plot
	# source('convolution2D.R')
	# cat('> Function(s) loaded: "convolution2D.R" \n')

# @param [matrix] 	A 				(each column represent a trace, each row a time step / depth step)
# @param [boolean] 	plot=TRUE 		(if false nothing is ploted except the frame and the colorbar)	
# @param [boolean]	trNorm=FALSE	(if true, each single trace are normalized (tn(i) = t(i)/sum(t(i))))
# @param [c(2)]		zlim=			(limit for the z-values (min,max))
# @param [text]		ylab= 			(label of the y-axis)
# @param [text]		xlab= 			(label of the x-axis)
# @param [text]		title= 			(title of the plot)
# @param [text]		col= 			(color of the plot, can be a colorbar)

# @return void
# -------------------------------------------

localOrientation <- function(P,blksze=c(5,10), thresh=0.1, winEdge=c(7,7), winBlur = c(3,3), winTensor = c(5,10), sdTensor=2, ...){
	
	n = nrow(P)
	m = ncol(P)
	
	#-------------------------------------------------
	#- Identify ridge-like regions and normalise image
	#-------------------------------------------------
	# normalization (mean = 0, sd = 1)
	Pn = (P-mean(P))/sd(as.vector(P))
	
	#blksze = 10
	#thresh = 0.1;

	# # apply standard deviation block-wise
	# nseq = seq(1,n/blksze[1])
	# mseq = seq(1,m/blksze[2])
	# P_sd = matrix(NA, nrow=n, ncol=m)
	# for(i in nseq){
		# for(j in mseq){
			# nv = c(((i-1)*blksze[1] +1):(i*blksze[1]))
			# mv = c(((j-1)*blksze[2] + 1):(j*blksze[2]))
			# std = sd(array(Pn[nv,mv]),na.rm=TRUE)
			# P_sd[nv,mv] = std
		# }
	# }
	# mask = P_sd > thresh
  
	# # normalize the image so that ridge = 0 and sdt = 1 
	# P = (P-mean(P[mask],na.rm=T))/sd(array(P[mask]),na.rm=T)
	
	P <- Pn
	
	#------------------------------
	#- Determine ridge orientations
	#------------------------------
	# reduction of the matrix
	#P =	P[1:floor(n/nnx)*nnx,1:floor(m/nny)*nny]
	
	# IMAGE SMOOTHING
	# Window size for bluring
	if(!is.null(winBlur)){
		blurWinX = winBlur[1]
		blurWinY = winBlur[2]
		k = matrix(1,nrow=blurWinX,ncol=blurWinY,byrow=TRUE)/(blurWinX * blurWinY)
		P_f  = convolution2D(P,k , 0)
	}else{
		P_f <- P
	}

	# GRADIENT FIELD
	# window size for edge dectection
	nnx = winEdge[1]
	nny = winEdge[2]
	vx = convolution2D(P_f, dx_gkernel(nnx,nny,1), 0)
	vy = convolution2D(P_f, dy_gkernel(nnx,nny,1), 0)

	image(t(vx),col=gray(seq(0,1,len = 250)))	
	image(t(vy),col=gray(seq(0,1,len = 250)))	
	  
	# TENSOR
	Gxx = vx^2
	Gyy = vy^2
	Gxy = vx*vy 
	  
	# LOCAL AVERAGED TENSOR
	#sze = 5 *2
	Jxx  = convolution2D(Gxx, gkernel(winTensor[1],winTensor[2],sdTensor), 0)
	Jyy  = convolution2D(Gyy, gkernel(winTensor[1],winTensor[2],sdTensor), 0)
	Jxy  = convolution2D(Gxy, gkernel(winTensor[1],winTensor[2],sdTensor), 0)

	# ANALYTIC SOLUTION BASED ON SVD DECOMPOSITION
	# (eigenvalue...)
	o_alpha = Jxx + Jyy									# energy
	o_beta  = sqrt((Jxx-Jyy)^2 + 4*(Jxy)^2)/o_alpha		# anisotropy
	o_theta = 1/2*atan2(2*Jxy,(Jxx - Jyy)) + pi/2		# orientation
	o_lambda1 = (Jxx + Jyy + sqrt((Jxx - Jyy)^2 + 4*(Jxy)^2))/2
	o_lambda2 = (Jxx + Jyy - sqrt((Jxx - Jyy)^2 + 4*(Jxy)^2))/2
	
	return(list(energy = o_alpha, anisotropy = o_beta, orientation = o_theta, lambda1 = o_lambda1, lambda2 = o_lambda2 ))

}
#-----------------
#-----------------


# Gaussian 2d-kernel
# n = nrow
# m = mrow
# sigma = sd
gkernel <- function(n,m, sigma=1){
	siz   = (n-1)/2;
	y = matrix(-siz:siz,n,m)
	siz   = (m-1)/2;
	x = matrix(-siz:siz,n,m,byrow=T)
	g = exp(-(x^2+y^2)/(2*sigma^2))
	sumg=sum(g)
	if(sumg!=0){
		g/sumg
	}else{
		g
	}
	
}

# Gaussian x-derivative kernel
# as edge detector
dx_gkernel <- function(n,m, sigma=1){
	siz   = round((n-1)/2);
	y = matrix(-siz:siz,n,m)
	siz   = (m-1)/2;
	x = matrix(-siz:siz,n,m,byrow=T)
	g = x*exp(-(x^2+y^2)/(2*sigma^2))

}

# Gaussian y-derivative kernel
# as edge detector
dy_gkernel <- function(n,m, sigma=1){
	siz   = round((n-1)/2);
	y = matrix(-siz:siz,n,m)
	siz   = (m-1)/2;
	x = matrix(-siz:siz,n,m,byrow=T)
	g = y*exp(-(x^2+y^2)/(2*sigma^2))
}




convolution2D <- function(h,k, bias=0){
	nh = nrow(h)
	mh = ncol(h)
	nk = nrow(k)
	mk = ncol(k)
	if(nk > nh || mk > mh){
		stop("Kernel 'k' should be smaller than the matrix 'h'\n")
	}
	h0 <- paddMatrix(h,nk,mk)
	nL <- nrow(h0)
	mL <- ncol(h0)
	k0 <- matrix(0, nrow=nL, ncol=mL)
	# h0[(nk-1) + 1:nh, (mk-1) + 1:mh] <- h
	h0[1:nh,  1:mh] <- h
	k0[1:nk, 1:mk] <- k
	g <- Re(fft(fft(k0)*fft(h0),inverse=TRUE))
	g2 <- g[nk-1 + 1:nh, mk-1 + 1:mh]
	# g2 <- g[nk + 1:nh, mk + 1:mh]
	return(g2)
}
# pads the edges of an image to minimize edge effects 
# %during convolutions and Fourier transforms. 
# %Inputs %I - image to pad 
# %p - size of padding around image 
# %Output %Ipad - padded image 
# SOURCE: http://matlabgeeks.com/tips-tutorials/how-to-blur-an-image-with-a-fourier-transform-in-matlab-part-i/
 # service@matlabgeeks.com i
paddMatrix <- function(I,p1, p2=NULL){
	if(is.null(p2)){
		p2 <- p1
	}
	nI <- nrow(I)
	mI <- ncol(I)
	Ipad <- matrix(0, nrow=nI+2*p1, ncol=mI + 2*p2)
	# middle
	Ipad[(p1+1):(p1+nI),(p2+1):(p2+mI)] <- I
	# top and bottom
	Ipad[1:p1,(p2+1):(p2+mI)] <- repmat(I[1,], p1, 1)
	Ipad[(p1+nI+1):(nI+2*p1), (p2+1):(p2+mI)] <- repmat(I[nI,], p1, 1)
	# left and right
	Ipad[(p1+1):(p1+nI), 1:p2] <- repmat(I[,1], 1, p2)
	Ipad[(p1+1):(p1+nI), (p2+mI+1):(mI + 2*p2)] <- repmat(I[,mI],1,p2)
	# corner
	Ipad[1:p1, 1:p2] <- I[1,1]
	Ipad[1:p1,(p2+mI+1):(mI + 2*p2)] <- I[1,mI]
	Ipad[(p1+nI+1):(nI+2*p1), 1:p2] <- I[nI,1]
	Ipad[(p1+nI+1):(nI+2*p1), (p2+mI+1):(mI+2*p2)] <- I[nI,mI]
	return(Ipad)
}


#########################################################################################
# Source:
# A replication of MatLab repmat function!
# R FOR OCTAVE USERS
# version 0.4
# Copyright (C) 2001 Robin Hankin
# http://cran.r-project.org/doc/contrib/R-and-octave.txt
#########################################################################################
repmat <- function(a,n,m) {kronecker(matrix(1,n,m),a)}


# -------------------------------------------
# ------------readDT1--------------------------
# -------------------------------------------
# @name	readDT1 (plot Ground Penetrating Radar image)
# @description This function read *.HD and associated *.DT1
# files from Sensors & Software.

# @date 30.04.2014 08:33
# @auteur Emanuel Huber
# @param [text]		filePath 			(file path of *.hd or *.dt1 file)
# @require source("trim.R")
	# source('trim.R')
	# cat('> Function(s) loaded: "trim.R" \n')
# @return list((hd = headerHD, dt1hd = headerDT1, data=myData))
# -------------------------------------------

readDT1 <- function( filePath){
	dirName 	<- dirname(filePath)
	splitBaseName <- unlist(strsplit(basename(filePath),'[.]'))
	baseName 	<- paste(splitBaseName[1:(length(splitBaseName)-1)],sep="")
	
	fileNameHD 	<- paste(dirName, "/",baseName,".HD",sep="")
	fileNameDT1	<- paste(dirName, "/",baseName,".DT1",sep="")
	
	headHD <-  scan(fileNameHD, what=character(),strip.white=TRUE,quiet=TRUE,fill=TRUE,blank.lines.skip=TRUE,flush=TRUE,sep="\n")
	nHD <- length(headHD)
	headerHD <- data.frame(nrow=nHD,ncol=2)
	for(i in seq_along(headHD)){
		hdline <- strsplit(headHD[i],"=")[[1]]
		if(length(hdline) < 2){
			headerHD[i,1] <- ""
			headerHD[i,2] <- trim(hdline[1])
		}else{
			headerHD[i,1:2] <-  as.character(sapply(hdline[1:2],trim))
		}
	}

	nbTraces 	= as.integer(as.character(headerHD[4,2]))
	nbPt 		= as.integer(as.character(headerHD[5,2]))
	#----------------#
	#--- READ DT1 ---#
	dt1 <- file(fileNameDT1 , "rb")

	indexDT1Header=c("traces", "position", "samples","topo", "NA1", "bytes","tracenb", "stack","window","NA2", "NA3",
				"NA4", "NA5", "NA6", "recx","recy","recz","transx","transy","transz","time0","zeroflag", "NA7", "time","x8",
				"com")	#,"com1","com2","com3","com4","com5","com6")
	headerDT1 = list()
	myData = matrix(NA,nrow=nbPt,ncol=nbTraces)
	for(i in 1:nbTraces){
		for(j in 1:25){
			headerDT1[[indexDT1Header[j]]][i] = readBin(dt1, what=numeric(), n = 1L,size=4)
			# hour of the day: format(as.POSIXct('0001-01-01 00:00:00') + headerDT1$time[1], "%I:%M:%S %p") 
		}
		# read the 28 characters long comment
		headerDT1[[indexDT1Header[26]]][i] = readChar(dt1, 28)
		# read the nbPt * 2 bytes rrace data
		myData[,i] = readBin(dt1, what=integer(), n = nbPt, size=2)
	}
	#headerDT1$time2 <- format(as.POSIXct(paste(as.character(headerHD[2,2]), ' 00:00:00', sep="")) + headerDT1$time, "%d-%m-%Y %I:%M:%S") 
	close(dt1)
	return(list(hd = headerHD, dt1hd = headerDT1, data=myData))
}
#-----------------
#-----------------


# A = GPR$hd
# if position = TRUE, return the row number
# if number = TRUE, try to convert

getHD <- function(A,string,number=TRUE,position=FALSE){
	if(position){
		which((trim(A[,1]) == string ) == TRUE)[1]
	
	}else{
		if(number){
			as.numeric(A[trim(A[,1])==string,2])
		}else{
			A[trim(A[,1])==string,2]
		}
	}

}

#--------------------------------------

# http://stackoverflow.com/questions/17256834/getting-the-arguments-of-a-parent-function-in-r-with-names

# Ryan Grannell
# website 	twitter.com/RyanGrannell
# location 	Galway, Ireland




get_args <- function (return_character=TRUE) {
	arg <- as.list(match.call(def = sys.function( -1 ),
				   call = sys.call(-1),
				   expand.dots = TRUE )
				   )
	narg <- length(arg)
	if(return_character){
		if(narg >=3){
			eval_arg <- sapply(arg[3:narg],eval)
			paste(arg[[1]],":", paste(names(arg[3:narg]),sapply(eval_arg,pasteArgs,arg[3:narg]),sep="=",collapse="+"),sep="")
		}else{
			paste(arg[[1]],":",sep="")
		}
	}else{
		return(arg)
	}
}


pasteArgs <- function(eval_arg,arg){
	if(is.numeric(eval_arg) || is.character(eval_arg)){
		return(paste(eval_arg,collapse=",",sep=""))
	}else if(is.list(eval_arg)){
		return(paste(names(eval_arg),"<-", (eval_arg),collapse=",",sep=""))
	}else if(is.matrix(eval_arg)){
		return(paste(arg))
	}else if(any(is.null(eval_arg))){
		return("")
	}
}

addArg <- function(proc, arg){
# paste(names(arg[3:narg]),sapply(eval_arg,paste_args,arg[3:narg]),sep="=",collapse="+")
	proc_add <- paste(names(arg), sapply(arg,pasteArgs, arg),sep="=",collapse="+")
	if(substr(proc,nchar(proc),nchar(proc)) == ":"){
		proc <- paste(proc,proc_add,sep="")
	}else{
		proc <- paste(proc,"+",proc_add,sep="")
	}
	return(proc)
}

#--------------------------------
# wapply: A faster (but less functional) ‘rollapply’ for vector setups
# April 23, 2013
# By A.N. Spiess, senior scientist at the Department of Andrology at the University Hospital Hamburg-Eppendorf
# This is what turned out (wapply for “window apply”)
wapply <- function(x, width, by = NULL, FUN = NULL, ...){
	FUN <- match.fun(FUN)
	if (is.null(by)) by <- width
	lenX <- length(x)
	SEQ1 <- seq(1, lenX - width + 1, by = by)
	SEQ2 <- lapply(SEQ1, function(x) x:(x + width - 1))
	 
	OUT <- lapply(SEQ2, function(a) FUN(x[a], ...))
	OUT <- base:::simplify2array(OUT, higher = TRUE)
	return(OUT)
}


coord2Line <- function(x){
	sp::Line(x[,1:2])
}
Line2Lines <- function(i,pp, myNames){
	sp::Lines(pp[i],myNames[i])
}