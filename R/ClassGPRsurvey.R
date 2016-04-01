
#------------------------------------------#
#----------- CLASS DEFINITION -------------#
setClass(
	Class="GPRsurvey",	
	slots=c(
		version = "character", 		# version of the class
		filepaths="character", 		# filepath of the GPR data
		names="character",			# names of the GPR profiles
		descriptions ="character",	# descriptions of the GPR profiles
		freqs ="numeric", 			# frequencies of the GPR profiles
		lengths="numeric",			# length in metres of the GPR profiles = [n]
		surveymodes ="character",	# survey mode of the GPR profiles (reflection/CMP)
		dates ="character",			# dates  of the GPR profiles
		antseps ="numeric",			# antenna separation of the GPR profiles
		posunit = "character",		# position units 
		crs ="character",			# coordinates reference system
		coordref="numeric",			# reference position
		coords="list",				# (x,y,z) coordinates for each profiles
		intersections="list",		# (x,y) position of the profile intersections
		fids="list"					# fiducials of the GPR profiles
	)
)

#------------------------------------------#
#-------------- CONSTRUCTOR ---------------#
# LINES = list of datapath
GPRsurvey <- function(LINES){
	n <- length(LINES)
	line_names <- character(n)
	line_descriptions <- character(n)
	line_surveymodes <- character(n)
	line_dates <- character(n)
	line_freq <- numeric(n)
	line_antsep <- numeric(n)
	line_lengths <- numeric(n)
	posunit <- character(1)
	crs <- character(1)
	xyzCoords <- list()
	fids <- list()
	for(i in seq_along(LINES)){
		gpr <- readGPR(LINES[[i]])
		# FIX ME! SHOULD BE OK!
		#	> check if name(gpr) is unique
		# 	> check if "crs" is unique
		# 	> check if "posunit" is unique
		line_names[i] 			<- name(gpr)[1]
		line_descriptions[i] 	<- description(gpr)
		line_surveymodes[i] 	<- gpr@surveymode
		line_dates[i] 			<- gpr@date
		line_freq[i] 			<- gpr@freq
		line_antsep[i] 			<- gpr@antsep
		posunit 				<- gpr@posunit[1]
		crs 					<- gpr@crs[1]
		if(length(gpr@coord)>0){
			if(is.null(colnames(gpr@coord))){
				xyzCoords[[line_names[i] ]] <- gpr@coord
			}else if(all(toupper(colnames(gpr@coord)) %in% c("E","N","Z"))){
				xyzCoords[[line_names[i] ]] <- gpr@coord[,c("E","N","Z")]
			}else if(all(toupper(colnames(gpr@coord)) %in% c("X","Y","Z"))){
				xyzCoords[[line_names[i] ]] <- gpr@coord[,c("X","Y","Z")]
			}else{
				xyzCoords[[line_names[i] ]] <- gpr@coord
			}
			# xyzCoords[[line_names[i] ]] 	<- gpr@coord
			line_lengths[i]			<- posLine(gpr@coord[,1:2],last=TRUE)
		}else{
			# xyzCoords[[line_names[i]]] 	<- NULL
# 			line_lengths[i]		<- gpr@dx * gpr@ntr
			line_lengths[i]		<- gpr@dx * ncol(gpr@data)
		}
		fids[[line_names[i] ]]		<- trimStr(gpr@com)
	}
	
	x <- new("GPRsurvey",
				version 		= "0.1",
				filepaths		= LINES, 			# vector of [n] file names
				names			= line_names,			# length = [n]
				descriptions 	= line_descriptions,	# length = [n]
				surveymodes 	= line_surveymodes,		# length = [n]
				dates 			= line_dates,			# length = [n]
				freqs 			= line_freq, 			# length = [n]
				lengths 		= line_lengths, 			# length = [n]
				antseps 		= line_antsep,			# length = [n]
				posunit 		= posunit,		# length = 1
				crs 			= crs,			# length = 1
				coords			= xyzCoords,		# header
				fids			= fids,
				intersections	= list()
	)
	x <- coordref(x)
	return(x)
}


setAs(from = "GPRsurvey", to = "SpatialLines",
      def = function (from) as.SpatialLines(from))
setAs(from = "GPRsurvey", to = "SpatialPoints",
      def = function (from) as.SpatialPoints(from))	  

as.SpatialLines <- function (x, ...){
	TOPO <- x@coords
	Names <- x@names
	lineList <- lapply(TOPO,xyToLine)
	linesList <- lapply(seq_along(lineList), LineToLines, lineList ,Names)
	mySpatLines <- sp::SpatialLines(linesList)
	if(crs(x) == '' || nchar(crs(x)) == 1){
		warning("no CRS defined!\n")
	}else{
		proj4string(mySpatLines) <- CRS(crs(x))
	}
	return(mySpatLines)
}

as.SpatialPoints <- function (x, ...){
	allTopo <- do.call(rbind,x@coords)	#  N, E, Z
	allTopo2 <- as.data.frame(allTopo)
	sp::coordinates(allTopo2) = ~E + N
	if(crs(x) == '' || nchar(crs(x)) == 1){
		warning("no CRS defined!\n")
	}else{
		sp::proj4string(allTopo2) <- sp::CRS(crs(x))
	}
	return(allTopo2)
}

#' Define a local reference coordinate
#' 
#' @name coordref
#' @rdname coordref
#' @export
setMethod("coordref", "GPRsurvey", function(x){
		if(length(x@coords)>0){
			A <- do.call("rbind",x@coords)
			A <- apply(round(A),2,range)
			Evalue <- .minCommon10(A[1,1],A[2,1])
			Nvalue <- .minCommon10(A[1,2],A[2,2])
			Zvalue <- 0
			x@coordref <- c(Evalue, Nvalue,Zvalue)
			cat("Coordinates of the local system:", x@coordref,"\n")
			x <- surveyIntersect(x)
		}
		return(x)
	}
)

setReplaceMethod(
	f="coordref",
	signature="GPRsurvey",
	definition=function(x,value){
		x@coordref <- value
		return(x)
	}
)

setMethod("crs", "GPRsurvey", function(x){
		return(x@crs)
	} 
)

setReplaceMethod(
	f="crs",
	signature="GPRsurvey",
	definition=function(x,value){
		value <- as.character(value)[1]
		x@crs <- value
		return(x)
	}
)

#------------------------------
# "["
setMethod(
	f= "[",
	signature="GPRsurvey",
	definition=function(x,i,j,drop){
		if(missing(i)) i <- j
		# cat(typeof(i),"\n")
		# cat(j,"\n")
		# i <- as.numeric(i)
		y <- x
		y@filepaths		<- x@filepaths[i]		# vector of [n] file names
		y@names			<- x@names[i]			# length = [n]
		y@descriptions	<- x@descriptions[i] 	# length = [n]
		y@surveymodes 	<- x@surveymodes[i]		# length = [n]
		y@dates			<- x@dates[i]			# length = [n]
		y@freqs			<- x@freqs[i] 			# length = [n]
		y@lengths		<- x@lengths[i] 		# length = [n]
		y@antseps		<- x@antseps[i] 		# length = [n]
		# posunit 		= posunit,				# length = 1
		# crs 			= crs,					# length = 1
		y@coords		<- x@coords[x@names[i]]				# header
		y@fids			<- x@fids[x@names[i]]
		y@intersections	<- x@intersections[x@names[i]]
		return(y)
	}
)

#------------------------------
# "[["
# return an instance of the class GPR!
# identical to getGPR
# i can be either the gpr data number or the gpr data name
setMethod(
	f= "[[",
	signature="GPRsurvey",
	definition=function (x, i, j, ...){
		if(missing(i)) i <- j
		return(getGPR(x,id=i))
	}
)

setMethod("getGPR", "GPRsurvey", function(x,id){
		if(length(id)>1){
			warning("Length of id > 1, I take only the first element!\n")
			id <- id[1]
		}
		if(is.numeric(id)){
			gpr <- readGPR(x@filepaths[[id]])
		}else if(is.character(id)){
			no <- which(x@names == trimStr(id))
			if(length(no > 0)){
				gpr <- readGPR(x@filepaths[[no]])
			}else{
				stop("No GPR lines with name", trimStr(id),"\n")
			}
		}
		if(length(x@coords[[gpr@name]])>0){
			coord(gpr) <- x@coords[[gpr@name]]
		}
		if(length(x@intersections[[gpr@name]])>0){
			ann(gpr) <- x@intersections[[gpr@name]][,3:4,drop=FALSE]
		}
		if(length(x@coordref)>0){
			gpr@coordref <- x@coordref
		}
		return(gpr)
	}
)



#-------------------------------------------#
#---------------- SETMETHOD ----------------#
# Print methods
# setMethod("print", "GPR", function(x) print.GPR(x))	 
# > 2. S3 function:
print.GPRsurvey <- function(x, ...){
	cat("*** Class GPRsurvey ***\n")
	n <- length(x)
	dirNames <- dirname(x@filepaths)
	if(length(unique(dirNames))==1){
		cat("Unique directory:", dirNames[1],"\n")
	}else{
		cat("One directory among others:", dirNames[1],"\n")
	}
	testCoords <- rep(0,n)
	names(testCoords) <- x@names
	if(length(x@coords)>0){
		testLength <- sapply(x@coords,length)
		testCoords[names(testLength)] <- testLength
	}
	testCoords <- as.numeric(testCoords > 0)+1
	testIntersecs <- rep(0,n)
	names(testIntersecs) <- x@names
	if(length(x@intersections)>0){
		testLength <- sapply(x@intersections,length)
		testIntersecs[names(testLength)] <- testLength
	}
	testIntersecs <- as.numeric(testIntersecs > 0)+1
	
	is_test <- c("NO","YES")
	cat("- - - - - - - - - - - - - - -\n")
	overview <- data.frame("name (id)"=.fNameWExt(x@filepaths),
							"length"=round(x@lengths,2),
							"units" = rep(x@posunit,n),
							"date" = x@dates,
							"fequency" = x@freqs,
							"coordinates" = is_test[testCoords],
							"intersections" = is_test[testIntersecs])
	print(overview)
	if(length(x@coords)>0 ){
		cat("- - - - - - - - - - - - - - -\n")
		if(length(x@crs) > 0 ){
			cat("Coordinate system:", x@crs,"\n")
		}else{
			cat("Coordinate system: undefined\n")
		}
		cat
	}
	cat("****************\n")
	return(invisible(overview))
}
# > 3. And finally a call to setMethod():
setMethod("show", "GPRsurvey", function(object){print.GPRsurvey(object)}) 

setMethod("length", "GPRsurvey", function(x) ncol(x@data))

setMethod(f="length", signature="GPRsurvey", definition=function(x){
		length(x@filepaths)
	}
)

# parameter add=TRUE/FALSE
# 			addArrows = TRUE/FALSE
plot.GPRsurvey <- function(x,y,...){
	if(length(x@coords)>0){
		plotAdd <- FALSE
		addArrows <- TRUE
		add_shp_files <- FALSE
		# addIntersections <- TRUE
		parIntersect <- list(pch=1,cex=0.8)
		parFid <- list(pch=21,col="black",bg="red",cex=0.7)
		#addFid <- TRUE
		lwd=1
		col <- 1
		# print(list(...))
		if( length(list(...)) ){
			dots <- list(...)
			if( !is.null(dots$add) && isTRUE(dots$add) ){
				plotAdd <- TRUE
			}
			if( !is.null(dots$addArrows) && isTRUE(!dots$addArrows) ){
				addArrows <- FALSE
			}
			dots$addArrows <- NULL
			if(!is.null(dots$lwd)){
				lwd <- dots$lwd
			}
			if(!is.null(dots$col)){
				col <- dots$col
			}
			if(!is.null(dots$addIntersections)){
				stop(paste0("'addIntersections' no more used!",
                      " Use instead 'parIntersect' with a vector",
                      " of arguments for the points function.\n"))
				#addIntersections <- dots$addIntersections
			}
			#dots$addIntersections <- NULL
			if(!missing(parFid)){
			#if(!is.null(dots$parIntersect)){
				parIntersect <- dots$parIntersect
			}
			if(!missing(parFid)){
			#if(!is.null(dots$parFid)){
				parFid <- dots$parFid
#stop("safdlkajslkj")
			}
			if(!is.null(dots$addFid)){
				stop(paste0("'addFid' no more used! Use instead 'parFid'",
                      " with a vector of arguments for the points",
                      "function.\n"))
			}
			# dots$addFid <- NULL
			# dots$add <- NULL
			if(!is.null(dots$shp_files)){
				add_shp_files <- TRUE
				shp_files <- dots$shp_files
			}
			dots$shp_files <- NULL
		}
		dots <- c(dots, list("type"="n"))
		# print(dots)
		# print(dots)
		if(!plotAdd){
			do.call("plot", c(list((do.call(rbind,x@coords))[,1:2]),dots))
		}
		if(add_shp_files){
			if(length(shp_files) > 0){
                sel <- seq(from=1,length.out=length(shp_files),by=2)
				BASEName <- unlist(strsplit(basename(shp_files),'[.]'))[sel]
				DIRName <- dirname(shp_files)
				for(i in seq_along(shp_files)){
					shp <- readOGR(DIRName[i], BASEName[i])
					cat(DIRName[i], BASEName[i],"\n",sep="")
					plot(shp, add=TRUE,pch=13,col="darkblue")
				}
			}
		}
		niet <- lapply(x@coords, .plotLine, lwd=lwd, col=col )
		if(addArrows){
			niet <- lapply(x@coords, .plotArrows, lwd=lwd)
		}
		if(!is.null(parFid)){
			for(i in 1:length(x)){
# 				fidxyz <- .fidpos(x@coords[[i]],x@fids[[i]])
				fidxyz <- x@coords[[i]][trimStr(x@fids[[i]]) != "", , 
                                    drop=FALSE]
				if(length(fidxyz)>0){
					do.call( points, c(list(x=fidxyz[,1:2]),parFid))
				}
			}
		}
		if(!is.null(parIntersect) && length(x@intersections)>0){ 
			for(i in 1:length(x@intersections)){
				if(!is.null(x@intersections[[i]])){
					do.call(points , c(list(x=x@intersections[[i]][,1:2]), 
					parIntersect))
				}
			}
		}
	}else{
		warning("no coordinates")
	}
}

setMethod("surveyIntersect", "GPRsurvey", function(x){
		# intersections <- list()
		for(i in seq_along(x@coords)){
		  top0 <- x@coords[[i]]
		  gtop0 <- sp::Line(top0[,1:2])
		  gtopa <- sp::Lines(list(gtop0), ID=c("a"))
		  Sa = SpatialLines(list(gtopa))
		  v <- seq_along(x@coords)[-i]
		  myCo_int <- c()
		  myTr_int <- c()
		  for(j in v){
			# cat(j,"\n")
			top1 <- x@coords[[j]]
			gtop1 <- sp::Line(top1[,1:2])
			gtopb <- sp::Lines(list(gtop1), ID=c("b"))
			Sb = SpatialLines(list(gtopb))
			pt_int <- gIntersection(Sa,Sb)
			if(!is.null(pt_int)){
				# cat("intersection!\n")
			  # for each intersection points
			  n_int <- 	nrow(coordinates(pt_int))
			  for(k in 1:n_int){
				d <- sqrt(apply((top0[,1:2] - matrix(coordinates(pt_int)[k,],
                            nrow=nrow(top0),ncol=2,byrow=TRUE))^2,1,sum))
				# if(length(c(coordinates(pt_int),which.min(d)[1],x@names[j]))!=4) 
				#stop("lkjlkJ")
				myTr_int <- rbind(myTr_int ,c(coordinates(pt_int)[k,],
                              which.min(d)[1],x@names[j]))
			  }
			}
		  }
		  if(length(myTr_int) > 0){
			x@intersections[[x@names[i]]] <- myTr_int
		  }else{
			x@intersections[[x@names[i]]] <- NULL
		  }
		}
		return(x)
	} 
)

setMethod("intersections", "GPRsurvey", function(x){
		return(x@intersections)
	}
)

setMethod("interpPos", "GPRsurvey", function(x,topo, ...){
		for(i in seq_along(x)){
			gpr <- readGPR(x@filepaths[[i]])
			topoLine <- topo[[i]]
			gpr <- interpPos(gpr,topoLine, ...)
			x@coords[[gpr@name]] <- gpr@coord
			x@lengths[i] <- posLine(gpr@coord[,1:2],last=TRUE)
		}			
		x <- coordref(x)
		return(x)
	}
)

setMethod(
	f="coords",
	signature="GPRsurvey",
	definition=function(x, i){
		if(length(x@coords) == 0){
		  return(x@coords)
		}
		if(missing(i)){
		  return(x@coords)
		}else{
		  return(x@coords[[i]])
		}
	}
)

setReplaceMethod(
	f="coords",
	signature="GPRsurvey",
	definition=function(x,values){
		if(!is.list(values)){
			stop("values should be a list!!\n")
		}
		if(length(values)!=length(x)){
			stop("number of elements not equal to the number of gpr files!!\n")
		}
		for(i in seq_along(x)){
			if(is.null(colnames(values[[i]]))){
				x@coords[[x@names[i]]] <- values[[i]]
			}else if(all(toupper(colnames(values[[i]])) %in% c("E","N","Z"))){
				x@coords[[x@names[i]]] <- values[[i]][c("E","N","Z")]
			}else{
				x@coords[[x@names[i]]] <- values[[i]]
			}
			x@lengths[i] <- posLine(values[[i]][,1:2],last=TRUE)
		}
		# in coordref, the intersection is computed by 
		#		"x <- surveyIntersect(x)"
		x <- coordref(x)
		return(x)
	}
)

setMethod("plot3DRGL", "GPRsurvey", 
        function(x,addTopo=FALSE,clip=NULL,normalize=NULL,nupspl=NULL,
        add=TRUE,xlim=NULL,ylim=NULL,zlim=NULL,...){
		add<-add
		for(i in seq_along(x)){
			cat("***", i , "***\n")
			gpr <- readGPR(x@filepaths[[i]])
			if(length(x@coords[[gpr@name]])>0){
				coord(gpr) <- x@coords[[gpr@name]]
				# cat(x@coordref,"\n")
				gpr@coordref <- x@coordref
			}
			if(length(coord(gpr))==0){
				cat(gpr@name, paste0(": no coordinates, I cannot plot",
                  " this line!!\n",sep=""))
			}else{
				plot3DRGL(gpr, addTopo=addTopo, clip=clip, normalize=normalize, 
                    nupspl=nupspl,add=add,xlim=xlim, ylim=ylim, zlim=zlim,...)
			}
			add <- TRUE
		}	
	}
)




setMethod("plotDelineations3D", "GPRsurvey", 
          function(x,sel=NULL,col=NULL,add=TRUE,...){
		add<-add
		for(i in seq_along(x)){
			gpr <- readGPR(x@filepaths[[i]])
			if(length(x@coords[[gpr@name]])>0){
				coord(gpr) <- x@coords[[gpr@name]]
				# cat(x@coordref,"\n")
				gpr@coordref <- x@coordref
			}
			if(length(coord(gpr))==0){
				cat(gpr@name, paste0(": no coordinates, I cannot plot",
                  " this line!!\n",sep=""))
			}else if(length(gpr@delineations) == 0){
				cat(gpr@name, ": no delineations for this line!!\n",sep="")
			}else{
				plotDelineations3D(gpr,sel=sel,col=col,add=add,...)
			}
			add <- TRUE
		}	
	}
)


#----------------------- EXPORT/SAVE -----------------#
setMethod("writeSurvey", "GPRsurvey", function(x, fPath, overwrite=FALSE){
	if(isTRUE(overwrite)){
		cat("file may be overwritten\n")
	}else{
	  fPath <- safeFPath(fPath)
	}
	x@filepath <- as.character(fPath)
	namesSlot <- slotNames(x)
	xList <- list()
# 	xList[["version"]] <- "0.1"
	for(i in seq_along(namesSlot)){
		xList[[namesSlot[i]]] <- slot(x, namesSlot[i])
	}
	saveRDS(xList, fPath)
# 	saveRDS(x, fPath)
})



setMethod("writeGPR", "GPRsurvey", 
        function(x,fPath, format=c("DT1","rds"), overwrite=FALSE){
		type=match.arg(format)
		mainDir <- dirname(path)
		if(mainDir =="." || mainDir =="/" ){
			mainDir <- ""
		}
		subDir <- basename(path)
		if (file.exists(paste(mainDir, subDir, "/", sep = "/", 
            collapse = "/"))) {
			# cat("subDir exists in mainDir and is a directory")
		} else 	if (file.exists(paste(mainDir, subDir, sep = "/", 
            collapse = "/"))) {
			#subDir <- ""
			# you will probably want to handle this separately
		} else {
			warning("directory ", path, " does not exist - creating")
			dir.create(file.path(mainDir, subDir))
		}
		for(i in seq_along(x)){
			# gpr <- readGPR(x@fPaths[[i]])
			gpr <- x[[i]]
			if(length(x@coords[[gpr@name]])>0){
				coord(gpr) <- x@coords[[gpr@name]]
			}
			if(length(x@intersections[[gpr@name]])>0){
				ann(gpr) <- x@intersections[[gpr@name]][,3:4]
			}
			fPath <- paste(mainDir,"/",subDir,"/",gpr@name,".",type,sep="")
			writeGPR(gpr, path=fPath,format=type , overwrite=FALSE)
			cat("File saved:",fPath,"\n")
		}			
	
	}
)

setMethod("exportFid", "GPRsurvey", function(x,fPath=NULL){
		for(i in seq_along(x)){
			gpr <- readGPR(x@filepaths[[i]])
			file_name <- paste(fPath,gpr@name,".txt",sep="")
			exportFid(gpr,file_name)
			cat("File \"",file_name,"\" created!\n",sep="")
			# x@coords[[gpr@name]] <- gpr@coord
		}
	}
)

setMethod("exportCoord", "GPRsurvey", 
    function(x,fPath=NULL,type=c("points","lines"),
            driver="ESRI Shapefile",...){
	type <- match.arg(type)
	folder <- dirname(fPath)
	fPath <- basename(fPath)
	if(type=="lines"){	
		mySpatLines <- as.SpatialLines(x)
		dfl <- data.frame(z=seq_along(mySpatLines), 
				row.names = sapply(slot(mySpatLines, "lines"), 
							function(x) slot(x, "ID")))
		mySpatLinesdf <- SpatialLinesDataFrame(mySpatLines, dfl , 
                            match.ID = TRUE)
		writeOGR(mySpatLinesdf, folder, fPath, driver=driver,...)
	}else if(type=="points"){	
		mySpatPoints <- as.SpatialPoints(x)
		writeOGR(mySpatPoints, folder, fPath, driver=driver,...)
	}
})

setMethod("exportDelineations", "GPRsurvey", function(x, dirpath=""){
	for(i in seq_along(x)){
		exportDelineations(getGPR(x,id=i),path=path)
	}
})
