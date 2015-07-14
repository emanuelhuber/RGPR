
#------------------------------------------#
#----------- CLASS DEFINITION -------------#
setClass(
	Class="GPRsurvey",	
	slots=c(
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
# LINES <- choose.files(caption = " RDS files",filters = c("rds","*.rds"))
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
	coords <- list()
	fids <- list()
	for(i in seq_along(LINES)){
		gpr <- readGPR(LINES[[i]])
		# FIX ME!
		#	> check if name(gpr) is unique
		# 	> check if "crs" is unique
		# 	> check if "posunit" is unique
		line_names[i] 			<- name(gpr)
		line_descriptions[i] 	<- description(gpr)
		line_surveymodes[i] 	<- gpr@surveymode
		line_dates[i] 			<- gpr@date
		line_freq[i] 			<- gpr@freq
		line_antsep[i] 			<- gpr@antsep
		posunit 				<- gpr@posunit
		crs 					<- gpr@crs
		if(length(gpr@coord)>0){
			if(is.null(colnames(gpr@coord))){
				coords[[line_names[i] ]] <- gpr@coord
			}else if(all(toupper(colnames(gpr@coord)) %in% c("E","N","Z"))){
				coords[[line_names[i] ]] <- gpr@coord[,c("E","N","Z")]
			}else if(all(toupper(colnames(gpr@coord)) %in% c("X","Y","Z"))){
				coords[[line_names[i] ]] <- gpr@coord[,c("X","Y","Z")]
			}else{
				coords[[line_names[i] ]] <- gpr@coord
			}
			# coords[[line_names[i] ]] 	<- gpr@coord
			line_lengths[i]			<- lineDist(gpr@coord[,1:2],last=TRUE)
		}else{
			# coords[[line_names[i]]] 	<- NULL
			line_lengths[i]		<- gpr@dx * gpr@ntr
		}
		fids[[line_names[i] ]]		<- gpr@com
	}
	
	x <- new("GPRsurvey",filepaths	= LINES, 				# vector of [n] file names
				names			= line_names,			# length = [n]
				descriptions 	= line_descriptions,	# length = [n]
				surveymodes 	= line_surveymodes,		# length = [n]
				dates 			= line_dates,			# length = [n]
				freqs 			= line_freq, 			# length = [n]
				lengths 		= line_lengths, 			# length = [n]
				antseps 		= line_antsep,			# length = [n]
				posunit 		= posunit,		# length = 1
				crs 			= crs,			# length = 1
				coords			= coords,		# header
				fids			= fids,
				intersections	= list()
	)
	x <- setCoordref(x)
	return(x)
}


setAs(from = "GPRsurvey", to = "SpatialLines",
      def = function (from) as.SpatialLines(from))
setAs(from = "GPRsurvey", to = "SpatialPoints",
      def = function (from) as.SpatialPoints(from))	  

as.SpatialLines <- function (x, ...){
	TOPO <- x@coords
	Names <- x@names
	lineList <- lapply(TOPO,coord2Line)
	linesList <- lapply(seq_along(lineList), Line2Lines, lineList ,Names)
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
		
	
	  # type=match.arg(type)
	# TOPO <- x@coords
	# Names <- x@names
	# if(type=="lines"){	
		# lineList <- lapply(TOPO,coord2Line)
		# linesList <- lapply(seq_along(lineList), Line2Lines, lineList ,Names)
		# mySpatLines <- SpatialLines(linesList)
		# if(crs(x) == '' || nchar(crs(x)) == 1){
			# warning("no CRS defined!\n")
		# }else{
			# proj4string(mySpatLines) <- CRS(crs(x))
		# }
		
		# d.f <- data.frame(z=seq_along(mySpatLines), row.names = sapply(slot(mySpatLines, "lines"), 
			# function(x) slot(x, "ID")))

		# mySpatLinesdf <- SpatialLinesDataFrame(mySpatLines, d.f , match.ID = TRUE)

		# writeOGR(mySpatLinesdf, folder, filename, driver="ESRI Shapefile")
	# }else if(type=="points"){	
		# allTopo <- do.call(rbind,TOPO)	#  N, E, Z
		# # allNames <- sapply(rep(Names, each=sapply(TOPO, length))
		# # A <- cbind(allTopo,allNames)
		# # allTogether <- as.data.frame(cbind(allTopo,allNames))
		# allTopo2 <- as.data.frame(allTopo)
		# coordinates(allTopo2) = ~E + N
		# if(crs(x) == '' || nchar(crs(x)) == 1){
			# warning("no CRS defined!\n")
		# }else{
			# proj4string(allTopo2) <- CRS(crs(x))
		# }
		# writeOGR(allTopo2, folder, filename, driver="ESRI Shapefile")

setMethod("setCoordref", "GPRsurvey", function(x){
		if(length(x@coords)>0){
			A <- do.call("rbind",x@coords)
			A <- apply(round(A),2,range)
			Evalue <- minCommon10(A[1,1],A[2,1])
			Nvalue <- minCommon10(A[1,2],A[2,2])
			Zvalue <- 0
			x@coordref <- c(Evalue, Nvalue,Zvalue)
			cat(x@coordref, "!!!\n")
			x <- surveyIntersections(x)
		}
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

setMethod("getLine", "GPRsurvey", function(x,no){
		no <- no[1]
		gpr <- readGPR(x@filepaths[[no]])
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
	overview <- data.frame("name"=.filename(x@filepaths),
							"length"=round(x@lengths,2),
							"units" = rep(x@posunit,n),
							"date" = x@dates,
							"fequency" = x@freqs,
							"coordinates" = is_test[testCoords],
							"intersections" = is_test[testIntersecs])
	print(overview)
	if(length(x@coords)>0 ){
		cat("- - - - - - - - - - - - - - -\n")
		if(x@crs!=""){
			cat("Coordinate system:", x@crs,"\n")
		}else{
			cat("Coordinate system: unknown\n")
		}
		cat
	}
	cat("****************\n")
	return(invisible(overview))
}
# > 3. And finally a call to setMethod():
setMethod("show", "GPRsurvey", function(object){print.GPRsurvey(object)}) 

setMethod("length", "GPRsurvey", function(x) ncol(x@data))

setMethod(
	f="length", 
	signature="GPRsurvey", 
	definition=function(x){
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
		addIntersections <- TRUE
		addFid <- TRUE
		dots <- list()
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
				addIntersections <- dots$addIntersections
			}
			dots$addIntersections <- NULL
			if(!is.null(dots$addFid)){
				addFid <- dots$addFid
			}
			dots$addFid <- NULL
			dots$add <- NULL
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
				BASEName <- unlist(strsplit(basename(shp_files),'[.]'))[seq(from=1,length.out=length(shp_files),by=2)]
				DIRName <- dirname(shp_files)
				for(i in seq_along(shp_files)){
					shp <- readOGR(DIRName[i], BASEName[i])
					cat(DIRName[i], BASEName[i],"\n",sep="")
					plot(shp, add=TRUE,pch=13,col="darkblue")
				}
			}
		}
		niet <- lapply(x@coords, plotLine, lwd=lwd, col=col )
		if(addArrows){
			niet <- lapply(x@coords, plotArrows, lwd=lwd)
		}
		if(addFid){
			for(i in 1:length(x)){
				fidxyz <- fidpos(x@coords[[i]],x@fids[[i]])
				if(length(fidxyz)>0){
					points(fidxyz[,1:2],pch=21,col="black",bg="red",cex=0.7)
				}
			}
		}
		if(length(x@intersections)>0 && addIntersections){
			for(i in 1:length(x@intersections)){
				if(!is.null(x@intersections[[i]])){
					points(x@intersections[[i]][,1:2],pch=1,cex=0.8)
				}
			}
		}
	}else{
		warning("no coordinates")
	}
}

setMethod("surveyIntersections", "GPRsurvey", function(x){
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
			# lineName <- substr(BASEName[[j]], 6, nchar(BASEName[[j]]))
			if(!is.null(pt_int)){
				# cat("intersection!\n")
			  # for each intersection points
			  n_int <- 	nrow(coordinates(pt_int))
			  for(k in 1:n_int){
				d <- sqrt(apply((top0[,1:2] - matrix(coordinates(pt_int)[k,],nrow=nrow(top0),ncol=2,byrow=TRUE))^2,1,sum))
				# if(length(c(coordinates(pt_int),which.min(d)[1],x@names[j]))!=4) stop("lkjlkJ")
				myTr_int <- rbind(myTr_int ,c(coordinates(pt_int)[k,],which.min(d)[1],x@names[j]))
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

setMethod("interpTraces", "GPRsurvey", function(x,topo){
		for(i in seq_along(x)){
			gpr <- readGPR(x@filepaths[[i]])
			topoLine <- topo[[i]]
			gpr <- interpTraces(gpr,topoLine)
			x@coords[[gpr@name]] <- gpr@coord
			x@lengths[i] <- lineDist(gpr@coord[,1:2],last=TRUE)
		}			
		x <- setCoordref(x)
		return(x)
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
			x@lengths[i] <- lineDist(values[[i]][,1:2],last=TRUE)
		}
		# in setCoordref, the intersection is computed by 
		#		"x <- surveyIntersections(x)"
		x <- setCoordref(x)
		return(x)
	}
)

setMethod("writeGPR", "GPRsurvey", function(x,path, format=c("DT1","rds")){
		mainDir <- dirname(path)
		if(mainDir =="." || mainDir =="/" ){
			mainDir <- ""
		}
		subDir <- basename(path)
		if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
			# cat("subDir exists in mainDir and is a directory")
		} else 	if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
			subDir <- ""
			# you will probably want to handle this separately
		} else {
			warning("directory ", path, " does not exist - creating")
			dir.create(file.path(mainDir, subDir))
		}
		for(i in seq_along(x)){
			gpr <- readGPR(x@filepaths[[i]])
			if(length(x@coords[[gpr@name]])>0){
				coord(gpr) <- x@coords[[gpr@name]]
			}
			if(length(x@intersections[[gpr@name]])>0){
				ann(gpr) <- x@intersections[[gpr@name]][,3:4]
			}

			# if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
				# # By this point, the directory either existed or has been successfully created
				# setwd(file.path(mainDir, subDir))
			# } else {
				# cat("subDir does not exist")
				# # Handle this error as appropriate
			# }
			# dir.create(file.path(mainDir, subDir))
			# setwd(file.path(mainDir, subDir))
			filepath <- paste(mainDir,"/",subDir,"/",gpr@name,".rds",sep="")
			writeGPR(gpr, path=filepath,format="rds")
			cat("File saved:",filepath,"\n")
		}			
	
	}
)


setMethod("plot3D", "GPRsurvey", function(x,add_topo=FALSE,clip=NULL,normalize=NULL,nupspl=NULL,add=TRUE,
		xlim=NULL,ylim=NULL,zlim=NULL,...){
# plot3D <- function(x,type=c("raster","wiggles"),add_topo=FALSE,clip=NULL,normalize=NULL,nupspl=NULL,...){
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
				cat(gpr@name, ": no coordinates, I cannot plot this line!!\n",sep="")
			}else{
				plot3D(gpr,add_topo=add_topo,clip=clip,normalize=normalize,nupspl=nupspl,add=add,xlim=xlim,ylim=ylim,zlim=zlim,...)
			}
			add <- TRUE
		}	
	}
)




setMethod("plotDelineations3D", "GPRsurvey", function(x,sel=NULL,col=NULL,add=TRUE,...){
		add<-add
		for(i in seq_along(x)){
			gpr <- readGPR(x@filepaths[[i]])
			if(length(x@coords[[gpr@name]])>0){
				coord(gpr) <- x@coords[[gpr@name]]
				# cat(x@coordref,"\n")
				gpr@coordref <- x@coordref
			}
			if(length(coord(gpr))==0){
				cat(gpr@name, ": no coordinates, I cannot plot this line!!\n",sep="")
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
setMethod("exportFID", "GPRsurvey", function(x,filepath=NULL){
		for(i in seq_along(x)){
			gpr <- readGPR(x@filepaths[[i]])
			file_name <- paste(filepath,gpr@name,".txt",sep="")
			exportFID(gpr,file_name)
			cat("File \"",file_name,"\" created!\n",sep="")
			# x@coords[[gpr@name]] <- gpr@coord
		}
	}
)

setMethod("exportCoord", "GPRsurvey", function(x,filepath=NULL,type=c("points","lines"),driver="ESRI Shapefile",...){
	type=match.arg(type)
	folder <- dirname(filepath)
	filename <- basename(filepath)
	if(type=="lines"){	
		mySpatLines <- as.SpatialLines(x)
		dfl <- data.frame(z=seq_along(mySpatLines), 
				row.names = sapply(slot(mySpatLines, "lines"), 
							function(x) slot(x, "ID")))
		mySpatLinesdf <- SpatialLinesDataFrame(mySpatLines, dfl , match.ID = TRUE)
		writeOGR(mySpatLinesdf, folder, filename, driver=driver,...)
	}else if(type=="points"){	
		mySpatPoints <- as.SpatialPoints(x)
		writeOGR(mySpatPoints, folder, filename, driver=driver,...)
	}
})

setMethod("exportDelineations", "GPRsurvey", function(x, path=""){
	for(i in seq_along(x)){
		exportDelineations(getLine(x,no=i),path=path)
	}
})
