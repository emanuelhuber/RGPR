

#' RGPR: A package for processing and visualising ground-penetrating data 
#' (GPR).
#'
#' The RGPR package provides two classes GPR and GPRsurvey
#' 
#' @section Reading/writing/export functions:
#' \itemize{
#'   \item \code{readGPR()}: format DT1 (Sensors&Software), rds (R-format)
#'   \item \code{writeGPR()}: format DT1 (Sensors&Software), rds (R-format)
#'   \item \code{exportPDF()}
#'   \item \code{exportDelineations()}
#'   \item \code{exportFid()}: ASCII-file
#'   \item \code{exportCoord()}: ASCII, SpatialLines or SpatialPoints
#'   \item \code{exportProc()}: ASCII-file
#' }
#'
#' @section Plot functions:
#' \itemize{
#'   \item \code{plot()}: raster or wiggles.
#'   \item \code{plot3D()}:
#'   \item \code{plotAmpl()}
#'   \item \code{plotDelineations()}
#' }
#'
#' @section Coercion:
#' \itemize{
#'   \item \code{as.matrix()}:
#'   \item \code{as.numeric()}:
#'   \item \code{as.list()}:
#'   \item \code{as.SpatialPoints()}:
#'   \item \code{as.SpatialLines()}:
#' }
#'
#' @section Delineation:
#' \itemize{
#'   \item \code{delineate()}:
#'   \item \code{plotDelineations()}:
#'   \item \code{delineations()}: list of the delineations
#'   \item \code{addDelineation}:
#'   \item \code{rmDelineations}:
#'   \item \code{exportDelineations}:
#'   \item \code{plotDelineations3D}:
#'   \item \code{identifyDelineation}:
#' }
#'
#' @references Several books!
#' @name RGPR
# @docType package
NULL

# WARNING: type = c("wiggles", "raster") should be the second arguments!!!

# Reflection mode, CMP mode 
# For a given transect, the data consist of a cross-section of 
# signal amplitudes (intensities) versus location 
# (along the two-way time axis and the horizontal axis). 

#
#    CHECK:  http://r-pkgs.had.co.nz/
#

##----------- helper functions -------------------##
# FID <- choose.files(caption = " txt files",filters = c("txt","*.txt"))
# output = list of data frame (one for each file from FID) 
#    with c("E","N","Z","TRACE") structure
#' @export
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
#' @export
readTopo <- function(TOPO,sep=","){
  myTopo <- list() 
  for(i in seq_along(TOPO)){
    A <- read.table(TOPO[[i]], sep=sep, stringsAsFactors = FALSE, header = TRUE)
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

  TS     <- agrep("TS" , PCODE)    # TOTAL STATION
  REF   <- agrep("REF" , PCODE)    # TOTAL STATION
  WATER   <- agrep("WATER" , PCODE)    # TOTAL STATION
  CROSS   <- which("CROSS" == PCODE)    # TOTAL STATION
  REVERSE  <- agrep("REVERSE", PCODE)  # 180° hor, ver
  LINES   <- agrep("LINE", PCODE) 
  LINES   <- LINES[!(agrep("LINE", PCODE) %in% REVERSE)]

  POINTS <- which(!(1:length(PCODE) %in% 
            c(LINES,TS,REVERSE,WATER,CROSS, REF)))  
  # topo
  NOT_REVERSE <- !(1:length(PCODE) %in% agrep("REVERSE", PCODE))

  not_rev <- !(1:nrow(topo) %in% agrep("REVERSE",topo$PCODE))

  #----------------------------------------------------
  if(add==FALSE){
    plot(topo[not_rev,c("E","N")],type="n", asp=1)
  }
  # reverse
  for(i in 1:length(REVERSE)){
    points( - topo[topo[,"PCODE"]==PCODE[REVERSE[i]],c("E","N")],
              pch=20,col=1)
  }
  # Water
  points( topo[topo[,"PCODE"] %in% PCODE[WATER],c("E","N")],pch=10,col=1)
  # points
  for(i in 1:length(POINTS)){
    points(topo[topo[,"PCODE"]==PCODE[POINTS[i]],c("E","N")],
                pch=3,col=1,cex=0.7)
  }
  # ref
  points(topo[topo[,"PCODE"]%in% PCODE[REF],c("E","N")],
          pch=25,col=3,bg="green")  
}



##------------- FILENAME/FILEPATH/EXTENSION -------------------##
# NAME: safe file path
# test if the file already exists
# if yes, add a suffix to the filepath
safeFPath <- function(fPath = NULL){
  dirName   <- dirname(fPath)
  fName <- .fNameWExt(fPath)
  ext <- .fExt(fPath)
  if(dirName == '.'){
    fPath <- fName
  }else{
    fPath <- paste0(dirName, '/' , fName)
  }
  fPath_orgi <- fPath
  k <- 0
  while(file.exists(paste0(fPath, ".", ext)) || 
          file.exists( paste0(fPath, ".HD"))){
    fPath <- paste0(fPath_orgi, "_", k)
    k <- k + 1
  }
  newfPath <- paste0(fPath, ".", ext)
  return(newfPath)
}

# returns string w/o leading or trailing whitespace
trimStr <- function (x) gsub("^\\s+|\\s+$", "", x)

# return filename without extension
#' @export
.fNameWExt <- function(x){
  unlist(lapply(strsplit(basename(x),"[.]"), head , 1 ))
}

# return the file extension.

#' @export
.fExt <- function(x){
#   cat("with caution... because split \'.\' may not be so good\n")
  unlist(lapply(strsplit(basename(x),"[.]"), tail , 1 ))
}

##------------- COLOR FUNCTIONS -------------------##
#' @name palGPR
#' @rdname palGPR
#' @export
palGPR <- function(colPal="default", n = 101, power = 1, returnNames = FALSE){
  colPal <- gsub("gray", "grey", x= colPal)
  tmp <- structure(list(
    default = colorRampPalette(c("#1C007C", "#1B0086", "#1A0091", "#18009C",
                "#1600A7", "#1400B2", "#1100C3", "#0E00CF", "#0A00E0",
                "#0300F5", "#0001FF", "#080FFF", "#1521FF", "#2232FF",
                "#2E42FF", "#3B52FF", "#4862FF", "#5470FF", "#617FFF",
                "#6E8CFF", "#7F9EFF", "#8CAAFF", "#98B5FF", "#A5C1FF",
                "#B2CBFF", "#BFD5FF", "#CBDFFF", "#D8E7FF", "#E5F0FF",
                "#F2F7FF", "#FFFCFB", "#FFF4F0", "#FFECE5", "#FFE3DA",
                "#FFDACE", "#FFCEC0", "#FFC4B5", "#FFB9AA", "#FFAE9E",
                "#FF9F90", "#FF9485", "#FF877A", "#FF766B", "#FF6960",
                "#FF5B55", "#FF4946", "#FF3B4E", "#FF3045", "#FF253D",
                "#FF1632", "#FF0B2A", "#FF0022", "#F70023", "#EE0023",
                "#E50023", "#DC0024", "#D30024", "#CA0024", "#C20024",
                "#B70023", "#AF0023", "#A70023", "#9C0022"))(n),
    hcl_0 = colorspace::diverge_hcl(n,power=1),
    # blue - white - red (fade)
    hcl_1 = colorspace::diverge_hcl(n, c = 100, l = c(50, 90), power = power), 
    # blue - white - orange (fade)
    hcl_2 = colorspace::diverge_hcl(n, h = c(246, 40), c = 96, 
                        l = c(65, 90), power=power),
    #  green - white- orange (fade)
    hcl_3 = colorspace::diverge_hcl(n, h = c(130, 43), c = 100, 
                        l = c(70, 90), power=power), 
    # blue/violet - white - red/violet 
    hcl_4 = colorspace::diverge_hcl(n, h = c(255, 330), 
                        l = c(40, 90), power=power), 
    # rose - white - turquise (fade)
    hcl_5 = colorspace::diverge_hcl(n, h = c(20, 200), c = 90, 
                        l = c(70, 95), power=power),  
    #  blue - white - red (vivid)
    hcl_6 = colorspace::diverge_hcl(n, h = c(246, 10), c = 120, 
                          l = c(30, 90), power=power), 
    # blue - white - red (tern)
    hcl_7 = colorspace::diverge_hcl(n, h = c(220, 10), c = 100, 
                        l = c(20, 90), power=power), 
    # blue - white - red (fade)
    hcl_8 = colorspace::diverge_hcl(n, h = c(250, 10), c = 150, 
                        l = c(30, 90), power=power), 
    grey3 = colorspace::diverge_hcl(n, h = c(300, 1), c = 1, 
            l = c(1, 100), power=power), 
    # too light
    grey1 = colorspace::sequential_hcl(n, h = c(1, 300), c = 0, 
                          l = c(10, 100), power=power), 
    #  too dark  
    grey2 = colorspace::sequential_hcl(n, h = c(300, 100), c = 0, 
              l = c(120, 10), power=power), 
    grey = colorspace::sequential_hcl(n, h = c(190, 1), c = 10, 
                          l = c(1, 110), power=power)
  ))
  if(returnNames){
   return( names(tmp) )
  }
  (tmp[[match(colPal, names(tmp))]])
}

# source: vignette of the R-package "colorspace" (Color Space Manipulation)
# plot color palette
# usage: 
# pal(palGPR("seismic",50))
# pal(palGPR(n=50))


#' @name plotPal
#' @rdname palGPR
#' @export
plotPal <- function(col, border = NA){
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, 
        xlab = "",  ylab = "")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

#' Colour palette
#'
#' @examples
#' displayPalGPR()
#' plotPal(palGPR("hcl_5"))
#' @name displayPalGPR
#' @rdname palGPR
#' @export
displayPalGPR <- function(){
   op <- par(no.readonly=TRUE)
   par(mai=op$mai + c(0,1,0,0))
  pNames <- palGPR(returnNames=TRUE)
  n <- 101
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, length(pNames)), 
        axes = FALSE, xlab = "", ylab = "")
  for(i in seq_along(pNames)){
    myPal  <- palGPR(colPal=pNames[i], n=n)
    rect(0:(n-1)/n, i-1/3, 1:n/n, i + 1/3, col = myPal, border = NA)
    mtext(pNames[i], side=2, at=i, adj = 1, las = 1)
  }
  title("Colour palettes from RGPR (palGPR)")
  par(op)
}

#
#' @export
colFromPal <- function(A , col = palGPR(n=101)){
  CCY = (A-min(A,na.rm=TRUE))/(max(A,na.rm=TRUE)-min(A,na.rm=TRUE))
  ClimY <- range(CCY,na.rm=TRUE)
  ClenY <- ClimY[2] - ClimY[1] + 1
  return(col[ (CCY)*(length(col)-1)+1 ] )
}
#--------------------------------#





  
# select a box on plot(mySurvey) and return list(xlim,ylim)
# that can be used in plot3D:
#   plot(mySurvey,asp=1)
#   bbox <- selectBBox()
#   plot3D(mySurvey, addTopo=TRUE,clip=30, xlim=bbox$xlim,
# ylim=bbox$ylim, add=FALSE)

selectBBox <- function(border="red",lwd=2,...){
  bbox <- locator(type="p",n=2)
  LIM <- sapply(bbox, range)
  rect(LIM[1,"x"], LIM[1,"y"], LIM[2,"x"], LIM[2,"y"], border=border)
  return(list("xlim"=LIM[,"x"], "ylim" =LIM[,"y"]))
}

# NOT USED!
# .fidpos <- function(xyz,fid){
#   return(xyz[trimStr(fid)!="",,drop=FALSE])
# }

.plotLine <- function(xyz,...){
  lines(xyz[,1:2],...)
}

.plotArrows <- function(xyz,col="red",length=0.1,...){
  arrows(xyz[nrow(xyz)-1,1], xyz[nrow(xyz)-1,2], xyz[nrow(xyz),1], 
         xyz[nrow(xyz),2], length = length,col=col,...)
}

.whichMin <- function(x,y){
  which.min(abs(x-y))
}

.which <- function(x,y){
  which(x==y)
}

.lengthList <- function(x){
  if(typeof(x)=="list"){
    return(length(x))
    # print(typeof(x))
  }else{
    return(1)
  }
}

#' @export
posLine <- function(loc,last=FALSE){
  loc <- as.matrix(loc)
  all_dist <- cumsum(c(0,sqrt(apply(diff(loc)^2,1,sum))))
  if(last){
        return(tail(all_dist,1))
  }else{
        return(as.numeric(all_dist))
  }
}
  
.doubleVector <- function(v,n=2L){
  if(n > 1){
    m <- length(v)
    dxpos <- rep( diff(v)/n,n-1)
    vv <- v[-m] + rep(seq(1,n-1),each=m-1)*dxpos
    xvalues  <- sort(c(v,vv ,v[m] + cumsum(rep(dxpos[length(dxpos)],n-1))))
    xvalues <- xvalues[1:(length(xvalues))]
    return(xvalues)
  }else{
    return(v)
  }
}

#--------------------------------
# wapply: A faster (but less functional) "rollapply" for vector setups
# April 23, 2013
# By A.N. Spiess, senior scientist at the Department of Andrology at the 
# University Hospital Hamburg-Eppendorf
# This is what turned out (wapply for "window apply")
wapply <- function(x=NULL, width = NULL, by = NULL, FUN = NULL, ...){
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width
  lenX <- length(x)
  SEQ1 <- seq(1, lenX - width + 1, by = by)
  SEQ2 <- lapply(SEQ1, function(x) x:(x + width - 1))
   
  OUT <- lapply(SEQ2, function(a) FUN(x[a], ...))
  OUT <- base:::simplify2array(OUT, higher = TRUE)
  return(OUT)
}

xyToLine <- function(x){
  sp::Line(x[,1:2])
}

LineToLines <- function(i,pp, myNames){
  sp::Lines(pp[i],myNames[i])
}


#--------------------------------------------#
#---------------- SETGENERIC ----------------#
# setGenericVerif <- function(x,y){
#   if(!isGeneric(x)){
#     setGeneric(x,y)
#   }else{
#     cat("setGeneric", x,"already exists!\n")
#   }
# }
setGenericVerif <- function(x,y){setGeneric(x,y)}


#------------------------------

setGenericVerif("as.SpatialPoints", function(x) 
standardGeneric("as.SpatialPoints"))

setGenericVerif("as.SpatialLines", function(x) 
standardGeneric("as.SpatialLines"))


#------------------------------
#' @name coordref
#' @rdname coordref
#' @export
setGenericVerif("coordref", function(x) standardGeneric("coordref"))

#' @name coordref<-
#' @rdname coordref
#' @export
setGenericVerif("coordref<-", function(x, value) standardGeneric("coordref<-"))

setGenericVerif("intersections", function(x) standardGeneric("intersections"))

#' @name filepath
#' @rdname filepath
#' @export
setGenericVerif("filepath", function(x) standardGeneric("filepath"))

#' @name filepath<-
#' @rdname filepath
#' @export
setGenericVerif("filepath<-", function(x, value) standardGeneric("filepath<-"))

setGenericVerif("coords", function(x,i) standardGeneric("coords"))
setGenericVerif("coords<-",function(x,values){standardGeneric("coords<-")})

#' @name coord
#' @rdname coord
#' @export
setGenericVerif("coord", function(x, i, ...) standardGeneric("coord"))

#' @name coord<-
#' @rdname coord
#' @export
setGenericVerif("coord<-",function(x,values){standardGeneric("coord<-")})

#' @name vel
#' @rdname vel
#' @export
setGenericVerif("vel", function(x) standardGeneric("vel"))

#' @name vel<-
#' @rdname vel
#' @export
setGenericVerif("vel<-",function(x,values){standardGeneric("vel<-")})

#' @name ann
#' @rdname ann
#' @export
setGenericVerif("ann", function(x) standardGeneric("ann"))

#' @name ann<-
#' @rdname ann
#' @export
setGenericVerif("ann<-",function(x,values){standardGeneric("ann<-")})

#' @name name
#' @rdname name
#' @export
setGenericVerif("name", function(x) standardGeneric("name"))

#' @name name<-
#' @rdname name
#' @export
setGenericVerif("name<-",function(x,value){standardGeneric("name<-")})

#' @name crs
#' @rdname crs
#' @export
setGenericVerif("crs", function(x) standardGeneric("crs"))

#' @name crs<-
#' @rdname crs
#' @export
setGenericVerif("crs<-",function(x,value){standardGeneric("crs<-")})

#' @name time0
#' @rdname time0
#' @export
setGenericVerif("time0", function(x) standardGeneric("time0"))

#' @name time0
#' @rdname time0
#' @export
setGenericVerif("time0<-",function(x,value){standardGeneric("time0<-")})

#' @name fid
#' @rdname fid
#' @export
setGenericVerif("fid", function(x) standardGeneric("fid"))

#' @name fid<-
#' @rdname fid
#' @export
setGenericVerif("fid<-",function(x,values){standardGeneric("fid<-")})

#' @name values
#' @rdname values
#' @export
setGenericVerif("values", function(x) standardGeneric("values"))

#' @name values<-
#' @rdname values
#' @export
setGenericVerif("values<-", function(x,value) standardGeneric("values<-"))

#' @name processing
#' @rdname processing
#' @export
setGenericVerif("processing", function(x) standardGeneric("processing"))

#' @name description
#' @rdname description
#' @export
setGenericVerif("description", function(x) standardGeneric("description"))

#' @name description<-
#' @rdname description
#' @export
setGenericVerif("description<-", function(x, value) 
standardGeneric("description<-"))

#------------------------------GPR
setGenericVerif("gethd", function(x,hd=NULL) standardGeneric("gethd"))

#' @name plotAmpl
#' @rdname plotAmpl
#' @export
setGenericVerif("plotAmpl", function(x, FUN = mean, add = FALSE, 
                all = FALSE,...) standardGeneric("plotAmpl"))
setGenericVerif("ampl", function(x, FUN=mean, ...) standardGeneric("ampl"))

#' @name interpPos
#' @rdname interpPos
#' @export
setGenericVerif("interpPos", function(x, topo, ...) 
    standardGeneric("interpPos"))

#' @name relPos
#' @rdname relPos
#' @export
setGenericVerif("relPos", function(x) 
    standardGeneric("relPos"))
    
#' @name readGPR
#' @rdname readGPR
#' @export
setGenericVerif("readGPR", function(fPath,desc="", coordfile=NULL,
                crs="", intfile=NULL) standardGeneric("readGPR"))

setGenericVerif("writeGPR", function(x,fPath, format=c("DT1","rds"),
                overwrite=FALSE){ standardGeneric("writeGPR")})

#' @name exportCoord
#' @rdname exportCoord
#' @export
setGenericVerif("exportCoord",  
          function(x, type = c("SpatialPoints", "SpatialLines", "ASCII"),
          fPath = NULL, folder = NULL,  sep = "\t", 
          driver = "ESRI Shapefile",...) standardGeneric("exportCoord"))
#' @name exportFid
#' @rdname exportFid
#' @export
setGenericVerif("exportFid", function(x,fPath=NULL) 
                  standardGeneric("exportFid"))

#' @name exportProc
#' @rdname exportProc
#' @export
setGenericVerif("exportProc",  function(x,fPath=NULL,sep="\t", row.names=FALSE,
              col.names=FALSE, ...) standardGeneric("exportProc"))

#' @name reverse
#' @rdname reverse
#' @export
setGenericVerif("reverse", function(x) standardGeneric("reverse"))

setGenericVerif("migration", function(x,type=c("static","kirchhoff"), ...) 
standardGeneric("migration"))
setGenericVerif("upsample", function(x,n) standardGeneric("upsample"))
setGenericVerif("timeCorOffset", function(x) standardGeneric("timeCorOffset"))

#' @name filter1D
#' @rdname filter1D
#' @export
setGenericVerif("filter1D", function(x, type = c("median", "hampel", 
                "Gaussian"), ...) standardGeneric("filter1D"))

#' @name filter2D
#' @rdname filter2D
#' @export
setGenericVerif("filter2D", function(x, type=c("median3x3"), ...) 
                standardGeneric("filter2D"))
setGenericVerif("dewow", function(x,type=c("MAD","Gaussian"),w ) 
                standardGeneric("dewow"))
setGenericVerif("gain", function(x, type=c("power", "exp", "agc"),
                  ...) standardGeneric("gain"))
setGenericVerif("dcshift", function(x, u=1:10, FUN=mean) 
                standardGeneric("dcshift"))
setGenericVerif("firstBreack", function(x, w = 11, ns = NULL, bet=NULL) 
                standardGeneric("firstBreack"))

setGenericVerif("clip", function(x, Amax=NULL,Amin=NULL) 
                standardGeneric("clip"))
setGenericVerif("gammaCorrection", function(x, a=1,b=1) 
                standardGeneric("gammaCorrection"))
setGenericVerif("traceScaling", function(x, 
                  type = c("stat","min-max","95","eq","sum", "rms")) 
                  standardGeneric("traceScaling"))

setGenericVerif("spec", function(x, type=c("f-x","f-k"), plotSpec=TRUE, 
                unwrapPhase = TRUE, ...) standardGeneric("spec"))
setGenericVerif("fFilter", function(x, f=100, type=c('low','high','bandpass'),
                L=257,plotSpec=FALSE) standardGeneric("fFilter"))
setGenericVerif("fkFilter", function(x, fk=NULL, L=c(5,5),npad=1) 
                standardGeneric("fkFilter"))

setGenericVerif("traceShift", function(x, t0, keep = 10, delete0 = TRUE) 
                standardGeneric("traceShift"))
setGenericVerif("deconv", function(x, method=c("spiking", "wavelet",
                "min-phase", "mixed-phase"), ...) standardGeneric("deconv"))
setGenericVerif("rotatePhase", function(x, phi) standardGeneric("rotatePhase"))


#------------------------------GRPsurvey
setGenericVerif("getGPR", function(x,id) standardGeneric("getGPR"))
setGenericVerif("surveyIntersect", function(x) 
                standardGeneric("surveyIntersect"))
setGenericVerif("writeSurvey", function(x, fPath, overwrite=FALSE){ 
                standardGeneric("writeSurvey")})


#------------------------------BOTH
setGenericVerif("plot3DRGL", 
          function(x, addTopo = FALSE, clip = NULL, normalize = NULL, 
                  nupspl = NULL, add = TRUE, xlim = NULL, ylim = NULL, 
                  zlim = NULL,...) 
standardGeneric("plot3DRGL"))

setGenericVerif("exportPDF", function(x,fPath=NULL,addTopo=FALSE,clip=NULL,
normalize=NULL,nupspl=NULL,...) standardGeneric("exportPDF"))

#setGenericVerif("adimproSmooth", function(x,hmax=2,...) standardGeneric("
# adimproSmooth"))

#---------------------- DELINEATIONS ---------------------#
#' @name delineate
#' @rdname delineation
#' @export
setGenericVerif("delineate", function(x,name=NULL,type=c("raster","wiggles"),
                  addTopo=FALSE,nupspl=NULL,n=10000,...) 
                  standardGeneric("delineate"))
#' @name rmDelineations<-
#' @rdname delineation
#' @export
setGenericVerif("rmDelineations<-", function(x,values=NULL) 
                  standardGeneric("rmDelineations<-"))
#' @name delineations
#' @rdname delineation
#' @export
setGenericVerif("delineations", function(x,sel=NULL,...) 
                  standardGeneric("delineations"))
#' @name addDelineation
#' @rdname delineation
#' @export
setGenericVerif("addDelineation", function(x,...) 
                  standardGeneric("addDelineation"))
setGenericVerif("showDelineations", function(x,sel=NULL,...) 
                  standardGeneric("showDelineations"))
#' @name exportDelineations
#' @rdname delineation
#' @export
setGenericVerif("exportDelineations", function(x, dirpath="") 
                  standardGeneric("exportDelineations"))
#' @name plotDelineations3D
#' @rdname delineation
#' @export
setGenericVerif("plotDelineations3D", function(x,sel=NULL,col=NULL,add=TRUE,...)
                  standardGeneric("plotDelineations3D"))
#' @name plotDelineations
#' @rdname delineation
#' @export
setGenericVerif("plotDelineations", function(x,sel=NULL,col=NULL,...) 
                  standardGeneric("plotDelineations"))
#' @name identifyDelineation
#' @rdname delineation
#' @export
setGenericVerif("identifyDelineation", function(x,sel=NULL,...) 
                  standardGeneric("identifyDelineation"))

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

.plot3DRGL <- function(A,x,y,z,z0,col=palGPR(n=101),back="fill", 
                  smooth = TRUE, lit=FALSE, lwd=0,empty=FALSE,...){
  nr = nrow(A)
  nc = ncol(A)
  if(empty==TRUE){
    X <- matrix(x, ncol=nc, nrow=2, byrow=TRUE)
    Y <- matrix(y, ncol=nc, nrow=2, byrow=TRUE)
    Z <-  matrix(z0, ncol=nc, nrow=2, byrow=TRUE) - 
          matrix(z[c(1,nr)], ncol=nc, nrow=2, byrow=FALSE)
    colA <- col[1]
    if(!is.null(list(...)$alpha) && (list(...)$alpha==0 || is.null(col))){

    }else{
      rgl::rgl.surface(Y, X, Z, color=colA, back=back, smooth = smooth, 
                  lit=lit, lwd= lwd,...) 
    }
    lines3d(y,z0,x, col="black",alpha=1,lwd=lwd)   
    lines3d(y,(z0-z[length(z)]),x, col="black",alpha=1,lwd=lwd)   
    lines3d(rep(y[1],2),(z0[1]-z),rep(x[1],2), col="black",alpha=1,lwd=lwd)
    lines3d(rep(y[length(y)],2),(z0[length(z0)]-z),rep(x[length(x)],2),
            col="black",alpha=1,lwd=lwd)   

  }else{
    X <- matrix(x, ncol=nc, nrow=nr, byrow=TRUE)
    Y <- matrix(y, ncol=nc, nrow=nr, byrow=TRUE)
    Z <-  matrix(z0, ncol=nc, nrow=nr, byrow=TRUE) - 
          matrix(z, ncol=nc, nrow=nr, byrow=FALSE)
    A = (A-min(A))/(max(A)-min(A))
    colA <- col[ (A)*100+1 ] # assign colors to heights for each point 
    rgl::rgl.surface(Y, X, Z, color=colA, back=back, smooth = smooth, 
                lit=lit, lwd= lwd,...) 
  }
}

.plot3DSlice <- function(XYZ,slice=c("x","y","z"),section=1,col=palGPR(n=101), 
                          sampling = c(0.25,0.25,0.04),rmStripes = TRUE){
  # k=100
  # j=25
  # i=40
  # col <- tim.colors(101) # height color lookup table
  slice = match.arg(slice)
  if(length(slice)>1){
    slice = slice[1]
  }
  
  dimXYZ = dim(XYZ)
  vz = seq(0,dimXYZ[3]-1,by=1)*sampling[3]  # dtime / 2 * v
  vx = seq(0,dimXYZ[1]-1,by=1)*sampling[1]
  vy = seq(0,dimXYZ[2]-1,by=1)*sampling[2]
  if(rgl::rgl.cur()==0){  # si la fenêtre rgl est ouverte, on plot dedans...
    rgl::rgl.open()
    rgl::rgl.bg( color=c("white"))
  }
  i = section
  j=i
  k=i
  if(slice=="x"){
    if(rmStripes == TRUE){ Xside = normalizeGPR(removeStripes(t(XYZ[,j,])))
    }else{  Xside = normalizeGPR((t(XYZ[,j,])))  }
    
    Xside_x = matrix(vx,nrow=dimXYZ[3],ncol=dimXYZ[1],byrow=TRUE)
    Xside_y = matrix( vy[j],nrow=dimXYZ[3],ncol=dimXYZ[1],byrow=TRUE)
    Xside_z = matrix( max(vz)-vz,nrow=dimXYZ[3],ncol=dimXYZ[1],byrow=FALSE)

    CCX = (Xside-min(Xside))/(max(Xside)-min(Xside))
    ClimX <- range(CCX)
    ClenX <- ClimX[2] - ClimX[1] + 1
    # col <- tim.colors(101) # height color lookup table
    #col = palette(gray(0:101 / 101))
    colCX <- col[ (CCX)*100+1 ] 
    
    surface3d(Xside_x, Xside_z, Xside_y, col= setCol(Xside), lit=FALSE,
            front="fill",back="fill")#, alpha=0.5)
  }else if(slice=="z"){
    if(rmStripes == TRUE){ Zside = (removeStripes(t(XYZ[,,k])))
    }else{  Zside = ((t(XYZ[,,k])))  }
    
    Zside_x = matrix(vx,nrow=dimXYZ[2],ncol=dimXYZ[1],byrow=TRUE)
    Zside_y = matrix( vy,nrow=dimXYZ[2],ncol=dimXYZ[1],byrow=FALSE)
    Zside_z = matrix(max(vz) - vz[k],nrow=dimXYZ[2],ncol=dimXYZ[1],byrow=FALSE)

    CCZ = (Zside-min(Zside))/(max(Zside)-min(Zside))
    ClimZ <- range(CCZ)
    ClenZ <- ClimZ[2] - ClimZ[1] + 1
    #col = palette(gray(0:101 / 101))
    colCZ <- col[ (CCZ)*100+1 ]
    
    surface3d(Zside_x, Zside_z, Zside_y, col= setCol(Zside), lit=FALSE,
              front="fill",back="fill")#, alpha=0.5)
  }else if(slice=="y"){
    if(rmStripes == TRUE){ Yside = normalizeGPR(removeStripes(t(XYZ[i,,])))
    }else{  Yside = normalizeGPR((t(XYZ[i,,])))  }
    
    Yside_x = matrix(vx[i],nrow=dimXYZ[3],ncol=dimXYZ[2],byrow=TRUE)
    Yside_y = matrix( vy,nrow=dimXYZ[3],ncol=dimXYZ[2],byrow=TRUE)
    Yside_z = matrix( max(vz)-vz,nrow=dimXYZ[3],ncol=dimXYZ[2],byrow=FALSE)
    
#     CCY = (Yside-min(Yside))/(max(Yside)-min(Yside))
#     ClimY <- range(CCY)
#     ClenY <- ClimY[2] - ClimY[1] + 1
#     colCY <- col[ (CCY)*100+1 ] 
    colCY <- colFromPal(Yside , col = col )

    surface3d(Yside_x, Yside_z, Yside_y, col= setCol(Yside), lit=FALSE,
              front="fill",back="fill")#, alpha=0.5)
  }
}

# shift the topography of the GPR profile (to display its topography)
.topoShift <- function(x, topo, dz){
  zShift <- (max(topo) - topo)
  # in fact >> old_t = x@depth
  old_t <- seq(0, length.out = nrow(x), by = dz)
  xShifted <- matrix(0, nrow = nrow(x) + floor(max(zShift)/dz), ncol = ncol(x))
  n <- 1:(nrow(x)-2)
  for(i in 1:ncol(x)){
    # compute new t-vector for each trace
    new_t <- old_t + zShift[i]
    xit <- seq(ceiling(new_t[1]/dz), ceiling(new_t[nrow(x)-2]/dz))
    # interpolate
    # FIX ME! does not work well (not nice interpolation)
    xShifted[xit+1,i] = signal::interp1(new_t, x[,i], xi = xit*dz, 
                                        method = "pchip",extrap = TRUE)  
  }
  return(xShifted)
}

# x = data matrix (col = traces)
# topoGPR = z-coordinate of each trace
# dx = spatial sampling (trace spacing)
# dts = temporal sampling
# v = GPR wave velocity (ns)
# max_depth = to which depth should the migration be performed
# dz = vertical resolution of the migrated data
# fdo = dominant frequency of the GPR signal
.kirMig <- function(x, topoGPR, dx, dts, v, max_depth = 8, 
                 dz = 0.025, fdo = 80){
  n <- nrow(x)
  m <- ncol(x)
  z <- max(topoGPR) - topoGPR
  fdo <- fdo*10^6   # from MHz to Hz
  lambda <- fdo / v * 10^-9
  v2 <- v^2
  kirTopoGPR <- matrix(0, nrow = max_depth/dz + 1, ncol = m)
  for( i in seq_len(m)){
    x_d <- (i-1)*dx   # diffraction
    z_seq <- seq(z[i],max_depth,by=dz)
#     mt <- (i - migTpl):(i + migTpl) # migration template
#     mt <- mt[mt > 0 & mt <= m]
#     l_migtpl <- length(mt)
    
    for(k in seq_along(z_seq)){
      z_d <- z_seq[k]
      t_0 <- 2*(z_d - z[i])/v    # = k * dts in reality
      z_idx <- round(z_d /dz + 1)
      # Fresnel zone
      # Pérez-Gracia et al. (2008) Horizontal resolution in a non-destructive
      # shallow GPR survey: An experimental evaluation. NDT & E International,
      # 41(8): 611–620.
      # doi:10.1016/j.ndteint.2008.06.002
      rf <- 0.5 * sqrt(lambda * 2 * (z_d - z[i]))
      rf_tr <- round(rf/dx)
      mt <- (i - rf_tr):(i + rf_tr)
      mt <- mt[mt > 0 & mt <= m]
      
      Ampl <- numeric(length(mt))
      for(j in mt){
        x_a <- (j-1)*dx
        t_top <-  t_0 - 2*(z[j] - z[i])/v
        t_x <- sqrt( t_top^2 +   4*(x_a - x_d)^2 /v2)
        t1 <- floor(t_x/dts) + 1 # the largest integers not greater
        t2 <- ceiling(t_x/dts) + 1 # smallest integers not less
        if(t2 <= n && t1 > 0 && t_x != 0){
          w <- ifelse(t1 != t2, abs((t1 - t_x)/(t1 - t2)), 0)
          # Dujardin & Bano amplitude factor weight: cos(alpha) = t_top/t_x
          # Ampl[j- mt[1] + 1] <- (t_top/t_x) * 
          # ((1-w)*A[t1,j] + w*A[t2,j])
          # http://sepwww.stanford.edu/public/docs/sep87/SEP087.Bevc.pdf
          Ampl[j- mt[1] + 1] <- (dx/sqrt(2*pi*t_x*v))*
                                (t_top/t_x) * 
                                ((1-w)*x[t1,j] + w*x[t2,j])
        }
      }
      kirTopoGPR[z_idx,i] <- sum(Ampl)#/l_migtpl
    }
  }
#   kirTopoGPR2 <- kirTopoGPR
  
  return(kirTopoGPR)
}

plotWig <- function(z, x = NULL, y = NULL, main ="", note=NULL,
          time_0 = 0, antsep = 1, v = 0.1,
          addFid = TRUE, fid = NULL,
          addAnn = TRUE, annotations = NULL, 
          depthunit="ns", posunit="m",
          topo=NULL, 
          pdfName=NULL, 
          yaxt = "s", bty = "o",
          col = "black", lwd = 0.5, ws = 1, side = 1, ratio = 1, 
          dx = 0.25, dz = 0.4, xlim = NULL, ylim = NULL, relTime0 = TRUE, ...){
  
  op <- par(no.readonly = TRUE) 
  dx <- mean(diff(x)) # estimated x-step
  z[is.na(z)] = 0
  z <- z/max(abs(z)) * dx
  # A =  as.matrix(A)/max(abs(A))
  # v <- ifelse(is.null(v),1,v/2)
  nr <- nrow(z)
  nc <- ncol(z)
  z <- z[nr:1,]
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
  if(grepl("[s]$",depthunit) && relTime0){
    y <- y + time_0
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
    mai=c(1,0.8,0.6,0.8)+0.02
    heightPDF <- fac*diff(ylim) + sum(omi[c(1,3)] + mai[c(1,3)])
    widthPDF <- fac*diff(xlim)*ratio +  sum(omi[c(2,4)]+ mai[c(2,4)])
  }else{
    mai=c(1,0.8,0.6,1.0)+0.02 
    heightPDF <- fac*(ylim[2] - ylim[1])*v/ 2 + sum(omi[c(1,3)] + mai[c(1,3)])
    widthPDF <- fac*(xlim[2] - xlim[1])*ratio + sum(omi[c(2,4)] + mai[c(2,4)])
  }
  if(!is.null(pdfName)){
    Cairo::CairoPDF(file = paste(pdfName,".pdf",sep=""),
        # pointsize=10,
        width = widthPDF, 
        height = heightPDF,
        # dpi=75,  # 75
        bg = "white",
        pointsize=10,
        # units = "in",
        title = pdfName)  
  }
  
  par(mai=mai,omi=omi,mgp=mgp)
      
  plot(0,0, type="n", xaxs="i", yaxs="i", yaxt="n",
      xlim=xlim, ylim=ylim, bty = "n", ...)
  usr <- par("usr")
  if(side==1){
    for(i in rev(seq_along(x))){
      y2 <- y + topo[i]
      wig = cbind(ws*z[,i]+x[i],y2)
      wig1 = rbind(c(x[i],y2[1]),wig,c(x[i],y2[nr]))
      polygon(wig1, col = col, border=NA)
      rect(min(wig1[,1]), ylim[1], x[i], ylim[2],col="white",border=NA)
      # lines(x[i]+ws*z[,i],y2,lwd=lwd)
    }
  }else{
    for(i in (seq_along(x))){
      y2 <- y + topo[i]
      wig = cbind(ws*z[,i]+x[i],y2)
      wig1 = rbind(c(x[i],y2[1]),wig,c(x[i],y2[nr]))
      polygon(wig1, col = col, border=NA)
      rect(max(wig1[,1]), ylim[1], x[i], ylim[2],col="white",border=NA)
    }
  }
  for(i in (seq_along(x))){
    y2 <- y + topo[i]
    lines(x[i]+ws*z[,i],y2,lwd=lwd)  
  }

  # plot fiducial markers
  if(!is.null(fid) && length(fid) > 0){
    .plotFid(fid[test], x[test])
  }
  
  # plot annotations
  testAnn <- FALSE
  if(addAnn && !is.null(annotations) && length(annotations) > 0){
    testAnn <- .plotAnn(annotations[test],x[test])
  }
  
  # plot title
  if(addAnn && testAnn){
    title(main,outer=TRUE,line=1)
  }else{
    title(main)  
  }
  
  # plot axis
#   axis(side=2, at=pretty_y + dusr/2, labels= -pretty_y)
#   dusr <- dylim/length(y)
  if( yaxt != "n"){
    pretty_y <- pretty(y ,10)
    axis(side=2, at=pretty_y, labels= -pretty_y)
    .depthAxis(y, pretty_y, time_0, v, antsep, depthunit, posunit )
  }
  
  if( bty != "n"){
    box(bty = bty)
  }
  if(!is.null(note) && length(note) > 0){
     mtext(note, side = 1, line = 4, cex=0.6)
  }
  
  if(!is.null(pdfName)){
    dev.off()
  }else{
    par(op)
    par("usr" = usr)
  }
}


# @relTime0 > boolean, y scale relative to time0? 0 <-> time0
# col, main, xlab, ylab, mar, barscale
#' @export
plotRaster <- function(z, x = NULL, y = NULL, main = "", note = NULL,
             time_0 = 0, antsep = 1, v = 0.1,
             addFid = TRUE, fid=NULL,
             addAnn = TRUE, annotations = NULL, 
             depthunit = "ns", posunit = "m",
             rasterImage = TRUE, resfac = 1, clab = "mV",
             add = FALSE, barscale = TRUE, addGrid = FALSE, 
             col = palGPR(n = 101), yaxt = "s", bty = "o",
             relTime0 = TRUE, ...){
  op <- par(no.readonly=TRUE)
  z =  as.matrix(z)
  z[is.na(z)]=0
  time_0 <- mean(time_0)
  zlim = c(-1, 1) * max(abs(z))
  xlim <- NULL
  mai <- op$mai
  if( length(list(...)) > 0 ){
    dots <- list(...)
    if( !is.null(dots$zlim)){
      zlim <- dots$zlim
    }
    if( !is.null(dots$xlim)){
      xlim <- dots$xlim
    }
  }
  #if(grepl("[s]$",depthunit)){

  if(barscale == FALSE){
    #colkeyVal <- NULL
    mai <- c(1.2, 1.2, 1.2, 1.2)
  }else{
    mai <- c(1.2, 1.2, 1.2, 1.8)
  }

  z <- t(z[nrow(z):1,])
  if(is.null(x)){
    x <- (1:nrow(z))
  }  
  if(is.null(y)){
    y <- -(ncol(z):1)
  }
  if(add == TRUE){ 
    par(new = TRUE)
  }else{
    par( mai = mai)
  }
  #cat("mai :", mai, "\n")
  #cat("oma :", oma, "\n")
  if(relTime0){
    y <- y + time_0
  }
  if( length(unique(diff(x))) > 1){
    rasterImage <- FALSE
  }
  #image(x,y,z,col=col,zlim=zlim,xaxs="i", yaxs="i", yaxt="n",...)
  plot3D::image2D(x = x, y = y, z = z, zlim = zlim, col = col, xaxs = "i", 
         yaxs = "i", yaxt = "n", rasterImage = rasterImage, clab = clab,
        resfac = resfac, main = "", bty = "n", colkey = FALSE, ...)  
  if(barscale){
    op2 <- par(no.readonly=TRUE)
    .barScale(zlim, y, col, collab=clab,collabcex=0.8)
   # plot3D::colkey(clim = zlim, clab = clab, width = 0.7, dist = 0.1, 
  #        add = TRUE, col = col)
    par(op2)
  }
  usr <- par("usr")
  if(is.null(xlim) ){
     test <- rep(TRUE,length(x))
  }else{
    test <- ( x >= xlim[1] & x <= xlim[2] )
  }
  
  # plot fiducial markers
  if(!is.null(fid) && length(fid) > 0){
    .plotFid(fid[test], x[test])
  }
  
  # plot annotations
  testAnn <- FALSE
  if(addAnn && !is.null(annotations) && length(annotations) > 0){
    testAnn <- .plotAnn(annotations[test],x[test])
  }
  
  # plot title
  if(addAnn && testAnn){
    title(main,outer=TRUE,line=1)
  }else{
    title(main)  
  }
  
  # plot axis
#   axis(side=2, at=pretty_y + dusr/2, labels= -pretty_y)
#   dusr <- dylim/length(y)
  if( yaxt != "n"){
    pretty_y <- pretty(y,10)
    axis(side=2, at=pretty_y, labels= -pretty_y)
    .depthAxis(y, pretty_y, time_0, v, antsep, depthunit, posunit )
  }
  # plot time0
  abline(h=0,col="red",lwd=0.5)
  
  # plot note
  if(!is.null(note) && length(note) > 0){
    mtext(note, side = 1, line = 4, cex=0.6)
  }
  
  # add grid
  if(addGrid){
    grid()
  }

  if( bty != "n"){
    box(bty = bty)
  }
  #op$usr <- usr
 # par(op)
 #par("usr" = usr)
}
#---

.plotAnn <- function(ann, x, line=1.7){
  if(length(ann)>0){
    testann <- (ann != "")
    if(sum(testann)>0){
      posann <- x
      ann <- gsub("#","\n",ann)
      abline(v=posann[testann],col="red",lwd=1)
      mtext(ann[testann], side = 3, line = line, at=posann[testann], 
            col="red", cex=0.9)
    }
    return(TRUE)
  }else{
    return(FALSE)
  }
}

.plotFid <- function(fid, x){
  usr <- par()$usr
  pin <- par()$pin  # inch
  if(!is.null(fid) && length(fid)>0 && any(fid!="")){
    cin <- par()$cin[2]
    posfid <- x
    testfid <- (fid != "")
    yr <- diff(usr[3:4])/(pin[2])
    if(sum(testfid)>0){  
      par(xpd=TRUE)
      cst <- yr*cin
      points(posfid[testfid],cst/2*0.75+rep(usr[4],sum(testfid)),pch=25,
            col="red",bg="yellow",cex=1)
      text(posfid[testfid],cst+rep(usr[4],sum(testfid)),fid[testfid],cex=0.6)
          #,pos=3,offset =0)
      par(xpd=FALSE)
    }
  }
}


.barScale <- function(zlim, y, col, collab="mV",collabcex=0.8){
  usr <- par()$usr
  pin <- par()$pin  # inch
  mai <- par()$mai
  dxin <- diff(usr[1:2])/(pin[1])
  dylim <- diff(usr[3:4])
  fin <- par()$fin
  # par(new=TRUE)
#   mai2 <- c(1, 0.8 + pin[1] + 1, 0.8, 0.8)
  mai2 <- c(par("mai")[1], par("mai")[1] + pin[1] + 1, par("mai")[3], 0.6)
  par(mai=mai2)
  fin2 <- par()$fin
  wstrip <- dxin*(fin2[1] - mai2[2] - mai2[4])/2
  xpos <- dxin*(mai2[2] - mai[2])
  zstrip <- matrix(seq(zlim[1],zlim[2],length.out=length(col)),nrow=1)
#   xstrip <- c( xpos,  xpos + wstrip*dxin)*c(0.97,1.03)
  xstrip <- c( xpos - 2*wstrip,  xpos + 2*wstrip)#*c(0.9, 1.1)
  ystrip <- seq(min(y),max(y),length.out=length(col))
  ystrip <- seq(usr[3],usr[4],length.out=length(col))
  pretty_z <- pretty(as.vector(zstrip))
  dzlim <- zlim[2]-zlim[1] 
  pretty_at <- usr[3] - dylim * (zlim[1] - pretty_z)/dzlim
  axis(side=4,las=2, at=pretty_at, labels=pretty_z)
 #  print(par("usr"))
  # print(range(xstrip))
  image(xstrip, ystrip, zstrip, zlim=zlim, add=TRUE, col=col, axes=FALSE, 
        xlab="", ylab="", xaxs="i", yaxs="i")
  # axis(side=4, las=2)
  title(main=collab, line =1, cex.main = 0.8)
  box()
}


.depthAxis <- function(y, pretty_y, time_0, v, antsep, depthunit, posunit ){
  if(grepl("[s]$",depthunit)){
#     depth <- (seq(0,by=2.5,max(abs(y))*v))
    depth <- pretty(seq(1.1,by=0.1,max(abs(y + 2*time_0))*v),10)
    depth2 <- seq(0.1,by=0.1,0.9)
    depthat <- depthToTime(depth, 0, v, antsep)
    depthat2 <- depthToTime(depth2,0, v, antsep)
    axis(side=4,at=-depthat, labels=depth,tck=-0.02)
    axis(side=4,at=-depthat2, labels=FALSE,tck=-0.01)
    axis(side=4,at= -1* depthToTime(1, 0, v, antsep), labels="1",tck=-0.02)
    mtext(paste0("depth (", depthunit, "),   v = ",v, " ", 
posunit,"/",depthunit),
          side=4, line=3)
  }else{
#     axis(side=4, at=pretty_y + dusr/2 , labels= -pretty_y)
    axis(side=4, at=pretty_y, labels= -pretty_y)
    mtext(paste0("depth (", depthunit, ")") ,side=4, line=3)
  }

}



# Jaun I. Sabbione and Danilo Velis (2010). Automatic first-breaks picking: 
# New strategies and algorithms. Geophysics, 75 (4): v67-v76
# -> modified Coppens's Method
# w = length leading window: about one period of the firs-arrival waveform
# ns = length eps (edge preserving smoothing) window: good results with ns 
# between one and two signal periods
#        -> default values ns= 1.5*w
# bet = stabilisation constant, not critical, set to 0.2*max(amplitude) 
.firstBreackPicking <- function(x, w = 11, ns = NULL, bet = 0.2){
  if(is.null(ns)){
    ns <- 1.5 * w
  }
  E1 <- c(wapply(x, width = w, by = 1, FUN = sum), rep(0, 2*floor(w/2)))
  E2 <- cumsum(x)
  Er <- E1/(E2 + bet)
  Er_fil <- .eps(Er, ns = ns)
  first_break <- which.max(abs(diff(Er_fil)))
  return(first_break)
}

# edge preserving smoothing
# luo et al. (2002): Edge preserving smoothing and applications: 
# The Leading edge, 21: 136-158
.eps <- function(x,ns){
  xmean <-  c(rep(0,floor(ns/2)), 
              wapply(x,width=ns,by=1, FUN=mean),rep(0,floor(ns/2)))
  xsd <- c(rep(0,floor(ns/2)), 
wapply(x,width=ns,by=1,FUN=sd),rep(0,floor(ns/2)))
  xtest <- wapply(xsd,width=ns,by=1,FUN=which.min) + 
            (0):(length(xmean)- 2*floor(ns/2)-1)
  return(c(rep(0,floor(ns/2)), xmean[xtest],rep(0,floor(ns/2))))
}

#==============================#
#========== SPATIAL FILTER =========#
.medianFilter3x3 <- function(A){
  B <- A  # <- matrix(0, 364,364)
  for(i in 1:(nrow(A)-2)) {
       for(j in 1:(ncol(A)-2) ) {
          xm <- A[i+0:2, j+0:2]
             B[i+1, j+1] <- xm[order(xm)[5]]
    }
  }  
  return(B)
}

.medianFilter1D <- function(a,w){
  b <- a  # <- matrix(0, 364,364)
  for(i in (w+1):(length(a)-w-1)){
    xm <- a[i+(-w:w)]
    b[i] <- xm[order(xm)[w+1]]
  }
  return(b)
}

#==============================#
#======= GAIN FUNCTIONS ========#
# dts = sampling time (e.g., 0.8 ns)
# t0 = starting time to apply the gain scaling
# te = ending time to apply the gain scaling
# tcst
# CF Yilmaz, p85
.gainPower <- function(A, alpha, dts, t0 = NULL, te = NULL,
                       tcst = NULL){
  g <- .gainPower0(A[,1], alpha, dts, t0, te, tcst)
  Anew <- (A)*g
  s1 = ((max(A))-(min(A)));  # scale factor
  s2 = ((max(Anew))-(min(Anew)));  # scale factor
  return(Anew/s2*s1 )
}

.gainPower0 <- function(d, alpha, dts, t0 = NULL, te = NULL, tcst = NULL){
  if(is.null(t0)) t0 <-0
  if(is.null(te)) te <-(length(d)-1)*dts
  if(!is.null(tcst) && !(tcst > t0 && tcst < te)){
    stop("you need tcst > t0 && tcst < te\n")
  }
  x <- (seq_along(d) - 1) *dts
  test <- x >= t0 & x <= te
  g <- rep(1L,length(d))
  g[test] <- 1 + (seq_along(d[test])*dts )^alpha
  g[x > te] <- max(g)
  if(!is.null(tcst) && any(x < tcst)) g[x < tcst] <- g[1+round(tcst/dts)]
  return( g)
}

.gainExp <- function(A, alpha, dts, t0 = NULL, te = NULL){
  g <- .gainExp0(A[,1], alpha, dts, t0, te)
  Anew <- (A)*g
  s1 = ((max(A))-(min(A)));  # scale factor
  s2 = ((max(Anew))-(min(Anew)));  # scale factor
  s12 <- s1/s2
  A3 <- (Anew * s12)
  return(  Anew)
}

.gainExp0 <- function(d, alpha, dts, t0 = NULL, te = NULL){
  if(is.null(t0) || t0==0) t0 <-0
  if(is.null(te)) te <-(length(d)-1)*dts
  x <- (seq_along(d) - 1) * dts
  test <- (x >= t0 & x <= te)
  test_max <- x > te
  g <- rep(1L,length(d))
  g[test] <-  exp((x[test] - t0) * dts * alpha)
  g[test_max] <-  exp(max(x[test] - t0)*dts*alpha)
  return( g)
}


.gainAgc <- function(A, dts, w = 10, p = 2, r = 0.5){
  w <- w/dts
  Anew <- apply(A,2,.gainAgc0,w,p,r)
  s1 = ((max(A))-(min(A)));  # scale factor
  s2 = ((max(Anew))-(min(Anew)));  # scale factor
  return(Anew * s1/s2)
}

.gainAgc0 <- function(d, w = 10, p = 2 , r = 0.5){
  # convert NA into 0
  d[is.na(d)] <- 0
  # Get local mean by smoothing the image with a Gaussian filter
  dAmp   <- mmand::gaussianSmooth(d, w)
  # Subtract image from local mean, raise to power 'p' then apply Gaussian
  # smoothing filter to obtain a local weighted sum. 
  # Finally raise the result
  # to power 'r' to obtain the 'gain'.  Typically p = 2 and r = 0.5 which will
  # make gain equal to the local RMS.  The abs() function is used to allow
  # for arbitrary 'p' and 'r'.
  dGain <- (mmand::gaussianSmooth(abs(d - dAmp)^p, w))^r
  # Apply inverse gain to the difference between the image and the local
  # mean to obtain the final AGC image. 
  dnew <- d/dGain
  return(dnew)
}


#=============================================#
#======== CLIP/GAMMA/NORMALIZE ================#


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

.rms <- function(num) sqrt(sum(num^2)/length(num))

scaleCol <- function(A,type = c("stat","min-max","95","eq","sum", "rms")){
  A =  as.matrix(A)
  type = match.arg(type)
  if(type == "stat"){
    Anorm <- scale(A, center=.colMeans(A, nrow(A), ncol(A)), 
              scale = apply(A, 2, sd, na.rm = TRUE))
  }else if(type == "sum"){
    Anorm <- scale(A, center=FALSE, scale=colSums(abs(A)))
  }else if(type == "eq"){
    # equalize line such each trace has same value for 
    # sqrt(\int  (A(t))^2 dt)
    amp <- apply((A)^2,2,sum)
    Anorm <- A * sqrt(amp)/sum(sqrt(amp))
  }else if(type == "95"){
    A_q95 = (apply((A),2,quantile,0.99,na.rm=TRUE))
    A_q05 = (apply((A),2,quantile,0.01,na.rm=TRUE))
    Anorm = (A)/(A_q95-A_q05)
  }else if(type == "rms"){
     Anorm <- A/apply(A ,2, .rms)
  }else{  # min-max
    Anorm <- scale(A, center=FALSE, scale=(apply((A),2,max,na.rm=TRUE)) - 
                    (apply(( A),2,min,na.rm=TRUE)))
  }
  return(Anorm)
}

rmsScaling <- function(...){
  stop("Deprecated! Use instead 'traceScaling(x,type=\"rms\")'\n")
}

#=============================================#
#======== SPECTRAL FUNCTIONS ================#


# @param [matrix]/[vector]   A     (each column represent a trace / a trace)
# @param [double]       dT     (sampling time in nanoseconde)  
# @param [double]      fac    (result multiplied by a factor, e.g. to get MHz 
# instead of Hz)

# @return power spectrum (frequency, power, phase)
# -------------------------------------------
powSpec <- function(A, dT = 0.8, fac = 1000000, plotSpec = TRUE, 
                   titleSpec = NULL, unwrapPhase = TRUE){
  A   <- as.matrix(A)
  nr  <- nrow(A)
  nc  <- ncol(A)
  N   <- 2^(ceiling(log2(nr)))
  A   <- rbind(A,matrix(0,nrow=N-nr,ncol=nc))

  # samping interval GPR = 0.8 ns
  Ts    <- dT*(10^(-9))     # [s] Sample time
  Fs    <- 1/Ts             # [Hz] Sampling frequency
  Fc    <- 1/(2*Ts)         # Nyquist frequency
  nfreq <- N/2 + 1
  
  # if y <- fft(z), then z is 
  # fft(y, inverse = TRUE) / length(y).
  # each column = discrete Fourier transform. 
  fft_A <- mvfft(A)
  # extract the power spectrum (sometimes referred to as "magnitude")
  pow <- as.matrix(Mod(fft_A))
  pow <- pow[1:nfreq,,drop=FALSE]   # select only first half
  # extract the phase which is atan(Im(fourier)/Re(fourier))
  pha <- as.matrix(Arg(fft_A))
  pha <- pha[1:nfreq,,drop=FALSE]    # # select only first half

  pow_mean <- apply(pow, 1, mean, na.rm = TRUE)
  if(unwrapPhase){
    pha <- apply(pha, 2, signal::unwrap)
  }
  pha_mean <- apply(pha, 1, mean, na.rm = TRUE)

  # frequenceS
  fre = Fs*seq(0, N/2)/N/fac
  
  # plot the power spectrum
  if(plotSpec){
    op <- par(no.readonly=TRUE)
    m = seq(0, 10000, by = 50)
    par(mfrow=c(2,1))
    par(mar=c(0, 4, 4, 2) + 0.1, oma=c(1,1,1,1) )
    plot(fre,pow_mean, type="n",
#           xaxt = "n",
          ylim=c(0,max(pow)),
          ylab="amplitude",xlab="")
      if(!is.null(dim(A))){
        invisible( apply(pow, 2, lines, x = fre, 
                col=rgb(0.2,0.2,0.2,7/max(ncol(A),7))) )
      }
      lines(fre,pow_mean,col="red")
#       Axis(side = 1, tcl = +0.3,  labels=FALSE ,at=m)
      if(!is.null(titleSpec)){
        title(titleSpec)
      }
    par(mar=c(4, 4, 0.3, 2))
    plot(fre,pha_mean, type="n", 
#           xaxt = "n",
          ylim=range(pha), 
          xlab = "frequency MHz", ylab="phase") 
      if(!is.null(dim(A))){
        invisible(  apply(pha, 2, lines, x = fre, 
                  col = rgb(0.2,0.2,0.2,7/max(ncol(A), 7))) )
      }
      lines(fre,pha_mean,col="red")
#       Axis(side = 1, tcl = +0.3,  labels=m ,at=m)
    par(op)
  }
  return(list(freq = fre, pow = pow, pha = pha))
}

# @param [matrix]/[vector]   A     (each column represent a trace / a trace)
# @param [double]       dT     (sampling time in nanoseconde)  


# @return power spectrum (frequency, power, phase)
# -------------------------------------------

.fFilter1D <- function(A, f = c(100), type = c('low', 'high', 'bandpass'), 
                        L = 257, dT = 0.8, plotSpec = FALSE, fac = 1000000){
  type <- match.arg(type)
  A <- as.matrix(A)
  nr <- nrow(A)      # signal length

  # samping interval GPR = 0.8 ns
  Ts    <- dT*(10^(-9))     # [s] Sample time
  Fs    <- 1/Ts             # [Hz] Sampling frequency
  
  # cut-off frequency/ies fc in (MHz)
  f <- sort(f) * 10^6    # cut-off frequency in Hz
  
  # FIXME > write a function setFFilter(f, type=..., Fs)
  if(type == "low" || type == "high"){
    # Design the filter using the window method:
    if(length(f)>1) {
        BW <- (f[2] - f[1])/Fs          # filter bandwidth
        fc <- f[1] + (f[2] - f[1])/2    # cut frequency
        L <- round(4 / BW)
        if(L %% 2 == 0){
          L <- L + 1  
        }
    }else if(length(f)==1){
      fc <- f[1]
    }
    h <- winSincKernel(L, fc/Fs, type)
  }else if(type == "bandpass"){
    if(length(f)==2 ) {
      h1 <- winSincKernel(L, f[1]/Fs, "low")
      h2 <- winSincKernel(L, f[2]/Fs, "high")
    }else if(length(f) == 4 ){
      BW <- (f[2] - f[1])/Fs
      fc <- f[1] + (f[2] - f[1])/2
      L <- round(4 / BW)
      if(L %% 2 == 0){
        L <- L + 1
      }
      h1 <- winSincKernel(L, fc/Fs, "low")
      BW <- (f[4] - f[3])/Fs
      fc <- f[3] + (f[4] - f[3])/2
      L <- round(4 / BW)
      if(L %% 2 == 0){
        L <- L + 1
      }
      h2 <- winSincKernel(L, fc/Fs, "high")
    }
    L = max(length(h1),length(h2))
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
  
  # L <- length(h)
  
  # Choose the next power of 2 greater than L+nr-1 
  Nfft = 2^(ceiling(log2(L + nr-1)))
  # Zero pad the signal and impulse response:
  h_long = c( h, rep(0, Nfft - L) )
  A = rbind(as.matrix(A) , matrix(0,nrow=Nfft-nr,ncol=ncol(A)) )

  fft_A = mvfft(A)    # signal
  fft_h = fft(h_long)        # filter

  # Now we perform cyclic convolution in the time domain using 
  # pointwise multiplication in the frequency domain:
  Y = fft_A * fft_h

  pow_A = Mod(fft_A)
  pow_h = Mod(fft_h)
  pow_y = Mod(Y)
  # si matrix -> moyenne sur les colonnes
  if(!is.null(dim(A))){
    pow_A = apply(pow_A,1, mean, na.rm=TRUE)
    pow_y = apply(pow_y,1, mean, na.rm=TRUE)
  }
  # select only first half of vectors
  pow_A = pow_A[1:(Nfft/2+1)] 
  pow_y = pow_y[1:(Nfft/2+1)] 
  pow_h = pow_h[1:(Nfft/2+1)] 

  fre = Fs*(0:(Nfft/2))/Nfft/fac  #[MHz]
  
  if(plotSpec == TRUE){
    op <- par(no.readonly=TRUE)
    # plot the power spectrum
    m = seq(0,900,by=50)
    #par(mfrow=c(2,1), mar=c(5, 4, 4, 6) + 0.1 )
    par( mar=c(0, 4, 0.3, 2) + 0.1, oma=c(3,2,1,2) )
    plot(fre,pow_A, type="l",
#         xaxt = "n",
        #  yaxt = "n", 
          ylim=c(0,max(pow_A,pow_y)),
          ylab="power",lwd=2)
      lines(fre,pow_y, type="l",col="blue",lwd=2)
#       Axis(side = 1, tcl = +0.3,  labels=m ,at=m)
      par(new=TRUE)
      plot(fre,pow_h,type="l", col="red",
        yaxt = "n",
        ylab="")
        legend("topright",c("input signal","filter","filtered signal"),
              col = c("black", "red", "blue"), lwd=c(2,1,2),bg = "white")
      abline(v=f/1000000, col="grey",lty=2)
     par(op)
  }
  a = (L-1)/2
  y = mvfft(Y, inverse = TRUE)
  y = y[a:(a+nr-1),]/nrow(y)
  return(Re(y))
}

winSincKernel <- function(L,f,type=c("low","high")){
  type = match.arg(type)  # if L is even (because L - filter length - 
#                             must be odd)
  x = (-(L-1)/2):((L-1)/2)
  # low-pass
  h = hammingWindow(L) * sincMod(x,2*pi*f)  # h is our filter
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
  # Nfft = 2^(ceiling(log2(L+M-1)))  # -1)))    # or 2^nextpow2(L+M-1)
nextpower2 <- function(x){
  return(2^(ceiling(log2(x))))
}

# shift the phase of signal by phi (in radian)
      
#' @export
phaseRotation <- function(x,phi){
  nf <- length(x)
  X <- fft(x)
  phi2 <- numeric(nf)
  phi2[2:(nf/2)] <- phi
  phi2[(nf/2+1):(nf)] <- -phi
  Phase <- exp(-complex(imaginary=-1)*phi2)
  xcor <- fft(X*Phase, inverse=TRUE)/nf
  return(Re(xcor))
}



# -------------------------------------------
# ------------addProfile3D--------------------------
# -------------------------------------------
# @name  addProfile3D (plot/add a 2D profile in rgl)
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

# @param [list]   LINE       (list containing several fPath of the DT1 file)
# @param [c(1)]   col=NULL   (palette of color)  
# @param [boolean]   plotNew=FALSE  (if true, open a new rgl window)
# @return void
# -------------------------------------------



# plot/add a 2D profile in rgl

# addProfile3D <- function(LINES, col=palGPR(n=101),plotNew=FALSE, normalize=TRU
# E, v=1, zlim=NULL, AGC=FALSE, sig=10){
  # if(plotNew){
    # # rgl.open()
    # open3d()
  # }
  # for(i in seq_along(LINES)){
    # #------------- READ DATA ------------------#
    # lineName2   <- strsplit(LINES,split="[.]")
    # lineName   <- lineName2[[i]][1]
    # fileNameHD   <- paste(lineName,".HD",sep="")
    # fileNameDT1 <- paste(lineName,".DT1",sep="")
    # cat(basename(lineName),"\n")
    # GPR <- readDT1(LINES[[i]])
    # #------------- read data ------------------#

    # myGPRdZ <- as.numeric(as.character(GPR$hd[7,2]))/as.numeric(as.character(
# GPR$hd[5,2]))
    # HD <- GPR$dt1hd
    
    # A <- GPR$data
    # A[is.na(A)] <- 0
    # if(!is.null(zlim)){
      # sel <- seq(1, zlim/myGPRdZ/v,by=myGPRdZ)
      # A <- A[sel,]
    # }
    # if(normalize){
      # A <- normalizeGPR(A)
    # }
    # if(AGC){
      # A <- apply(A,2,gain,sig=sig)
    # }
     # # example with GPR profile A 
    # nr = nrow(A)
    # nc = ncol(A)
    # X <- matrix(HD$recx, ncol=nc, nrow=nr, byrow=TRUE)
    # Y <- matrix(HD$recy, ncol=nc, nrow=nr, byrow=TRUE)
    # Z <-  matrix(HD$topo, ncol=nc, nrow=nr, byrow=TRUE) - 
# matrix(myGPRdZ*v*(0:(
# nr-1)), ncol=nc, nrow=nr, byrow=FALSE)
    # if(all(HD$topo==0)){
      # warning("No topography \n")
    # }
    # if(all(HD$recx==0)){
      # warning("No x-coordinates \n")
    # }
    # if(all(HD$recy==0)){
      # warning("No y-coordinates \n")
    # }
    # A = (A-min(A))/(max(A)-min(A))
    # Alim <- range(A)
    # Alen <- Alim[2] - Alim[1] + 1
    
    # # if(is.null(col))     col <- tim.colors(101)  # height color lookup table

    # colA <- col[ (A)*100+1 ] # assign colors to heights for each point 
    # rgl.surface(X, Y, Z, color=colA, back="fill", smooth = TRUE, lit=FALSE, 
# lwd=0) 
    # # surface3d(X, Y, Z, color=colA, back="fill", smooth = FALSE, lit=FALSE, 
# lwd=0) 
  # }
# }




#-------------------------------------
#------- PRIVAT FUNCTION --------#
byte2volt <- function ( V=c(-50,50), nBytes = 16) {
  abs(diff(V))/(2^nBytes)
}




.minCommon10 <- function(xmin,xmax){
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



 
.upsample <- function(A, n=c(2,1), type=c("DFT","bicubic")){
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
    nr <- nrow(A)  # time  
    nc <- ncol(A)  # x  

    nk <- (nextpower2(nc))
    nf <- (nextpower2(nr))
    A1 <- matrix(0,nrow=nf,ncol=nk)
    A1[1:nr,1:nc] <- A
    A1_fft <- fft(A1)
    
    A_fftint <- matrix(0,nrow=n[1]*nf,ncol=n[2]*nk)
    A_fftint[1:(nf/2),1:(nk/2)] <- A1_fft[1:(nf/2),1:(nk/2)]
    A_fftint[((n[1]-1)*nf + nf/2+1):(n[1]*nf),
             ((n[2]-1)*nk + nk/2 + 1):(n[2]*nk)] <- 
                  A1_fft[(nf/2+1):(nf),(nk/2 + 1):nk]
    A_fftint[1:(nf/2),
             ((n[2]-1)*nk + nk/2 + 1):(n[2]*nk)] <- 
                  A1_fft[1:(nf/2),(nk/2 + 1):nk]
    A_fftint[((n[1]-1)*nf + nf/2+1):(n[1]*nf),
              1:(nk/2)] <- 
          A1_fft[(nf/2+1):(nf),1:(nk/2)]
    
    A_int = fft(A_fftint, inverse = TRUE)
    A_int <- A_int[1:(n[1]*nr),1:(n[2]*nc)]/(nk*nf)
  }else if(is.vector(A)){
    # FTA = fft(A);
    n_A = length(A)
    # Choose the next power of 2 greater than L+M-1 
    Nfft = 2^(ceiling(log2(n_A)))    # 
    # Zero pad the signal and impulse response:
    A0 = c( A, rep(0,Nfft-n_A) )
    n_A0 <- length(A0)
    
    FTA <- fft(A0)
    
    # % now insert enough zeros into the dft to match the desired density 'n'
    FTA = c(FTA[1:(n_A0/2)], rep.int(0,floor((n[1]-1)*n_A0)), 
                                    FTA[(n_A0/2+1):n_A0])

    A_int = fft(FTA, inverse = TRUE)
    A_int <- A_int[1:(n_A * (n[1]))]/n_A0
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
# A convolution matrix is a matrix, formed from a vector, 
# whose product with another vector 
# is the convolution of the two vectors.

# A = convmtx(y,nf) returns the convolution matrix, A, 
# such that the product of A and a vector, x, 
# is the convolution of y and x. 
# If y is a column vector of length m, A is (m+nf-1)-by-nf and the 
# product of A and a column vector, x, of length n is the 
# convolution of y and x. 
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
deconvolve <- function(y,h,mu=0.0001){
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
deconvolutionMtx <- function(y,h,nf,mu=0.0001){
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



# setGenericVerif("rmsScaling", function(x) standardGeneric("rmsScaling"))
#' Optimum Phase Rotation
#'
#' @name optPhaseRotation
#' @rdname optPhaseRotation
#' @export
optPhaseRotation <- function(gpr,rot=0.01,plot=TRUE){
  # x_dec <- as.vector(gpr/apply(as.matrix(gpr),2,RMS))
  x_dec <- as.vector(gpr)
  pi_seq <- seq(0,pi,by=rot)
  kurt <- numeric(length(pi_seq))
  nx <- length(x_dec)
  for(i in seq_along(pi_seq)){
    xrot <- phaseRotation(x_dec, pi_seq[i])
    # xrot_scaled2 <- (xrot -   mean(xrot))^2
    # kurt[i] <- ((1/nx) * sum( xrot_scaled2^2)) / 
    # ( (1/nx) *sum( xrot_scaled2))^2 
    kurt[i] <- e1071::kurtosis( xrot)
  }
  phi_max <- pi_seq[which.max(kurt)]
  cat("rotation angle =",phi_max/pi*180, "degree\n",sep="")
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
#     algorithm and also returned
# if shift is not NULL, case of wavelet estimation
#     from the trace via the autocorrelation matrix.
# mu = percent of pre-whitening
.spikingFilter <- function(y,nf=32,mu=0.1,shft=1){
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







# version vectoriel!!!!
inPoly <- function(x, y, vertx, verty){
  inPo <- rep(0L, length(x))
  nvert <- length(vertx)
  for(i in 1:nvert){
    j <- ifelse(i==1, nvert,i-1)
    myTest <- ((verty[i] > y) != (verty[j]>y)) &
                (x < (vertx[j]-vertx[i]) * (y-verty[i]) / 
                (verty[j]-verty[i]) + vertx[i])
    inPo[myTest] <- !inPo[myTest]
  }
  return(inPo)
}

.FKSpectrum <- function(A, dx = 0.25, dz = 0.8, npad = 1, 
                        p = 0.01, plotSpec = TRUE){
  # A <- GPR$data    #[90:1000,]
  nr <- nrow(A)  # time  
  nc <- ncol(A)  # x  

  #============== PLOT F-K SPECTRUM ===============#
  # padding (try also 2*(2^nextpow2(nc))
  nk <- npad*(nextpower2(nc))
  nf <- npad*(nextpower2(nr))
  A1 <- matrix(0,nrow=nf,ncol=nk)
  A1[1:nr,1:nc] <- A

  # function to center the spectrum!! (no need of fttshift!)
  #centres spectrum: Gonzalez & Wintz (1977) Digital Image Processing p.53
  A1  <- A1 * (-1)^(row(A1) + col(A1))
  A1_fft <- fft(A1)
  A1_fft_pow <- Mod(A1_fft)
  A1_fft_phase <- Arg(A1_fft)
  # plotGPR((A1_fft_phase[1:(nf/2),])^0.05)

  # Sampling frequency [Hz] = 1 / Sample time [s]
  Fs = 1/(dz*10^(-9))
  fac = 1000000
  fre = Fs*seq(0,nf/2)/nf/fac
  
  # wavenumber
  Ks <- 1/dx      # [1/m] Sampling frequency
  knu <- 1:(nk/2)/(2*(nk/2)) * Ks  #[1/m]
  knutot <- c(-rev(knu),knu)

  # labels: find a function between "xat" and "xLabels" and use "pretty()"
  xat   <- c(0,nk/2,nk)/nk
  xLabels <- c(min(knutot), 0, max(knutot))
  yat    <- c(0,nf/2,nf)/nf
  yLabels  <- c(0, max(fre)/2, max(fre))

  # Note: when plotting spectra (S)  use log(S) or S.^alpha (alpha=0.1-0.3) to
  #       increase the visibility of small events 
  # p = 0.05
  if(plotSpec){
    plot3D::image2D(x = knutot, y = fre, z = (t(A1_fft_pow[1:(nf/2),])^p), 
                xlab="wavenumber (1/m)",
                ylab="frequency MHz")
#      axis(side=4, labels=TRUE)

  }
   return(list(pow=A1_fft_pow[1:(nf/2),], 
               pha=A1_fft_phase[1:(nf/2),],
               fre = fre,
               wnb = knutot))
}




.FKFilter <- function(A, fk, L = c(5, 5), npad=1){
  nr <- nrow(A)  # time  
  nc <- ncol(A)  # x  

  #============== PLOT F-K SPECTRUM ===============#
  # padding (try also 2*(2^nextpow2(nc))
  nk <- npad*(nextpower2(nc))
  nf <- npad*(nextpower2(nr))
  A1 <- matrix(0,nrow=nf,ncol=nk)
  A1[1:nr,1:nc] <- A

  # function to center the spectrum!! (no need of fttshift!)
  #centres spectrum: Gonzalez & Wintz (1977) Digital Image Processing p.53
  # A1  <- A1 * (-1)^(row(A1) + col(A1))
  A1_fft <- fft(A1)
  
  # plotGPR(Mod(A1_fft)^0.05)
  # plotGPR(Re(fft(A1_fft,inv=TRUE))[1:nr,1:nc])
  # plotGPR(A)
  
  #============== FILTER F-K SPECTRUM ===============#
  myFlong <- matrix(0,nrow=nf,ncol=nk)
  myFlong[1:(nf/2),1:(nk/2)] <- fk[(nf/2):1,(nk/2):1]
  # myFlong  <- myFlong * (-1)^(row(myFlong) + col(myFlong))
  myFlong[(nf/2+1):(nf),(nk/2 + 1):nk] <- fk[1:(nf/2),1:(nk/2)]
  myFlong[1:(nf/2),(nk/2 + 1):nk] <- fk[(nf/2):1,(nk):(nk/2 + 1)]
  # myFlong[(nf/2+1):(nf),1:(nk/2)] <- fk[1:(nf/2),(nk/2 + 1):(nk)]
  myFlong[(nf/2+1):(nf),1:(nk/2)] <- fk[1:(nf/2),(nk/2 + 1):nk]
  # plotGPR(myFlong)


  # hamming window
  if(length(L)==1) L <- c(L,L)
  if(all(L!=0)){
    ham2D <- hammingWindow(L[1])%*%t(hammingWindow(L[2]))
    ham2Dlong <- matrix(0,nrow=nf,ncol=nk)
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
# @name  localOrientation (Compute the local orientation)
# @description Compute the local orientation based on the tensor of the
# gradient field and on SVD decomposition.

# @date 04.09.2012 12:45
# @auteur Emanuel Huber

# @require source("convolution2D.R") -> tim.colors + image.plot
  # source('convolution2D.R')
  # cat('> Function(s) loaded: "convolution2D.R" \n')

# @return void
# -------------------------------------------


# return structure tensor
#------------------------------
#' Structure tensor of GPR data
#' 
#' @name strucTensor
#' @rdname strucTensor
#' @export
strucTensor <- function(P, winBlur = c(3,3), winEdge=c(7,7), 
      winTensor = c(5,10), sdTensor=2, ...){
  n <- nrow(P)
  m <- ncol(P)
  
  #-------------------------------------------------
  #- Identify ridge-like regions and normalise image
  #-------------------------------------------------
  # normalization (mean = 0, sd = 1)
  P <- (P-mean(P, na.rm=TRUE))/sd(as.vector(P), na.rm=TRUE)

  #------------------------------
  #- Determine ridge orientations
  #------------------------------
  #  BLURING
  if(!is.null(winBlur)){
    if(length(winBlur) == 1){
      winBlur <- c(winBlur, winBlur)
    }
    k = matrix(1, nrow = winBlur[1], ncol = winBlur[2], byrow = TRUE)/
                (winBlur[1] * winBlur[2])
    P_f  = convolution2D(P, k , 0)
  }else{
    P_f <- P
  }

  # GRADIENT FIELD
  # window size for edge dectection
  if(length(winEdge) == 1){
      winEdge <- c(winEdge, winEdge)
    }
  vx <- convolution2D(P_f, dx_gkernel(winEdge[1], winEdge[2], 1), 0)
  vy <- convolution2D(P_f, dy_gkernel(winEdge[1], winEdge[2], 1), 0)

  # local TENSOR
  Gxx <- vx^2
  Gyy <- vy^2
  Gxy <- vx*vy 
    
  # LOCAL AVERAGED TENSOR
  #sze = 5 *2
  Jxx  <- convolution2D(Gxx, gkernel(winTensor[1],winTensor[2],sdTensor), 0)
  Jyy  <- convolution2D(Gyy, gkernel(winTensor[1],winTensor[2],sdTensor), 0)
  Jxy  <- convolution2D(Gxy, gkernel(winTensor[1],winTensor[2],sdTensor), 0)

  # ANALYTIC SOLUTION BASED ON SVD DECOMPOSITION
  A1 <- 0.5 * sqrt((Jxx - Jyy)^2 + 4*(Jxy^2))
  A2 <- 0.5 * (Jxx - Jyy)
  u1x <- A2 + A1
  u1y <- Jxy
  u2x <- A2 - A1
  u2y <- Jxy
  
  lambda1 <- 0.5 * (Jxx + Jyy) + A1
#   lambda1 = (Jxx + Jyy + sqrt((Jxx - Jyy)^2 + 4*(Jxy)^2))/2
  lambda2 <- 0.5 * (Jxx + Jyy) - A1
#   lambda2 = (Jxx + Jyy - sqrt((Jxx - Jyy)^2 + 4*(Jxy)^2))/2
  
  # polar parametrisation
  o_alpha <- Jxx + Jyy                               # energy
  o_beta  <- sqrt((Jxx-Jyy)^2 + 4*(Jxy)^2)/o_alpha   # anisotropy
  o_theta <- 1/2*atan2(2*Jxy,(Jxx - Jyy)) + pi/2     # orientation
  
  return(list(tensor  = list("xx" = Jxx,
                             "yy" = Jyy,
                             "xy" = Jxy),
              vectors = list("u1x" = u1x,
                             "u1y" = u1y,
                             "u2x" = u2x,
                             "u2y" = u2y),
              values  = list("lambda1" = lambda1,
                             "lambda2" = lambda2),
              polar   = list("energy" = o_alpha,
                             "anisotropy" = o_beta,
                             "orientation" = o_theta)))
}

localOrientation <- function(P, blksze=c(5,10), thresh=0.1, winEdge=c(7,7), 
winBlur = c(3,3), winTensor = c(5,10), sdTensor=2, ...){
  
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
  #P =  P[1:floor(n/nnx)*nnx,1:floor(m/nny)*nny]
  
  # IMAGE SMOOTHING
  # Window size for bluring
  if(!is.null(winBlur)){
    blurWinX = winBlur[1]
    blurWinY = winBlur[2]
    k = matrix(1, nrow = blurWinX, ncol = blurWinY, byrow = TRUE)/
                (blurWinX * blurWinY)
    P_f  = convolution2D(P, k , 0)
  }else{
    P_f <- P
  }

  # GRADIENT FIELD
  # window size for edge dectection
  nnx = winEdge[1]
  nny = winEdge[2]
  vx = convolution2D(P_f, dx_gkernel(nnx,nny,1), 0)
  vy = convolution2D(P_f, dy_gkernel(nnx,nny,1), 0)

#   image(t(vx),col=gray(seq(0,1,len = 250)))  
#   image(t(vy),col=gray(seq(0,1,len = 250)))  
    
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
  o_alpha = Jxx + Jyy                  # energy
  o_beta  = sqrt((Jxx-Jyy)^2 + 4*(Jxy)^2)/o_alpha    # anisotropy
  o_theta = 1/2*atan2(2*Jxy,(Jxx - Jyy)) + pi/2    # orientation
  o_lambda1 = (Jxx + Jyy + sqrt((Jxx - Jyy)^2 + 4*(Jxy)^2))/2
  o_lambda2 = (Jxx + Jyy - sqrt((Jxx - Jyy)^2 + 4*(Jxy)^2))/2
  
  return(list(energy = o_alpha, anisotropy = o_beta, orientation = o_theta, 
lambda1 = o_lambda1, lambda2 = o_lambda2 ))

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

#' Two-dimensional convolution
#' 
#' The convolution is performed with 2D FFT
#' @name convolution2D
#' @rdname convolution2D
#' @export
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
  g <- Re(fft(fft(k0)*fft(h0),inverse=TRUE))/(nL * mL)
  g2 <- g[nk-1 + 1:nh, mk-1 + 1:mh]
  # g2 <- g[nk + 1:nh, mk + 1:mh]
  return(g2)
}
# pads the edges of an image to minimize edge effects 
# %during convolutions and Fourier transforms. 
# %Inputs %I - image to pad 
# %p - size of padding around image 
# %Output %Ipad - padded image 
# SOURCE: http://matlabgeeks.com/tips-tutorials/how-to-blur-an-image-with-a-
# fourier-transform-in-matlab-part-i/
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


###############################################################################
# Source:
# A replication of MatLab repmat function!
# R FOR OCTAVE USERS
# version 0.4
# Copyright (C) 2001 Robin Hankin
# http://cran.r-project.org/doc/contrib/R-and-octave.txt
################################################################################
repmat <- function(a,n,m) {kronecker(matrix(1,n,m),a)}


# -------------------------------------------
# ------------readDT1--------------------------
# -------------------------------------------
# @name  readDT1 (plot Ground Penetrating Radar image)
# @description This function read *.HD and associated *.DT1
# files from Sensors & Software.

# @date 30.04.2014 08:33
# @auteur Emanuel Huber
# @param [text]    fPath       (file path of *.hd or *.dt1 file)
# @require source("trimStr.R")
  # source('trimStr.R')
  # cat('> Function(s) loaded: "trimStr.R" \n')
# @return list((hd = headerHD, dt1hd = headerDT1, data=myData))
# -------------------------------------------

readDT1 <- function( fPath){
  dirName   <- dirname(fPath)
  splitBaseName <- unlist(strsplit(basename(fPath),'[.]'))
  baseName   <- paste(splitBaseName[1:(length(splitBaseName)-1)],sep="")
  
  fileNameHD   <- paste(dirName, "/",baseName,".HD",sep="")
  fileNameDT1  <- paste(dirName, "/",baseName,".DT1",sep="")
  
  headHD <-  scan(fileNameHD, what=character(),strip.white=TRUE,quiet=TRUE,
fill=TRUE,blank.lines.skip=TRUE,flush=TRUE,sep="\n")
  nHD <- length(headHD)
  headerHD <- data.frame(nrow=nHD,ncol=2)
  for(i in seq_along(headHD)){
    hdline <- strsplit(headHD[i],"=")[[1]]
    if(length(hdline) < 2){
      headerHD[i,1] <- ""
      headerHD[i,2] <- trimStr(hdline[1])
    }else{
      headerHD[i,1:2] <-  as.character(sapply(hdline[1:2],trimStr))
    }
  }

  nbTraces   = as.integer(as.character(headerHD[4,2]))
  nbPt     = as.integer(as.character(headerHD[5,2]))
  #----------------#
  #--- READ DT1 ---#
  dt1 <- file(fileNameDT1 , "rb")

  indexDT1Header=c("traces", "position", "samples","topo", "NA1", "bytes",
                    "tracenb", "stack","window","NA2", "NA3", "NA4",
                    "NA5", "NA6", "recx","recy","recz","transx","transy",
                    "transz","time0","zeroflag", "NA7", "time","x8","com")  
                    #,"com1","com2","com3","com4","com5","com6")
  headerDT1 = list()
  myData = matrix(NA,nrow=nbPt,ncol=nbTraces)
  for(i in 1:nbTraces){
    for(j in 1:25){
      headerDT1[[indexDT1Header[j]]][i] = readBin(dt1, what=numeric(), 
                                                  n = 1L, size=4)
      # hour of the day: format(as.POSIXct('0001-01-01 00:00:00') + 
               # headerDT1$time[1], "%I:%M:%S %p") 
    }
    # read the 28 characters long comment
    headerDT1[[indexDT1Header[26]]][i] = readChar(dt1, 28)
    # read the nbPt * 2 bytes rrace data
    myData[,i] = readBin(dt1, what=integer(), n = nbPt, size=2)
  }
  #headerDT1$time2 <- format(as.POSIXct(paste(as.character(headerHD[2,2]), 
            # ' 00:00:00', sep="")) + headerDT1$time, "%d-%m-%Y %I:%M:%S") 
  close(dt1)
  return(list(hd = headerHD, dt1hd = headerDT1, data=myData))
}
#-----------------
#-----------------

# A = GPR$hd
# if position = TRUE, return the row number
# if number = TRUE, try to convert
.getHD <- function(A,string,number=TRUE,position=FALSE){
  if(number){
    value <- as.numeric(A[trimStr(A[,1])==string,2])
  }else{
    value <- A[trimStr(A[,1])==string,2]
  }
  if(length(value)>0){
    if(position){
      pos <- which((trimStr(A[,1]) == string ) == TRUE)[1]
      return(c(value,pos))
    }else{
      return(value)
    }
  }else{
    return(NULL)
  }
}

#--------------------------------------
# http://stackoverflow.com/questions/17256834/getting-the-arguments-of-a-parent-
# function-in-r-with-names
# Ryan Grannell
# website   twitter.com/RyanGrannell
# location   Galway, Ireland
getArgs <- function (return_character=TRUE) {
  arg <- as.list(match.call(def = sys.function( -1 ),
           call = sys.call(-1),
           expand.dots = TRUE )
           )
  narg <- length(arg)
  if(return_character){
    if(narg >=3){
      eval_arg <- sapply(arg[3:narg],eval)
      paste(arg[[1]],":", paste(names(arg[3:narg]),
          sapply(eval_arg,pasteArgs,arg[3:narg]),sep="=",collapse="+"),sep="")
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
# paste(names(arg[3:narg]),sapply(eval_arg,paste_args,arg[3:narg]),sep="=",
# collapse="+")
  proc_add <- paste(names(arg), sapply(arg,pasteArgs, arg),
                  sep = "=", collapse = "+")
  if(substr(proc,nchar(proc),nchar(proc)) == ":"){
    proc <- paste(proc, proc_add, sep = "")
  }else{
    proc <- paste(proc, "+", proc_add, sep = "")
  }
  return(proc)
}


