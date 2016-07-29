#------------------------------------------#
#----------- CLASS DEFINITION -------------#

#' An S4 class to represent a ground-penetrating radar (GPR) data.
#'
#' @slot version A length-one character vector indicating the version of RGPR
#' @slot data A \eqn{m \times n} numeric matrix consiting of a 
#'            cross-section of signal amplitudes as a function of the GPR 
#'            position. The columns of \code{data} correspond to the GPR 
#'            traces and the row of \code{data} to the time/depth samples.
#' @slot traces A length-m numeric vector corresponding to the trace number.
#' @slot depth A length-n numeric vector indicating the sampling time or
#'              the vertical position of the trace samples.
#' @slot pos A length-m numeric vector indicating the relative position of
#'            the trace along the survey profile.
#' @slot time0 A length-m numeric vector containing the 'time-zero' of every
#'              trace.
#' @slot time A length-m numeric vector containing the recording time of 
#'            every trace.
#' @slot fid A length-m character vector containing fiducial markers associated
#'           with the traces.
#' @slot ann A length-m character vector containing annotations associated 
#'          with the traces.
#' @slot coord A \eqn{m \times 3} matrix containing the (x, y, z) positions of
#'             every trace.
#' @slot rec  A \eqn{m \times 3} matrix containing the (x, y, z) positions of
#'             the receiver for every trace.
#' @slot trans A \eqn{m \times 3} matrix containing the (x, y, z) positions of
#'             the transmitter for every trace.
#' @slot coordref A length-3 numeric vector containing the coordinates of a
#'                local reference.
#' @slot freq A length-one numeric vector corresponding to the GPR antennae
#'            frequency (in MHz).
#' @slot dz A length-one numeric vector corresponding to the time or depth
#'          sampling step.
#' @slot dx A length-one numeric vector corresponding to the trace step.
#' @slot antsep A length-one numeric vector corresponding to the antenna
#'              separation.
#' @slot name A length-one character vector containing the name of the GPR
#'            data.
#' @slot description A length-one character vector containing the description 
#'                  of the GPR data.
#' @slot filepath A length-one character vector containing the file path of 
#'                the original GPR data.
#' @slot depthunit A length-one character vector corresponding to the 
#'                  time/depth unit (e.g., "ns", "m").
#' @slot posunit A length-one character vector corresponding to the 
#'               (x, y)-unit (e.g., "m").
#' @slot surveymode A length-one character vector containing the survey mode
#'                  (e.g., "Reflection", "CMP")
#' @slot date A length-one character vector containing the date of the survey
#'            in the format "yyyy-mm-dd".
#' @slot crs A length-one character vector containing the coordinate 
#'          reference system following the R notation of proj4string 
#'          from the PROJ.4 library. 
#' @slot proc A length-varying character vector whose each element correspond
#'            to a processing step applied to the data.
#' @slot vel  A list containing the velocity model.
#' @slot delineations A list containing delineated structures.
#' @slot hd A list containing less relevant additional informations.
setClass(
  Class="GPR",  
  slots=c(
    version = "character",   # version of the class
    data = "matrix",     # one column per trace
    traces = "numeric",  # numbering of each trace (from 1 to ntr)
    depth = "numeric",  # depth position
    pos = "numeric",    # position  of the traces
    time0 = "numeric",  # time-zero (first air-wave arrival)
    time = "numeric",   # time of the trace recording
    fid = "character",   # fiducial marks
    ann = "character",  # annotation (e.g. intersections)
    coord = "matrix",   # coordinates (x,y,z) of each traces
    rec = "matrix",     # coordinates (x,y,z) of the receiver antenna
    trans = "matrix",   # coordinates (x,y,z) of the transmitter antenna
    coordref = "numeric", # coordinates references
    freq = "numeric",   # antenna frequency
    dz = "numeric",   # time/depth sampling
    dx = "numeric",     # spatial trace sampling
    antsep = "numeric",   # antenna separation
    name = "character",  # name of the profile
    description = "character",  # description of the pro
    filepath = "character",  # filepath of the profile
    depthunit = "character", # time/depth unit
    posunit = "character",  # spatial unit
    surveymode = "character", # survey mode (reflection/CMP)
    date = "character",    # date of the survey , format %Y-%m-%d
    crs = "character",  # coordinate reference system of coord
    proc= "character",  # processing steps
    vel = "list",      # velocity model
    delineations = "list",  # delineated lines
    hd = "list"      # header from *.dt1 file
  )
)


#------------------------------------------#
#-------------- CONSTRUCTOR ---------------#
# x = classical GPR list
.gpr <- function(x, name=character(0), description=character(0),
                 fPath=character(0)){
  rec_coord <- cbind(x$dt1$recx,x$dt1$recy,x$dt1$recz)
  trans_coord <- cbind(x$dt1$transx,x$dt1$transy,x$dt1$transz)
  if(sum(is.na(rec_coord))>0){
    warning(paste(name,": ",sum(is.na(rec_coord)), 
                "NA's in the receiver coordinates\n"))
  }
  if(sum(is.na(trans_coord))>0){
    warning(paste(name,": ",sum(is.na(trans_coord)), 
                "NA's in the transmitter coordinates\n"))
  }
  if(sum(is.na(x$dt1$topo))>0){
    warning(paste(name,": ",sum(is.na(x$dt1$topo)), 
                "NA's in the topo coordinates\n"))
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
  #====== HEADER DATA (FILE *.HD) ======#
  pos_used <- integer(nrow(x$hd))
  ttw  <- .getHD(x$hd,"TOTAL TIME WINDOW", position=TRUE)
  if(!is.null(ttw)){
    dz <- ttw[1]/nrow(x$data)
    pos_used[ttw[2]] <- 1L
  }else{
    warning("time/depth resolution unknown! I take dz = 0.4!\n")
    dz <- 0.4
    ttw  <- nrow(x$data) * dz
  }
  tzap <- .getHD(x$hd, "TIMEZERO AT POINT", position=TRUE)
  if(sum(abs(x$dt1$time0)) == 0){
    if(!is.null(tzap)){
      time_0 <- rep(tzap[1]*dz - dz,ncol(x$data))
      pos_used[tzap[2]] <- 1L
    }
  }else{
    time_0 <- x$dt1$time0
  }
  surveyDate <- gsub(pattern ="[^0-9]", replacement = "-", x$hd[3,2])
  # 2014-04-10 (new PulseEkko format)
  yyyymmdd <- "^([0-9]{4})([^0-9])([0-9]{2})([^0-9])([0-9]{2})"
  # 10/06/2011 (old PulseEkko format)
  ddmmyyyy <- "^([0-9]{2})([^0-9])([0-9]{2})([^0-9])([0-9]{4})"
  if(grepl(yyyymmdd,surveyDate)){
    d <- as.character(as.Date(surveyDate, "%Y-%m-%d"))
    pos_used[3] <- 1L
  }else if(grepl(ddmmyyyy,surveyDate)){
    d <- as.character(as.Date(surveyDate, "%d-%m-%Y"))
    pos_used[3] <- 1L
  }else{
    warnings("Could not understand the survey date\n")
    d <- "1970-01-01"
  }
  GPR_device <-  paste(x$hd[2,1],x$hd[2,2],sep="")
  pos_used[2] <- 1L
  if(!grepl("^(Data.)",GPR_device)){
    GPR_device <- ""
  }
  dx <- .getHD(x$hd, "STEP SIZE USED", position=TRUE)
  if(!is.null(dx)){
    pos_used[dx[2]] <- 1L
  }else{
    dx <- mean(diff(x$dt1hd$position))
  }
  
  posunit = .getHD(x$hd, "POSITION UNITS",number=FALSE, position=TRUE)
  if(!is.null(posunit)){
    pos_used[as.numeric(posunit[2])] <- 1L
  }else{
    posunit <- "m"
  }
  freq = .getHD(x$hd, "NOMINAL FREQUENCY", position=TRUE)
  if(!is.null(freq)){
    pos_used[freq[2]] <- 1L
  }else{
    freq <- 100
  }
  antsep = .getHD(x$hd, "ANTENNA SEPARATION", position=TRUE)
  if(!is.null(antsep)){
    pos_used[antsep[2]] <- 1L
  }else{
    antsep <- 1.00
  }
  surveymode = .getHD(x$hd, "SURVEY MODE",number=FALSE, position=TRUE)
  if(!is.null(surveymode)){
    pos_used[as.numeric(surveymode[2])] <- 1L
  }else{
    surveymode <- "m"
  }

  #-------- header: x@hd ----------#
  nop  <- .getHD(x$hd,"NUMBER OF PTS/TRC", position=TRUE)
  if(!is.null(nop)){
    pos_used[nop[2]] <- 1L
  }
  not <- .getHD(x$hd, "NUMBER OF TRACES", position=TRUE)
  if(!is.null(not)){
    pos_used[not[2]] <- 1L
  }
  startpos <- .getHD(x$hd, "STARTING POSITION", position=TRUE)
  if(!is.null(startpos)){
    pos_used[startpos[2]] <- 1L
  }else{
    startpos <- 0
  }
  endpos <- .getHD(x$hd, "FINAL POSITION", position=TRUE)
  if(!is.null(endpos)){
    pos_used[endpos[2]] <- 1L
  }else{
    endpos <- dx[1]*ncol(x$data)
  }
  
  hd_list <- list("startpos" = startpos[1],   # "STARTING POSITION"
                  "endpos" = endpos[1],       # "FINAL POSITION"
                  "gprdevice" = GPR_device)

  x$hd2 <- x$hd[!pos_used,]
  if(nrow(x$hd2)>0){
    key <-  trimStr(x$hd2[,1])
    test <- key!=""
    key <- key[test]
    key2 <- gsub("[[:punct:]]",replacement="",key)
    key2 <- gsub(" ",replacement="_",key2)
    nameL <- trimStr(x$hd2[test,2])
    names(nameL) <- as.character(key2)
    hd_list_supp <- as.list(nameL)
    hd_list <- c(hd_list,hd_list_supp)
  }
  myT <- as.double(as.POSIXct(x$dt1$time, origin = as.Date(d)))
  new("GPR",   version="0.1",
        data = byte2volt()*x$data,
        traces = x$dt1$traces,            # x$dt1$traces
        fid = trimStr(x$dt1$com),         # x$dt1$fid    <-> x$dt1$x8
        coord = coord,                    # x$dt1$topo  of the traces
        pos = x$dt1$pos,                  # x$dt1$position  of the traces
        depth = seq(0,by=dz,length.out=nrow(x$data)),
        rec = rec_coord,                  # x$dt1$recx,x$dt1$recy,x$dt1$recz
        trans = trans_coord,
        time0 = time_0,                   # x$dt1$time0
        time = myT,                       # x$dt1$time
        proc = character(0),              # processing steps
        vel = list(0.1),                  # m/ns
        name = name,
        description = description,
        filepath = fPath,
        dz = dz, 
        dx = dx[1],                       # "STEP SIZE USED"
        depthunit = "ns",
        posunit = posunit[1],
        freq = freq[1], 
        antsep = antsep[1], 
        surveymode = surveymode[1],
        date = d,
        crs = character(0),
        hd = hd_list                      # header
  )

}

#' Read a GPR data file
#' 
#' @param fPath Filepath (character).
#' @param desc Short description of the file (character).
#' @param coordfile Filepath of a text file containing the coordinates (x,y,z)
#'                   of each traces.
#' @param crs Coordinate reference system (character)
#' @param intfile Filepath of a text file containing the intersection.
#' @return The GPR data as object of the class RGPR.
#' @examples
#' NULL
#' @name readGPR
#' @rdname readGPR
# @aliases readGPR-methods
setMethod("readGPR", "character", function(fPath, desc = "", 
          coordfile = NULL, crs = "", intfile = NULL){
    ext <- .fExt(fPath)
    # DT1
    if(file.exists(fPath)){
      if("DT1" == toupper(ext)){
        name <- .fNameWExt(fPath)
        A <- readDT1(fPath)
        x <- .gpr(A,name=name,fPath=fPath,desc=desc)
        if(!is.null(coordfile)){
          cat("coordinates added\n")
          xyzCoord <- as.matrix(read.table(coordfile,sep=",",head=TRUE))
          x@crs  <-   crs    
          x@coord <-   xyzCoord
        }
        if(!is.null(intfile)){
          cat("intersection added\n")
          intGPR <- (read.table(intfile, sep = " ", head = TRUE, 
                      stringsAsFactors = FALSE))
          x@ann <- intGPR
        }
        return(x)
      }else if("rds" == tolower(ext)){
        x <- readRDS(fPath)
        if(class(x)=="GPR"){
          x@filepath <- fPath
          return(x)
        }else if(class(x)=="list"){
          versRGPR <- x[["version"]]
          y <- new("GPR",
            version = x[['version']],
            data = x[['data']],
            traces = x[['traces']],           # x$dt1$traces
            depth = x[['depth']],
            pos = x[['pos']],                 # x$dt1$position of the traces
            time0 = x[['time0']],             # x$dt1$time0
            time = x[['time']],               # x$dt1$time
            fid = trimStr(x[['fid']]),        # x$dt1$fid <-> x$dt1$x8
            ann = trimStr(x[['ann']]),        # x$dt1$fid <-> x$dt1$x8
            coord = x[['coord']],             # x$dt1$topo  of the traces
            rec = x[['rec']],                 # x$dt1$recx,x$dt1$recy,x$dt1$recz
            trans = x[['trans']],
            coordref = x[['coordref']],       # x$dt1$topo of the traces
            freq = x[['freq']], 
            dz = x[['dz']], 
            dx = x[['dx']], 
            antsep = x[['antsep']], 
            name = x[['name']],
            description = x[['description']],
            filepath =x[['filepath']],
            depthunit = x[['depthunit']],
            posunit = x[['posunit']],
            surveymode = x[['surveymode']],
            date = x[['date']],
            crs = x[['crs']],
            proc = x[['proc']],               # processing steps
            vel = x[['vel']],                 # m/ns
            delineations = x[['delineations']],
            hd =  x[['hd']]                   # header
          )
          y@filepath <- fPath
          return(y)
        }
      }else{
        stop(paste0("Problem with the file extension.",
                    "Should be either '.DT1' or '.rds'\n"))
      }
    }else{
      stop(fPath, "does not exist!")
    }
  } 
)

#------------------------------------------#
#---------------- COERCION ----------------#
###--- Coercion from GPR to ...
setAs(from = "GPR", to = "matrix", def = function(from){ from@data } )

#' Coercion to matrix
#'
#' @name as.matrix
#' @rdname GPRcoercion
#' @export
setMethod("as.matrix",signature(x="GPR"),function(x){as(x,"matrix")})

setAs(from = "GPR", to = "vector", def = function(from){ from@data})

#' Coercion to vector
#'
#' @name as.vector
#' @rdname GPRcoercion
#' @export
setMethod("as.vector", signature(x="GPR"), 
          function(x,mode="any"){as.vector(x@data)})

#' @importClassesFrom sp SpatialLines
setAs(from = "GPR", to = "SpatialLines",
      def = function (from) as.SpatialLines(from))

#' Coercion to SpatialLines
#'
#' @name as.SpatialLines
#' @rdname GPRcoercion
#' @export
# as.SpatialLines <- function (x, ...){
setMethod("as.SpatialLines", signature(x = "GPR"), function(x){
  myLine <- sp::Line(x@coord[,1:2])
  myLines <- sp::Lines(list(myLine), ID=x@name)
  mySpatLines <- sp::SpatialLines(list(myLines))
  if(length(crs(x)) == 0){
  warning("no CRS defined!\n")
  }else{
    sp::proj4string(mySpatLines) <- sp::CRS(crs(x))
  }
  return(mySpatLines)
})

#' @importClassesFrom sp SpatialPoints
setAs(from = "GPR", to = "SpatialPoints",
      def = function (from) as.SpatialPoints(from))

#' Coercion to SpatialPoints
#'
#' @name as.SpatialPoints
#' @rdname GPRcoercion
#' @export
# as.SpatialPoints <- function (x, ...){
setMethod("as.SpatialPoints", signature(x = "GPR"), function(x){
  myPoints <- as.data.frame(x@coord)
  sp::coordinates(myPoints) = ~E + N
  if(length(crs(x)) == 0){
  warning("no CRS defined!\n")
  }else{
    sp::proj4string(myPoints) <- sp::CRS(crs(x))
  }
  return(myPoints)
})

#' Coercion to numeric
#'
#' @name as.numeric
#' @rdname GPRcoercion
#' @export
setMethod("as.numeric", "GPR",  function(x, ...) as.numeric(x@data))

setMethod("as.double", "GPR",  function(x, ...) as.double(x@data))

###--- Coercion from ... to GPR
setAs(from = "matrix", to = "GPR", def = function (from) as.GPR.matrix(from))

#' Coercion from matrix to GPR
#'
#' @name as.GPR.matrix
#' @rdname GPRcoercion
#' @export
as.GPR.matrix <- function (x, ...){
  new("GPR", 
    version = "0.1",
    data = x,
    traces = 1:ncol(x),           # x$dt1$traces
    pos = (1:ncol(x) -1)*0.25,    # x$dt1$position  of the traces
    depth = (1:nrow(x) -1)*0.8,
    time0 = rep(0,ncol(x)),       # x$dt1$time0
    time = rep(0,ncol(x)),        # x$dt1$time
    proc = character(0),          # processing steps
    vel = list(0.1),              # m/ns
    name = character(0),
    description = character(0),
    filepath = character(0),
    dz = 0.8, 
    dx = 0.25, 
    depthunit = "ns",
    posunit = "m",
    freq = 100, 
    antsep = 1, 
    surveymode = "reflection",
    date = format(Sys.time(), "%d/%m/%Y"),
    crs = character(0),
    hd = list()                   # header
  )
}

setAs(from = "list", to = "GPR", def = function (from) as.GPR.list(from))

#' Coercion from list to GPR
#'
#' @name as.GPR.list
#' @rdname GPRcoercion
#' @export
as.GPR.list <- function (x, ...){
  # prefix: "d_" for default
  if(any("data" == tolower(names(x))) && is.matrix(x$data)){
    if(is.null(x$pos) && !is.null(x$dx) && is.numeric(x$dx)){
      x$pos <- (1:ncol(x$data) -1) * x$dx
    }else{
      x$pos <- (1:ncol(x$data) -1)*0.25
    }
    if(is.null(x$depth) && !is.null(x$dz) && is.numeric(x$dz)){
      x$depth <- (1:nrow(x$data) -1) * x$dz
    }else{
      x$depth <- (1:nrow(x$data) -1)*0.8
    }
    if(is.null(x$dx)){
      x$dx <- mean(diff(x$pos))
    }
    if(is.null(x$dz)){
      x$dz <- mean(diff(x$depth))
    }
    myArg <- as.list(match.call(def = sys.function(-2),
           call = sys.call(-2),
           expand.dots = FALSE )
           )
    d_name <- paste(eval(myArg[2]))
    y <- new("GPR", 
        version = "0.1",
        data = x$data,
        traces = 1:ncol(x$data),    # trace numbering
        pos = x$pos,                # position of the traces
        depth= x$depth,
        time0 = rep(0,ncol(x$data)),  
        time = rep(0,ncol(x$data)), # time of trace records
        proc =  character(0),       # processing steps
        # proc =  ifelse(is.null(x$proc),character(0),x$proc),  
        # processing steps
        vel = list(0.1),  #m/ns
        name = as.character(d_name),
        description = paste("coercion of ",
                as.character(d_name)," (",typeof(x),") into GPR",sep=""),
        filepath = character(0),
#         ntr = ncol(x$data), 
#         w = nrow(x$data)*x$dz, 
        dz = x$dz, 
        dx = x$dx, 
        depthunit = "ns",
        posunit = "m",
        freq = 100,
        antsep = 1, 
        surveymode = "reflection",
        date = format(Sys.time(), "%d/%m/%Y"),
        crs = character(0),
        hd = list())
    sNames <- slotNames(y)
    sNames <- sNames[ !(sNames %in% c("data","pos","depth","dz","dx"))]
    for(i in seq_along(sNames)){
      if(!is.null(x[[sNames[i]]])){
        slot(y, sNames[i], check = TRUE) <- x[[sNames[i]]]
      }
    }# if(!is.null(x$traces)) 
    return(y)
  }else{
    stop("The list must have a 'data' index name")
  }
}    


#' @export
setMethod("length", "GPR", function(x) ncol(x@data))
#' @export
setMethod("summary", "GPR", function(object, ...) 
#' @export
summary(as.vector(object@data)))
#' @export
setMethod("mean", "GPR", function(x, ...) mean(as.vector(x@data)))
#' @export
setMethod("median", "GPR", function(x, na.rm = FALSE) 
        median(as.vector(x@data),na.rm = FALSE))
# setMethod("range", "GPR", function(..., na.rm=FALSE) 
# range(as.matrix(...),na.rm=na.rm))

#' @export
setMethod(
  f="apply", 
  signature="GPR", 
  definition=function(X,MARGIN,FUN,...){
    apply(X@data,MARGIN,FUN,...)
  }
)
#' @export
setMethod(
  f="nrow", 
  signature="GPR", 
  definition=function(x){
    nrow(x@data)
  }
)
#' @export
setMethod(
  f="ncol", 
  signature="GPR", 
  definition=function(x){
    ncol(x@data)
  }
)
#' @export
setMethod(
  f="dim", 
  signature="GPR", 
  definition=function(x){
    dim(x@data)
  }
)
#' @export
setMethod(
  f="min", 
  signature="GPR", 
  definition=function(x,...,na.rm=FALSE){
    min(x@data,na.rm=na.rm)
  }
)
#' @export
setMethod(
  f="max", 
  signature="GPR", 
  definition=function(x,...,na.rm=FALSE){
    max(x@data,na.rm=na.rm)
  }
)
#' @export
setMethod(
  f="range", 
  signature="GPR", 
  definition=function(x,...,na.rm=FALSE){
    range(x@data,na.rm=na.rm)
  }
)

#' Basic mathematical functions
#'
#' 
#' @param x An object of the class RGPR.
#' @examples
#' data(frenkeLine00)
#' A <- exp(frenkeLine00)
#' @name Math
#' @rdname Math
#' @export
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
      exp  = exp(x@data),
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
        # "acos" "acosh" "asin" "asinh" "atan" "atanh" "gamma" "lgamma"
        # "digamma" "trigamma"
      stop(paste(.Generic, "not allowed on GPR objects"))
    )
    proc <- getArgs()
    x@proc <- c(x@proc, proc)
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



#' Basic arithmetical functions
#'
#' 
#' @param e1 An object of the class RGPR.
#' @param e2 An object of the class RGPR.
#' @examples
#' data(frenkeLine00)
#' A <- exp(frenkeLine00)
#' B <- A + frenkeLine00
#' @name Arith
#' @rdname Arith
#' @export
setMethod(
  f= "Arith",
  signature=c(e1="GPR",e2="ANY"), 
  definition=.GPR.arith
)
#' @name Arith
#' @rdname Arith
#' @export
setMethod(
  f= "Arith",
  signature=c(e1="GPR",e2="GPR"), 
  definition=.GPR.arith
)
#' @name Arith
#' @rdname Arith
#' @export
setMethod(
  f= "Arith",
  signature=c(e1="ANY",e2="GPR"), 
  definition=.GPR.arith
)

#------------------------------
# "["
#' @export
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
#         x@w <- length(i)*x@dz
        x@depth <- x@depth[i]
      } else{ 
        # rval <- rval[i, j, drop = drop.]
        rval <- rval[i, j, drop = drop]
#         x@w <- length(i)*x@dz
        x@depth <- x@depth[i]
        x@traces <- x@traces[j]
        x@pos <- x@pos[j]
        x@time0 <- x@time0[j]
        x@time <- x@time[j]
        x@fid <- x@fid[j]
        if(length(x@coord)>0)  x@coord <- x@coord[j,,drop=FALSE]
        if(length(x@rec)>0) x@rec <- x@rec[j,,drop=FALSE]
        if(length(x@trans)>0) x@trans <- x@trans[j,,drop=FALSE]
#         x@ntr <- length(j)
        if(!is.null(x@hd$startpos) && !is.null(x@hd$endpos)){
          trpos <- seq(x@hd$startpos, x@hd$endpos,by=x@dx)
          x@hd$endpos <- trpos[j[length(j)]]
          x@hd$startpos <- trpos[j[1]]
        }
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
#' @export
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
    x@proc <- c(x@proc, "[<-")
    return (x)
  }
)

#' @export
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

#' Name of the GPR data
#' 
#' @name name
#' @rdname name
#' @export
setMethod("name", "GPR", function(x){
  return(x@name)
} 
)

#' @name name<-
#' @rdname name
#' @export
setReplaceMethod(
  f="name",
  signature="GPR",
  definition=function(x,value){
    value <- as.character(value)[1]
    name <- value
    x@proc <- c(x@proc, "name<-")
    return(x)
  }
)

#' Description of the GPR data
#' 
#' @name description
#' @rdname description
#' @export
setMethod("description", "GPR", function(x){
  return(x@description)
} 
)

#' @name description<-
#' @rdname description
#' @export
setReplaceMethod(
  f="description",
  signature="GPR",
  definition=function(x,value){
    description <- as.character(value)[1]
    x@proc <- c(x@proc, "description<-")
    return(x)
  }
)

#' Filepath of the GPR data
#' 
#' @name filepath
#' @rdname filepath
#' @export
setMethod("filepath", "GPR", function(x){
    return(x@filepath)
  } 
)

#' @name filepath<-
#' @rdname filepath
#' @export
setReplaceMethod(
  f="filepath",
  signature="GPR",
  definition=function(x,value){
    x@filepath <- as.character(value)[1]
    x@proc <- c(x@proc, "filepath<-")
    return(x)
  }
)

#' Annotations of the GPR data
#' 
#' @name ann
#' @rdname ann
#' @export
setMethod("ann", "GPR", function(x){
    return(x@ann)
  } 
)

#' @name ann<-
#' @rdname ann
#' @export
setReplaceMethod(
  f="ann",
  signature="GPR",
  definition=function(x,values){
    vals <- values
    if(!is.matrix(values)){
      vals <- matrix(values,nrow=1,ncol=length(values))
    }
    traces <- (values[,1])
    annnames <- as.character(values[,2])
    valuesList <- (tapply(annnames, traces, identity))
    test <- unlist(lapply(valuesList,paste,sep="",collapse="#"))
    x@ann <- character(length(x))
    x@ann[as.numeric(names(test))] <- test
    x@proc <- c(x@proc, "ann<-")
    # FIXME > sapply(test, trimStr)
    return(x)
  }
)

#' Coordinates of the GPR data
#' 
#' @name coord
#' @rdname coord
#' @export
setMethod("coord", "GPR", function(x, i, ...){
#     if(is.integer(i) && i > 0 && i < 4)
    if(length(x@coord) == 0){
      return(x@coord)
    }
    if(missing(i)){
      i <- 1:3
    }
    return(x@coord[, i, ...])
  } 
)

#' @name coord<-
#' @rdname coord
#' @export
setReplaceMethod(
  f="coord",
  signature="GPR",
  definition=function(x,values){
    values <- as.matrix(values)
    if(ncol(x@data) == nrow(values) && ncol(values)==3){
      x@coord <- values
      x@proc <- c(x@proc, "coord<-")
    }else{
      stop("Dimension problem!!")
    }
    return(x)
  }
)
#' Coordinate reference system (CRS) of the GPR data
#' 
#' @name crs
#' @rdname crs
#' @export
setMethod("crs", "GPR", function(x){
    return(x@crs)
  } 
)

#' @name crs<-
#' @rdname crs
#' @export
setReplaceMethod(
  f="crs",
  signature="GPR",
  definition=function(x,value){
    value <- as.character(value)[1]
    x@crs <- value
    x@proc <- c(x@proc, "crs<-")
    return(x)
  }
)

#' Velocity model of the GPR data
#' 
#' @name vel
#' @rdname vel
#' @export
setMethod("vel", "GPR", function(x){
  return(x@vel)
} 
)

#' @name vel<-
#' @rdname vel
#' @export
setReplaceMethod(
  f="vel",
  signature="GPR",
  definition=function(x,values){
    x@vel <- values
    x@proc <- c(x@proc, "vel<-")
    return(x)
  }
)


#' 'time-zero' of every traces
#' 
#' \code{time0} returns the 'time-zero' of every traces. Generally, 
#' 'time-zero' corresponds to the first wave arrival (also called first wave
#' break).
#' 
#' @param x An object of the class GPR.
#' @return A vector containing the time-zero values of each traces.
#' @examples
#' data(frenkeLine00)
#' time0(frenkeLine00)
#' @seealso \code{\link{firstBreack}} to estimate the first wave break.
#' @name time0
#' @rdname time0
#' @export
# @aliases time0-methods time0<- time0<--methods
setMethod("time0", "GPR", function(x){
    return(x@time0)
  } 
)

#' @name time0<-
#' @rdname time0
#' @export
setReplaceMethod(
  f="time0",
  signature="GPR",
  definition=function(x,value){
    if(length(value) == length(x@time0)){
      x@time0 <- value
    }else{
      x@time0 <- rep(value[1], length(x@time0))
    }
    x@proc <- c(x@proc, "time0<-")
    return(x)
  }
)
#' Fiducial markers of the GPR data
#' 
#' @name fid
#' @rdname fid
#' @export
setReplaceMethod(
  f="fid",
  signature="GPR",
  definition=function(x,values){
    values <- as.character(values)
    x@fid <- values
    x@proc <- c(x@proc, "fid<-")
    return(x)
  }
)

#' @name fid<-
#' @rdname fid
#' @export
setMethod("fid", "GPR", function(x){
    return(x@fid)
  } 
)

#' Values of the GPR data
#' 
#' @name values
#' @rdname values
#' @export
setMethod("values", "GPR", function(x){
    return(x@data)
  } 
)

#' @name values<-
#' @rdname values
#' @export
setReplaceMethod(
  f="values",
  signature="GPR",
  definition=function(x,value){
    if(all(dim(value)==dim(x))){
      x@data <- value
      x@proc <- c(x@proc, "values<-")
      return(x)
    }else{
      stop("x [",nrow(x),"x",ncol(x),"] and A [",nrow(value),"x",ncol(value),
            "] should have the same size\n")
    }
  } 
)

#' Amplitude of the GPR data
#' 
#' @name ampl
#' @rdname ampl
#' @export
setMethod("ampl", "GPR", function(x, FUN=mean, ...){
  AMP <- apply(abs(x@data),1,FUN,...)
    return(AMP)
  } 
)

#' Processing steps applied to the data
#' 
#' \code{processing} returns all the processing steps applied to the data.
#' 
#' @param x An object of the class GPR.
#' @return A character vector whose elements contain the name of the 
#' processing functions with their arguments applied previously on the
#' GPR data.
#' @examples
#' data(frenkeLine00)
#' A <- dewow(frenkeLine00, type = "Gaussian")
#' processing(A)
#' @name processing
#' @rdname processing
#' @export
setMethod("processing", "GPR", function(x){
    return(x@proc)
  } 
)


#================= PROCESSING ===============#
#----------------- DC-SHIFT
#' Direct-current removal
#' 
#' @name dcshift
#' @rdname dcshift
#' @export
setMethod("dcshift", "GPR", function(x, u, FUN=mean){
    x <-  x - apply(x[u,],2,FUN)
    if(class(FUN)=="function"){
      nameFun <- "FUN"
    }else{
  #  if(isGeneric("FUN")){
      nameFun0 <- selectMethod(FUN, "numeric")
      nameFun <-nameFun0@generic[1]
    }
    x@proc <- c(x@proc, paste0("dcshift:",head(u,1),"-",tail(u,1),"+",nameFun))
    return(x)
  } 
)

#----------------- FIRST-BREAK
#' First wave break
#'
#' Compute the first wave break
#'
#' Jaun I. Sabbione and Danilo Velis (2010). Automatic first-breaks picking: 
#' New strategies and algorithms. Geophysics, 75 (4): v67-v76
#' -> modified Coppens's Method
#' nl = length leading window: about one period of the firs-arrival waveform
#' ns = length eps (edge preserving smoothing) window: good results with ns 
#' between one and two signal periods
#'        -> default values ns= 1.5*nl
#' bet = stabilisation constant, not critical, set to 0.2*max(amplitude) 
#' @name dcshift
#' @rdname dcshift
#' @export
setMethod("firstBreack", "GPR", function(x, w = 11, ns = NULL, bet = NULL){
    w <- round(w / x@dz)
    if( (w %% 2) == 0){
      w <- w + 1
    }
    if(is.null(ns)){
      ns <- round(1.5 * w)
    }
    if( ns %% 2 == 0){
      ns <- ns + 1
    }
    xs <- x@data^2
    if(is.null(bet)){
      bet <- 0.2 * max(xs)
    }
    fb <- apply(xs, 2, .firstBreackPicking, w = w, ns = ns, bet = bet)*x@dz
    return(fb)
  } 
)

#----------------- DEWOW
#' Trace dewowing
#' 
#' \code{dewow} remove the low-frequency component (the so-called 'wow') of 
#' every trace..
#' 
#' @param x An object of the class GPR.
#' @param type A length-one character vector, either \code{MAD} (Median
#'              Absolute Deviation filter) or \code{Gaussian} (Gaussian
#'              filter)
#' @param w A length-one numeric vector equal to the window length 
#'            of the filter. Per default, the filter length is five times
#'            the GPR pulse width.
#' @return An object of the class GPR whose traces are dewowed.
#' @examples
#' data(frenkeLine00)
#' A <- dewow(frenkeLine00, type = "Gaussian")
#' A
#' @name dewow
#' @rdname dewow
#' @export
setMethod("dewow", "GPR", function(x, type=c("MAD","Gaussian"),w){
  type <- match.arg(type)
  if(missing(w)){
    # argument initialization
    # pulse width in ns, (x@freq is in MHz)
    pw <- 1/(x@freq * 10^6)/10^-9
    w <- (5 * pw)/x@dz
  }else{
    w <- round(w / x@dz)
  }
  if(type=="MAD"){  
    A <- x@data
    if(length(dim(A))<2){
      A <- matrix(A,ncol=1,nrow=length(A))
    }
    X <- rbind(matrix(0,ncol=ncol(A),nrow=w), A, matrix(0,ncol=ncol(A),nrow=w))
    n <- nrow(X)
    Y <- X
    for (i in (w + 1):(n - w)) {
      Y[i,] <-  apply( X[(i - w):(i + w),,drop=FALSE],2, median)
      # x0 = 0.1  # argument initialization
      # S0 <- 1.4826 * apply( abs(X[(i - w):(i + w),,drop=FALSE] - Xmed),
      #  2, median)
      # test <- abs(X[i,] - Xmed) > x0 * S0
      # Y[i,test] <- Xmed[test]      
    }
    x@data <- A - Y[(w+1):(n-w),]
  }else if(type == "Gaussian"){
    t0 <- round(mean(x@time0)/x@dz)
    A <- x@data
    before_t0 <- x@depth <= mean(x@time0)
    A[before_t0,] <- 0
    if(length(dim(A))<2){
      A <- matrix(A,ncol=1,nrow=length(A))
    }
    x@data[!before_t0,] <- A[!before_t0,] - 
    mmand::gaussianSmooth(A,w)[!before_t0,]
  }
  proc <- getArgs()
  x@proc <- c(x@proc, proc)
  return(x) 
})

#----------------- 1D-SCALING (GAIN)
#' Gain compensation
#' 
#' @name gain
#' @rdname gain
#' @export
setMethod("gain", "GPR", function(x, 
          type=c("power", "exp", "agc"),...){
  type <- match.arg(type)
  x@data[is.na(x@data)] <-0
  if(type=="power"){
    x@data <- .gainPower(x@data, dts = x@dz, ...)
  }else if(type=="exp"){
    x@data <- .gainExp(x@data, dts = x@dz, ...)
  }else if(type=="agc"){
    x@data <- .gainAgc(x@data, dts = x@dz, ...)
  }
  proc <- getArgs()
  x@proc <- c(x@proc, proc)
  return(x)
  } 
)

#----------------- 1D-FILTER
#' One dimensional filters
#' 
#' @name filter1D
#' @rdname filter1D
#' @export
setMethod("filter1D", "GPR", function(x, type = c("median", "hampel", 
          "Gaussian"), ...){
    type <- match.arg(type)
    if(type == "median"){
      w <- 10    # argument initialization
      if( length(list(...)) ){
        dots <- list(...)
        if( !is.null(dots$w)){
          w <- dots$w
          w <- round(w / x@dz)
        }
      }  
      if(w %% 2 == 1){
        w <- w + 1  # uneven window
      }
      w <- (w-1)/2
      x@data <-  apply(x@data,2,.medianFilter1D,w)
    }else if(type == "hampel"){
      w = 10    # argument initialization
      if( length(list(...)) ){
        dots <- list(...)
        if( !is.null(dots$w)){
          w <- dots$w
          w <- round(w / x@dz)
        }
      }
      A <- x@data
      if(length(dim(A))<2){
        A <- matrix(A,ncol=1,nrow=length(A))
      }
      X <- rbind(matrix(0,ncol=ncol(A),nrow=w), A, 
                matrix(0,ncol=ncol(A),nrow=w))
      n <- nrow(X)
      Y <- X
      for (i in (w + 1):(n - w)) {
        Xmed <- apply( X[(i - w):(i + w),,drop=FALSE],2, median)
        # S0 <- 1.4826 * apply( abs(X[(i - w):(i + w),,drop=FALSE] - Xmed),
        # 2, median)
        # test <- abs(X[i,] - Xmed) > x0 * S0
        # Y[i,test] <- Xmed[test]
        Y[i,] <- Xmed
      }
      x@data <- Y[(w+1):(n-w),]
    }else if(type == "Gaussian"){
      x@data <- mmand::gaussianSmooth(x@data, ...)
    }
    proc <- getArgs()
    x@proc <- c(x@proc, proc)
    return(x)
  } 
)

#----------------- SPATIAL-FILTER
#setMethod("adimproSmooth", "GPR", function(x,hmax=2,...){
#     IMG <- x@data
#     IMG <- (IMG-min(IMG))/(max(IMG)-min(IMG))
#     adimg <- make.image(IMG)
#     # img.smooth <- adimpro::awsimage(adimg, hmax = 2)
#     # img.smooth <- adimpro::awsimage(adimg, hmax = 2)
#     # img.smooth <- adimpro::awsaniso(adimg, hmax = 2,...)
#     img.smooth <- adimpro::awspimage(adimg, hmax = 2,...)
#     AA <- extract.image(img.smooth)
#     AAA <- (AA-mean(AA))/sd(AA)
#     x@data <- AAA
#     proc <- getArgs()
#    x@proc <- c(x@proc, proc)
#     return(x)
#  }
#)

#' Two-dimensional filters
#' 
#' @name filter2D
#' @rdname filter2D
#' @export
setMethod("filter2D", "GPR", function(x, type = c("median3x3"), ...){
    type <- match.arg(c("median3x3"))
    if(type == "median3x3"){
      x@data <-  .medianFilter3x3(x@data)
    }
    proc <- getArgs()
    x@proc <- c(x@proc, proc)
    return(x)
  } 
)

#----------------- CLIP/GAMMA/NORMALIZE
#' Clip the amplitude
#' 
#' @name clip
#' @rdname clip
#' @export
setMethod("clip", "GPR", function(x,Amax=NULL,Amin=NULL){
  x@data <- .clip(x@data,Amax,Amin)
  proc <- getArgs()
  x@proc <- c(x@proc, proc)
  return(x)
  } 
)
#' Gamma correction of the amplitude
#' 
#' @name gammaCorrection
#' @rdname gammaCorrection
#' @export
setMethod("gammaCorrection", "GPR", function(x,a=1,b=1){
  x@data <- .gammaCorrection(x@data,a,b)
  proc <- getArgs()
  x@proc <- c(x@proc, proc)
  return(x)
  } 
)

#' Trace scaling
#'
#' @name traceScaling
#' @rdname traceScaling
#' @export
setMethod("traceScaling", "GPR", function(x, 
            type = c("stat","min-max","95","eq","sum", "rms")){
    x@data <- scaleCol(x@data, type=type)
    proc <- getArgs()
    x@proc <- c(x@proc, proc)
    return(x)
  }
)

#----------------- FREQUENCY FILTERS
#' Frequency filter
#'
#' @name fFilter
#' @rdname fFilter
#' @export
setMethod("fFilter", "GPR", function(x, f = 100, type = 
c('low','high','bandpass'),L = 257, plotSpec = FALSE){
    x@data <- .fFilter1D(x@data, f = f,  type = type, L = L, dT = x@dz, 
                        plotSpec = plotSpec)
    proc <- getArgs()
    x@proc <- c(x@proc, proc)
    return(x)
  } 
)

#' Frequency-wavenumber filter
#'
#' @name fkFilter
#' @rdname fkFilter
#' @export
setMethod("fkFilter", "GPR", function(x, fk=NULL, L=c(5,5),npad=1){
    if(is.null(fk)) stop("fk argument has to be specified")
    # if polygon
    if(is.list(fk) && length(fk) == 2){
      areaunclosed <- t(do.call("rbind",area))
      nk <- npad*(nextpower2(ncol(x@data)))
      nf <- npad*(nextpower2(nrow(x@data)))
      # frequency
      Ts = x@dz*10^(-9)    # [s] Sample time
      fac = 1000000
      fre = (1/Ts)*seq(0,nf/2)/nf/fac
      
      # wavenumber
      Ks <- 1/x@dx      # [1/m] Sampling frequency
      knu <- 1:(nk/2)/(2*(nk/2)) * Ks  #[1/m]
      knutot <- c(-rev(knu),knu)
      fk <- outer(fre,knutot,inPoly,
                  vertx=areaunclosed[,2],
                  verty=areaunclosed[,1])
    }else if(!is.matrix(fk)){
      stop("fk should be either of type 'list' or 'matrix'\n")
    }else if(is.matrix(fk)){
      cat("# FIXME! function to transform matrix into polygon\n")
    }
    x@data <- .FKFilter(x@data,fk=fk, L=L, npad=npad)
    proc <- getArgs()
    x@proc <- c(x@proc, proc)
    return(x)
    
  } 
)



#--------------- CONVOLUTION/DECONVOLUTION


#' Phase rotation
#'
#' Rotate the phase of the GPR data by a given angle \code{phi}.
#' @param x A GPR data
#' @param phi A length-one numeric vector defining the phase rotation in radian.
#' @return The GPR data with rotated phase.
#' @name rotatePhase
#' @rdname rotatePhase
#' @export
setMethod("rotatePhase", "GPR", function(x, phi){
  # rotatePhase <- function(x,phi){
    x@data <- apply(x@data, 2, phaseRotation, phi)
    proc <- getArgs()
    x@proc <- c(x@proc,proc)
    return(x)
  }
)

#' Trace convolution (1D)
#'
#' Convolution of the GPR traces with a wavelet
#' @param x A GPR data
#' @param w A numeric vector defining a wavelet or a matrix with number of
#'           columns equal to the number of traces.
#' @return The convolved GPR data.
#' @name conv1D
#' @rdname conv1D
#' @export
setMethod("conv1D", "GPR", function(x, w){
  # rotatePhase <- function(x,phi){
    x@data <- convolution(x@data, w)
    x@proc <- c(x@proc,"conv1D")
    return(x)
  }
)
#' 2D onvolution
#'
#' Convolution of the GPR data with a kernel
#' @param x A GPR data
#' @param w A numeric matrix with smaller dimension than the GPR data.
#' @return The convolved GPR data.
#' @name conv2D
#' @rdname conv2D
#' @export
setMethod("conv2D", "GPR", function(x, w){
  # rotatePhase <- function(x,phi){
    x@data <- convolution2D(x@data, w)
    x@proc <- c(x@proc, "conv2D")
    return(x)
  }
)

#' Deconvolution
#'
#' A generic function to perform different types of convolution
#' 
#' @section Spiking and mixed-phase deconvolution:
#' The required arguments for \code{method = "spiking"} and 
#' \code{method = "mixed-phase"} are:
#' \itemize{
#'   \item \code{W}: A length-two numeric vector defining the time/depth window
#'                   for which the wavelet is estimated
#'   \item \code{wtr}: A length-one numeric vector defining the number of 
#'                     neighorough traces to be combine into a "super trace"
#'                     (the total number of traces is \code{2*wtr + 1}).
#'   \item \code{nf} A length-one numeric vector defining the filter length.
#'   \item \code{mu} A length-one numeric vector defining the amount of noise.
#' }
#' @section Wavelet deconvolution:
#' The required arguments for \code{method = "wavelet"} are:
#' \itemize{
#'   \item \code{h}: A numeric vector corresponding to the wavelet used to
#'                   deconvolve the GPR data.
#'   \item \code{mu} A length-one numeric vector defining the amount of noise.
#' }
#'
#' @param method Type of deconvolution method.
#' @param ... additional arguments, see \code{details}.
#' @return A list containing the deconvolued GPR data (and possibly other
#' variables.
#' @name deconv
#' @rdname deconv
#' @export
setMethod("deconv", "GPR", function(x, 
            method=c("spiking", "wavelet", "min-phase", "mixed-phase"),...){
    method <- match.arg(method, c("spiking", "wavelet", "min-phase",
                                  "mixed-phase"))
    toReturn <- list()
    if(method == "spiking" || method == "mixed-phase"){
    # deconvSpiking <- function(x,W,wtr,nf,mu){
      if( length(list(...)) ){
        dots <- list(...)
        if(is.null(dots$W)){
          stop(paste0("W must be defined\n"))
        }else{
          W <- dots$W
        }
        if(is.null(dots$wtr)){
          stop(paste0("wtr must be defined\n"))
        }else{
          wtr <- dots$wtr
        }
        if(is.null(dots$nf)){
          stop(paste0("nf must be defined\n"))
        }else{
          nf <- dots$nf
        }
        if(is.null(dots$mu)){
          stop(paste0("mu must be defined\n"))
        }else{
          mu <- dots$mu
        }
        if(is.null(dots$shft)){
          shft <- 1
        }else{
          shft <- dots$shft
        }
      }else{
        stop(paste0("spiking deconvolution requires the following arguments:",
                    "W,wtr,nf,mu\n"))
      }
      
      #if(missing(W) || missing(wtr) || missing(nf) || missing(mu)){
      #  stop(paste0("spiking deconvolution requires the following arguments:",
       #             "W,wtr,nf,mu\n"))
      #}
      #if(missing(shft)){
       # shft=1
      #}
      W <- seq(W[1],W[2])
      X <- traceScaling(x, type="rms")@data
      # X <- X / apply(as.matrix(X),2,RMS)
      Xdec <- matrix(nrow=nrow(X),ncol=ncol(X))
      Fmin <- matrix(nrow=nf,ncol=ncol(X))
      Wmin <- matrix(nrow=nf,ncol=ncol(X))
      for(i in 1:ncol(X)){
        ww <- (i-wtr):(i+wtr)
        ww <- ww[ww <= ncol(X) & ww >= 1]
        supertrace <- as.vector(X[W,ww])
        # inverse minimum-phase wavelet estimation # variante 1 (Emanuel)
        Fmin[,i] <- .spikingFilter(supertrace,nf=nf ,mu=mu, shft=shft)
        # Wmin[,i] <- deconv(c(1,rep(0,nf-1)),Fmin[,i], nf=nf,mu=mu)
        Wmin[,i] <- deconvolve(c(1,rep(0,nf-1)),Fmin[,i], mu=mu)
        # minimum-phase deconvolued data
        Xdec[,i] <- convolution(X[,i],Fmin[,i])[1:nrow(X)]
      }
      # estimated min-phase wavelet
      w_0 <- matrix(0, nrow=round(nf/3),ncol=ncol(Wmin))
      w_min <- list(x = seq(-round(nf/3),to =  nf , by = 1)*x@dz,
                    y = rbind(w_0, Wmin, rep(0, ncol(Wmin))))
      x@data <- Xdec
      toReturn <- list("fmin" = Fmin, "wmin" = w_min)
    }else if(method == "wavelet"){
      if(missing(h) || missing(mu)){
        stop(paste0("wavelet deconvolution requires the following arguments:",
                    "h, mu\n"))
      }
      x <- traceScaling(x, type="rms")
      x@data <- apply(x@data,2, deconvolve, h, mu)      
    }else if(method== "min-phase"){
      stop("min-phase deconvolution has to be first written!!!\n")
    }
    if(method == "mixed-phase"){
      # optimal phase shift
      phi <- optPhaseRotation(x@data[W,], rot = 0.05, plot=TRUE)
      # phase rotation
      x@data <-  apply(x@data, 2, phaseRotation, phi)
      # mixed phase wavelet
      w_mix <- w_min
      w_mix$y <- apply(w_min$y, 2 , phaseRotation, - phi)
      toReturn[["optRot"]] <- phi
      toReturn[["wmix"]] <- w_mix
    }
    # gprdec <- gpr
#     x@data <- Xdec
    proc <- getArgs()
    x@proc <- c(x@proc, proc)
    toReturn[["x"]] <- x
    return(toReturn)
  }
)

#--------------- DATA EDITING FUNCTIONS
#' Shift the traces
#'
#' @name traceShift
#' @rdname traceShift
#' @export
setMethod("traceShift", "GPR", function(x,  t0, keep = 10, delete0 = TRUE){
# traceShift <- function(x,t0, keep=10){
    if(length(t0) == 1){
      t0 <- rep(t0, ncol(x))
    }
    t0 <- t0/x@dz 
    if(min(t0) > keep){
      t0 <- t0 - keep
    }else{
      t0 <- t0 - min(t0) + 1
    }
    A <- x@data
    Anew <- matrix(nrow=nrow(A),ncol=ncol(A))
    
    minShift <- min(t0) - keep
    maxShift <- max(t0) - keep
    
    for(i in seq_along(A[1,])){
      vs <- seq(t0[i],nrow(A))
      vsp <- seq(t0[i],nrow(A))-t0[i]+1
      Anew[vsp,i] <- A[vs,i]
    }
    x@data <- Anew  
    if(delete0 == TRUE){
      test <- apply(abs(Anew),1,sum)
      firstPos <- which(!rev(test == 0) )[1]
      if(!is.na(firstPos)){
        n <- length(test)
        vsel <- 1:(n-firstPos + 1L)
        x <- x[vsel,]
      }
    }
#     x@proc <- c( x@proc, "trace shift")
    proc <- getArgs()
    x@proc <- c(x@proc, proc)
    return(x)
  }
)

#-------------------------------------------#
#---------------- SETMETHOD ----------------#
# Print methods
# setMethod("print", "GPR", function(x) print.GPR(x))
# > 1. helper function:
.GPR.print   <-  function(x, digits=5){
  topaste <- c(paste("***","Class GPR", "***\n"))
  topaste <- c(topaste, paste0("name = ", x@name, "\n"))
  if(length(x@filepath) > 0){
    topaste <- c(topaste, paste0("filepath = ", x@filepath, "\n"))
  }
  nbfid <- sum(trimStr(x@fid)!= "")
  if(nbfid > 0){
    topaste <- c(topaste, paste0(nbfid, " fiducial(s)\n"))
  }
  if(length(x@description) > 0){
    topaste <- c(topaste, paste0("description = ", x@description, "\n"))
  }
  if(length(x@date) > 0){
    topaste <- c(topaste, paste("survey date = ", x@date,"\n"))
  }
  topaste <- c(topaste, paste0(x@surveymode,", ",x@freq,"MHz,", 
                "Window length=",(nrow(x@data)-1)*x@dz, x@depthunit,
                ", dz=",x@dz,x@depthunit,"\n"))
  topaste <- c(topaste, paste0(ncol(x@data), " traces,", 
                diff(range(x@pos)),"",x@posunit," long\n"))
  if(length(x@proc)>0){
    topaste <- c(topaste, paste("> PROCESSING\n"))
    for(i in seq_along(x@proc)){
      topaste <- c(topaste, paste0("  ",i,". ", x@proc[i],"\n"))
    }      
  }
  topaste <- c(topaste, paste("****************\n"))
  return(topaste)      
}    

#' Print GPR
#'
#' @method print GPR 
#' @name print
#' @rdname show
#' @export
# > 2. S3 function:
print.GPR <- function(x, ...){
  jj <- .GPR.print(x, ...)
  cat(jj)
  return(invisible(jj))
}
# > 3. And finally a call to setMethod():
#' Show some information on the GPR object
#'
#' Identical to print().
#' @name show
#' @rdname show
#' @export
setMethod("show", "GPR", function(object){print.GPR(object)})   

#' Add a GPR trace on a plot
#'
#' @method lines GPR 
#' @name lines
#' @rdname lines
#' @export
lines.GPR <- function(x,...){
  if(length(x@vel)>0){  
    v <- x@vel[[1]]
  }else{
    v <- 0
  }
  if(any(dim(x) == 1)){
#     z <- seq(-x@time0, by = x@dz, length.out = length(x@data))
    z <- x@depth - x@time0
    lines(z, x@data,...)
   }else{
    stop("x must a vector!")
  }
}

#' Plot the GPR object.
#'
#' If the GPR object consists of a single trace, wiggle plot is shown.
#' @method plot GPR 
#' @name plot
#' @rdname plot
#' @export
# options: type=c(raster,wiggles), addTopo, clip, normalize
plot.GPR <- function(x,y,...){
  # type=c("raster","wiggles"),addTopo=FALSE,clip=NULL,normalize=NULL,
#       nupspl=NULL,...){
  # print(list(...))
  type <- "raster"
  addTopo <- FALSE
  normalize <- NULL
  nupspl <- NULL
  dots <- list()
  addAnn <- TRUE
  addFid <- TRUE
#   clim <- NULL
  clip <- NULL
  zlim <- NULL    # depth 
  main <- x@name
  if( length(list(...)) ){
    dots <- list(...)
    if( !is.null(dots$type)){
      type <- dots$type
      dots$type <- NULL
    }
    if( !is.null(dots$zlim)){
      zlim <- dots$zlim
      dots$zlim <- NULL
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
    if( !is.null(dots$main) ){
      main <- dots$main
      dots$main <- NULL
    }
    # myCol <- colGPR(n=101)
    # if( !is.null(dots$col) && !isTRUE(dots$col) ){
      # myCol <- dots$col
    # }
    if( !is.null(dots$addAnn) && !isTRUE(dots$addAnn) ){
      addAnn <- FALSE
    }
    if( !is.null(dots$addFid) && !isTRUE(dots$addFid) ){
      addFid <- FALSE
    }
    addTopo <- FALSE
    if( !is.null(dots$addTopo) && isTRUE(dots$addTopo) ){
      addTopo <- TRUE
    }
    #dots$col <- NULL
    dots$addAnn <- NULL
    dots$addFid <- NULL
    dots$addTopo <- NULL
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
    v <- x@vel[[1]]
  }else{
    v <- 0
  }
  if(any(dim(x) == 1)){
    par(mar=c(5,4,3,2)+0.1,oma=c(0,0,3,0), mgp=c(2, 0.5, 0))
    if(grepl("[m]$",x@depthunit)){
      xlab <- paste0("depth (",x@depthunit,")")
      z <- x@depth
    }else if(grepl("[s]$",x@depthunit)){
      xlab <- paste0("two-way travel time (",x@depthunit,")")
      z <- x@depth - x@time0
    }
#     z <- seq(-x@time0, by = x@dz, length.out = length(x@data))
    plot(z, x@data, type = "n", 
          xlab = xlab, 
          ylab = "amplitude (mV)",xaxt = "n")
    x_axis <- pretty(z,10)
      axis(side = 1, at = x_axis, labels = x_axis, tck = +0.02)
    if(grepl("[m]$",x@depthunit)){
      axis(side = 3, at = x_axis, labels = x_axis, tck = +0.02)
    }else if(grepl("[s]$",x@depthunit)){
      depth_0 <- depth0(0, v, antsep=x@antsep)
      depth2 <- seq(0.1,by=0.1,0.9)
      depthat0 <- depthToTime(0, 0, v, antsep=x@antsep)
      if(max(z)*v/2 > 1.3){
        depth <- pretty(seq(1.1,by=0.1,max(z)*v/2 ),10)
        depthat <- depthToTime(depth, 0, v, antsep=x@antsep)
        axis(side=3,at=depthat, labels=depth,tck=+0.02)
      }
      depthat2 <- depthToTime(depth2, 0, v, antsep=x@antsep)
      axis(side=3,at=depthat0, labels="0",tck=+0.02)
      axis(side=3,at=depthat2, labels=FALSE,tck=+0.01)
      axis(side=3,at=depthToTime(1, 0, v, antsep=x@antsep), 
            labels=FALSE,tck=+0.02)
      abline(v=depth_0,col="grey",lty=3)
      mtext(paste0("depth (m),   v=",v,"m/ns") ,side=3, line=2)
    }
    abline(h = 0, lty = 3, col = "grey")
    abline(v = 0, col = "red")
    lines(z, x@data,...)
    title(paste0(x@name, ": trace #", x@traces," @",x@pos,x@posunit),
                  outer=TRUE)
#     par(op)
   }else{
    if(!is.null(nupspl)){
      x <- upsample(x,n=nupspl)
    }
    if(!is.null(normalize)){
      x@data <- normalize(x@data,type=normalize)
    }
    # warning("First upsample then addTopo. 
#     Problem: interpolate also coord!!!")
    if(!is.null(clip) && is.numeric(clip)){
      if(length(clip) > 1){
        x@data <- .clip(x@data,clip[2],clip[1])
      }else if(length(clip)==1){
        x@data <- .clip(x@data,clip[1])
      }
    }
    if(addFid == FALSE){
      x@fid <- character(length(x@fid))
    }
    if(length(x@coord)>0){
      xvalues <- posLine(x@coord)
    }else{
      xvalues <- x@pos
    }    
    type <- match.arg(type, c("raster","wiggles"))
    if(type == "raster"){
      if(addTopo){
        x <- migration(x)
      }
      if(grepl("[m]$",x@depthunit)){
        ylab <- paste("depth (",x@depthunit,")",sep="")
      }else if(grepl("[s]$",x@depthunit)){
        ylab <- paste("two-way travel time (",x@depthunit,")",sep="")
      }
      yvalues <- -rev(x@depth)
      if(is.null(zlim)){
        zlim <- range(yvalue)
      }
#       if(length(x@coord) == 0){
#         x@coord <- matrix(0,nrow=ncol(x),ncol=3)
#         x@coord[,1] <- x@pos
#       }
#       xvalues <- posLine(x@coord)
#       if(is.null(clim)){
#         clim <- c(-1, 1) * max(abs(x@data), na.rm = TRUE)
#       }
      do.call(plotRaster, c(list(z = x@data, x = xvalues, y = yvalues, 
                     main = main, ylim = zlim,
                     xlab = x@posunit, ylab = ylab, note = x@filepath,
                     time_0 = x@time0, antsep = x@antsep, v = v, 
                     addFid = addFid, fid = x@fid, 
                     addAnn = addAnn, annotations = x@ann,
                     depthunit = x@depthunit, posunit = x@posunit), dots))
    }else if(type=="wiggles"){
      if(addTopo && length(x@coord)>0){
        topo <- x@coord[,3]
      }else{
        topo = NULL
      }
      if(grepl("[m]$",x@depthunit)){
        ylab <- paste("depth (",x@depthunit,")",sep="")
      }else if(grepl("[s]$",x@depthunit)){
        if(addTopo){
          ylab <- paste("depth (m)",sep="")
        }else{
          ylab <- paste("two-way travel time (",x@depthunit,")",sep="")
        }
      }
      yvalues <- -rev(x@depth)
      if(is.null(zlim)){
        zlim <- range(yvalue)
      }
#       if(length(x@coord)>0){
#         xvalues <- posLine(x@coord)
#       }else{
#         xvalues <- x@pos
#       }
      do.call(plotWig, c(list(z = x@data, x = xvalues, y = yvalues, 
                    main=main, ylim = zlim,
                    xlab = x@posunit, ylab = ylab, note = x@filepath, 
                    time_0 = x@time0, antsep = x@antsep, v = v, 
                    addFid = addFid, fid = x@fid,
                    addAnn = addAnn, annotations=x@ann,
                    depthunit=x@depthunit, posunit = x@posunit,
                    topo = topo), dots))
    }
  }
}




#' Three-dimensional plot of the GPR data with Open-GL
#'
#' @name plot3DRGL
#' @rdname plot3DRGL
#' @export
setMethod("plot3DRGL", "GPR", 
function(x,addTopo = FALSE, clip = NULL, normalize = NULL, 
        nupspl = NULL, add = TRUE, xlim=NULL, ylim=NULL, zlim=NULL,...){
    if(length(x@vel)>0){  
      velo <- x@vel[[1]]
    }else{
      velo <- 0
    }
    xsel <- rep(TRUE,length(x))
    if(!is.null(xlim)){
      xlim <- sort(xlim)
      xsel <- coord(x, 1) >= xlim[1] &  coord(x, 1) <= xlim[2]
    }
    ysel <- rep(TRUE,length(x))
    if(!is.null(ylim)){
      ylim <- sort(ylim)
      ysel <- coord(x, 2) >= ylim[1] &  coord(x, 2) <= ylim[2]
      cat(ylim,"  range=",range(coord(x, 2)),"\n")
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
    # warning("First upsample then addTopo. 
#     Problem: interpolate also coord!!!")
    if(!is.null(clip) && is.numeric(clip)){
      if(length(clip)>1){
        x@data <- .clip(x@data,clip[2],clip[1])
      }else if(length(clip)==1){
        x@data <- .clip(x@data,clip[1])
      }
    }
    if(length(x@coordref)!=3 ){
      refCoord <- apply(coord(x),2,min)
    }else{
      refCoord <-x@coordref
    }
    z0 <- coord(x, 3) - refCoord[3]
    if(addTopo){
      x <- migration(x)
      z0 <- rep(max(coord(x, 3)),length(x)) - refCoord[3]
    }
    cat(refCoord,max(coord(x, 3)),"\n")
    A <-as.matrix(x)
    # cat(refCoord,"\n")
    xpos <- coord(x, 1) - refCoord[1]
    ypos <- coord(x, 2) - refCoord[2]
    zpos <- x@depth
    if(add==FALSE){
      # rgl.open()
      open3d()
    }
    .plot3DRGL(A,xpos,ypos,zpos,z0,...)
  }
)

#' Plot the trace amplitude
#' 
#' Plot the amplitude estimated over the whole GPR data as a function of 
#'  time/depth.
#' 
#' @param x An object of the class GPR.
#' @param FUN A function to be applied on each row of the GPR data to 
#'            estimate the wave amplitude as a function of time/depth.
#' @param add A length-one boolean vector. If TRUE the amplitude is plotted
#'            on the previous plot. If FALSE (default) a new plot is created.
#' @param all A length-one boolean vector. If TRUE the logarithm of the 
#'              amplitude of every trace is ploted on the estimate amplitude.
#'              Default is FALSE.
#' processing functions with their arguments applied previously on the
#' GPR data.
#' @examples
#' data(frenkeLine00)
#' plotAmpl(frenkeLine00, FUN = median)
#' @name plotAmpl
#' @rdname plotAmpl
#' @export
setMethod("plotAmpl", "GPR", function(x, FUN = mean, add = FALSE, 
            all = FALSE, ...){
#   op <- par(no.readonly=TRUE)
  AMP <- apply(abs(x@data),1,FUN,...)
  z <- seq(0,by=x@dz,length.out=length(AMP))
  if(!add){
    par(mar=c(5, 4, 4, 2)+0.1)
      plot(z, log(AMP), type = "l", xlab = x@depthunit, ylab = "log(mV)", ...)
    if(all == TRUE){
      nothing <- apply(log(abs(x@data)), 2, lines, x = z,
                    col = rgb(0.2,0.2,0.2,7/max(ncol(A),7)))
    }
    title(x@name)
  }else{
    if(all == TRUE){
      nothing <- apply(log(abs(x@data)), 2, lines, x = z, 
                        col=rgb(0.2,0.2,0.2,7/max(ncol(A),7)))
    }
    lines(z, log(AMP), ...)
  }
#   par(op)
  } 
)


#' Return the amplitude spectrum of the GPR object.
#'
#' @name spec
#' @rdname spec
#' @export
setMethod("spec", "GPR", function(x, type = c("f-x","f-k"), plotSpec = TRUE, 
        unwrapPhase = TRUE, ...){
    type <- match.arg(type)
    if(type == "f-x"){
      S <- powSpec(x@data, dT = x@dz, fac = 1000000, 
                  plotSpec = plotSpec, titleSpec = x@name)
    }else if(type == "f-k"){
      S <- .FKSpectrum(x@data,dx=x@dx,dz=x@dz, plotSpec=plotSpec,...)
    }
    invisible(S)
#     return(S)
  } 
)

#' Interpolate the trace position.
#'
#' @param x An object of the class GPR.
#' @param topo A (mx4) numeric matrix, with m the number of traces 
#'				in x. The columns names of topo must be
#'				"E", "N", "Z" and "TRACE".
#' @param plot A length-one boolean vector. If TRUE some control
#'				plots are displayed.
#' @name interpPos 
#' @rdname interpPos
#' @export
setMethod("interpPos", "GPR", function(x, topo, plot = FALSE,...){
    # test if any measured points have a reference to a trace 
	# of the GPR-line
    if(all(is.na(topo[,"TRACE"]))){
      warning(paste0(x@name, ": no link between the measured points",
                    "and the GPR traces!\n"))
    }else{
      # if we have measured some points before the line start or 
      # after the end of the GPR Line, we delete them
      test <- !is.na(topo[,"TRACE"])
      trueEnd <- max(which(test==TRUE))
      trueBeg <- min(which(test==TRUE))
      topo <- topo[trueBeg:trueEnd,]
      #--- 3D topo Distance ---#
      dist3D <- posLine(topo[,c("N","E","Z")], last=FALSE)
      #--- INTERPOLATION GPR POSITION FROM TOTAL STATION ---#
      # if there are points measured with the total station
      # that do not have an fiducial (FID) > interpolate them!
      myWarning <- ""
      if(!all(test[trueBeg:trueEnd])){
        # dist3D[topo$PNAME %in% FID$PNAME] > distance for the points 
        # also recorded in FID
        myWarning <- "  points total station without fiducials"
        # cat("  points total station without fiducials")
        test <- !is.na(topo[,"TRACE"])
        if(sum(test)>2){
          interp_method <- "linear"
        }else{
          interp_method <- "linear"
        }
        FIDpos <- signal::interp1(x = dist3D[test], 
                                  y = x@pos[topo[test,"TRACE"]], 
                                  xi = dist3D,
                                  method = interp_method,
                                  extrap=TRUE)
      }else{
        FIDpos   <- x@pos[topo[test,"TRACE"]]
      }
      # now: FIDpos <=> topo

      #--- INTERPOLATION N,E,Z ---#
      posInt   <- signal::interp1(FIDpos,dist3D, x@pos, 
                      method ="linear" , extrap=TRUE)
      # 'pchip': piecewise cubic hermite interpolating polynomial 
      Nint   <- signal::interp1(dist3D,topo$N, posInt, 
                      method ="linear" , extrap=TRUE)
      Eint   <- signal::interp1(dist3D,topo$E, posInt, 
                      method ="linear", extrap=TRUE)
      if(length(dist3D)<= 2){
        Zint <-  signal::interp1(dist3D,topo$Z, posInt, 
                      method ="linear", extrap=NA)
      }else{
        Zint <-  signal::interp1(dist3D,topo$Z, posInt, 
                      method ="pchip", extrap=NA)
      }

      lastNA  <- max(which(!is.na(Zint)))
      firstNA <- min(which(!is.na(Zint)))
      if(firstNA > 1)  Zint[1:(firstNA-1)] <- Zint[firstNA]
      if(lastNA < length(Zint)){
        Zint[(lastNA+1):length(Zint)] <- Zint[lastNA]
      }
      cat(x@name,": mean dx=", round(mean(diff(posInt)),3), 
          "  range dx=",round(min(diff(posInt)),3),"-", 
          round(max(diff(posInt)),3))
      cat(myWarning)
      cat("\n")
	  if(plot == TRUE){
		  par(mfrow=c(1,3))
		  plot(FIDpos, dist3D, pch = 20, col = "red", cex = 2, asp = 1,
				xlab = "FID  position", ylab = "trace spacing (3D)",
		        xlim = range(x@pos), ylim=range(posInt), main=paste( x@name))
		  points(x@pos,posInt,pch=20,col="blue")
		  plot(posInt, Zint, type = "l", asp = 10, 
				xlab = "interpolated trace spacing", 
				ylab="interpolated elevation",
		        main = paste0(x@name, " min dx=",round(min(diff(posInt)),2), 
		        "  max dx=",round(max(diff(posInt)),2)))
		  points(dist3D, topo[,c("Z")],pch=20,col="red")
		  plot(topo[,c("E","N")], col = 1, type = "l", lwd = 2, asp = 1,
		        ylim = range(Nint), xlim = range(Eint), 
		        main = paste0(x@name, " mean dx=", round(mean(diff(posInt)),2)))
		  points(topo[,c("E","N")],col=1,pch=20,cex=2)
		  lines(Eint,Nint,col=2,lwd=2)
		  Sys.sleep(1)
      }
      A <- matrix(nrow=length(Nint),ncol=3)
      A[,1] <- Eint
      A[,2] <- Nint
      A[,3] <- Zint
      colnames(A) <- c("E","N","Z")
      x@coord <- A
      x@proc <- c(x@proc, "interpPos")
    }
    return(x)
  }
)

#' Relative trace position on the GPR profile.
#'
#' @name relPos
#' @rdname relPos
#' @export
setMethod("relPos", "GPR", function(x){
    return(posLine(x@coord))
  } 
)

#' Reverse the trace position.
#'
#' @name reverse
#' @rdname reverse
#' @export
setMethod("reverse", "GPR", function(x){
    xnew <- x
    xnew@data <- x@data[,length(x):1]
    # traces="numeric",  # trace number
    # depth="numeric",  # depth position
    # pos="numeric",    # position  of the traces          
    xnew@time0 <- rev(x@time0)
    xnew@time <- rev(x@time)
    xnew@fid <- rev(x@fid)
    xnew@ann <- rev(x@ann)
    if(length(x@coord)>0){
      xnew@coord <- x@coord[nrow(x@coord):1,]
    }
    if(length(x@rec)>0){
      xnew@rec <- x@rec[nrow(x@rec):1,]
    }
    if(length(x@trans)>0){
      xnew@trans <- x@trans[nrow(x@trans):1,]
    }
    proc <- getArgs()
    x@proc <- c(x@proc, proc)
    return(xnew)
  }
)

#---------------------- DELINEATIONS ---------------------#
#' Delineate structure on GPR data
#'
#' @name delineate
#' @rdname delineation
#' @export
setMethod("delineate", "GPR", 
function(x,name=NULL,type=c("raster","wiggles"),addTopo=FALSE,
        nupspl=NULL,n = 10000, ...){
    if(is.null(dev.list())){
      stop(paste0("You must first plot the GPR profile",
                  "with the function \"plot\"!\n"))
    }
    xsave <- x
    itp <- locator(type="l", n=n)
    if(length(itp)>0){
      if(length(x@vel)>0){  
        velo <- x@vel[[1]]
      }else{
        velo <- 0
      }
      if(!is.null(nupspl)){
        x <- upsample(x,n=nupspl)
      }
      topo <- rep(0,length(x))
      type=match.arg(type)
      if(type=="raster"){
        if(addTopo){
          x <- migration(x)
        }
        # yvalues <- -rev(x@depth) 
        yvalues <- -(x@depth) 
        yvalues <- yvalues + mean(x@time0)
      }else if(type=="wiggles"){
        yvalues <- -rev(x@depth) 
        if(addTopo){
          topo <- x@coord[,3]
          topo <- topo - max(topo)
          yvalues <- yvalues * velo/ 2
          time_0 <- mean(x@time0)
          depth_0 <- depthToTime(z=0, time_0, v=velo, antsep=x@antsep) * velo/ 2
          yvalues <- yvalues + depth_0
        }
      }
      if(length(x@coord) == 0){
        x@coord <- matrix(0,nrow=ncol(x),ncol=3)
        x@coord[,1] <- x@pos
      }
      xvalues <- posLine(x@coord)
      posxOnPlot <- sapply(itp$x, .whichMin, xvalues)
      posyOnPlot <- sapply(itp$y, .whichMin, yvalues)
      mySel <- posxOnPlot >= 0 & posxOnPlot <= length(x) & 
               posyOnPlot >= 0 & posyOnPlot <= nrow(x)
      posxOnPlot2 <- posxOnPlot[mySel]
      posPts <- posyOnPlot[mySel]
      posTrace <- x@traces[posxOnPlot2]
      xpos <- x@coord[posxOnPlot2,1]
      ypos <- x@coord[posxOnPlot2,2]
      # zpos <- x@coord[posPts,3]
      zpos <- itp$y[mySel]
      if(is.null(name)){
        xsave@delineations <- c(xsave@delineations, 
                    list(cbind(posTrace,posPts,xpos,ypos,zpos)))
      }else{
        name <- as.character(name)
        if(length(xsave@delineations[[name]])>0){
          xsave@delineations[[name]] <- c(xsave@delineations[[name]], 
                    list(cbind(posTrace,posPts,xpos,ypos,zpos)))
        }else{
          xsave@delineations[[name]] <- 
                    list(cbind(posTrace,posPts,xpos,ypos,zpos))
        }
      }
    }
    return(xsave)
  }
)

# add "manually" delineation to GPR data
# m - m or m - ns (depends on addTopo = FALSE/TRUE

#' @name addDelineation
#' @rdname delineation
#' @export
setMethod("addDelineation", "GPR", function(x,itp, 
name=NULL,type=c("raster","wiggles"),addTopo=FALSE,...){
    if(is.null(dev.list())){
      stop("You must first plot the GPR profile with the function \"plot\"!\n")
    }
    xsave <- x
    # itp <- locator(type="l", n=n)
    topo <- rep(0,length(x))
    type=match.arg(type)
    if(type=="raster"){
      if(addTopo){
        x <- migration(x)
      }
      # yvalues <- -rev(x@depth) 
      yvalues <- -(x@depth) 
      yvalues <- yvalues + mean(x@time0)
    }else if(type=="wiggles"){
      yvalues <- -rev(x@depth) 
      if(addTopo){
        topo <- x@coord[,3]
        topo <- topo - max(topo)
        yvalues <- yvalues * velo/ 2
        time_0 <- mean(x@time0)
        depth_0 <- depthToTime(z=0, time_0, v=velo, antsep=x@antsep) * velo/ 2
        yvalues <- yvalues + depth_0
      }
    }
    if(length(x@coord) == 0){
      x@coord <- matrix(0,nrow=ncol(x),ncol=3)
      x@coord[,1] <- x@pos
    }
    xvalues <- posLine(x@coord)      
    posxOnPlot <- sapply(itp$x, .whichMin, xvalues)
    posyOnPlot <- sapply(itp$y, .whichMin, yvalues)
    mySel <- posxOnPlot >= 0 & posxOnPlot <= length(x) & 
             posyOnPlot >= 0 & posyOnPlot <= nrow(x)
    posxOnPlot2 <- posxOnPlot[mySel]
    posPts <- posyOnPlot[mySel]
    posTrace <- x@traces[posxOnPlot2]
    xpos <- x@coord[posxOnPlot2,1]
    ypos <- x@coord[posxOnPlot2,2]
    # zpos <- x@coord[posPts,3]
    zpos <- itp$y[mySel]
    if(is.null(name)){
      xsave@delineations <- c(xsave@delineations, 
            list(cbind(posTrace,posPts,xpos,ypos,zpos)))
    }else{
      name <- as.character(name)
      if(length(xsave@delineations[[name]])>0){
        xsave@delineations[[name]] <- c(xsave@delineations[[name]], 
            list(cbind(posTrace,posPts,xpos,ypos,zpos)))
      }else{
        xsave@delineations[[name]] <- 
            list(cbind(posTrace,posPts,xpos,ypos,zpos))
      }
    }
    return(xsave)
  }
)

# Remove manually delineation from the GPR data
#
#' @name rmDelineations<-
#' @rdname delineation
#' @export
setReplaceMethod("rmDelineations", "GPR", function(x,values=NULL){
    deli <- x@delineations
    n_d <- length(deli)
    if(!is.null(values) && n_d >0 && values!="all"){
      n_tot <- sum(sapply(deli, .lengthList))
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
              if(length(x@delineations[[i]])==0 || 
                  is.null(unlist(x@delineations[[i]]))){
                x@delineations[i] <- NULL
                # i<-i-1
                break
              }
              # print(x@delineations[[i]])
            }
          }
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

# Show the list of delineation of the GPR data
#
#' @name delineations
#' @rdname delineation
#' @export
setMethod("delineations", "GPR", function(x,sel=NULL,...){
    deli <- x@delineations
    n_d <- length(deli)
    if(n_d >0){
      if(length(x@coord) == 0){
        x@coord <- matrix(0,nrow=ncol(x),ncol=3)
        x@coord[,1] <- x@pos
      }
      x_dist <- posLine(x@coord)
      cat("*** delineated lines ****\n")
      it <- 0
      for(i in 1:n_d){
        if(typeof(deli[[i]])=="list"){
          n_sub_d <- length(deli[[i]])
          cat(names(deli[i]), ":\n",sep="")
          for(j in 1:n_sub_d){
            it <- it + 1
            tracePos <- sapply( deli[[i]][[j]][,1], .which, x@traces)
            xpos <- x_dist[tracePos]
            zpos <- deli[[i]][[j]][,4]
            cat(it,". length = ", round(diff(range(xpos)),2), ";  depth = ",  
round(diff(range(zpos)),2),";  ", length(xpos), " pts" ,"\n",sep="")
            # lines(xpos, zpos,col=col[i],...)
          }
        }else{
          it <- it + 1
          tracePos <- sapply( deli[[i]][,1], .which, x@traces)
          xpos <- x_dist[tracePos]
          zpos <- deli[[i]][,4]
          cat(it,". length =", round(diff(range(xpos)),2), "; depth =",  
round(diff(range(zpos)),2),"; number of pts =", length(xpos) ,"\n",sep="")
          # lines(xpos, zpos,col=col[i],...)
        }
        cat("- - - - - - - - - - -\n")
      }
    }else{
      cat("No lines were delineated!\n")
    }
  }
)

# Export the coordinates of the delineations
#
#' @name exportDelineations
#' @rdname delineation
#' @export
setMethod("exportDelineations", "GPR", function(x, dirpath=""){
    if(length(x@coord) == 0){
      x@coord <- matrix(0,nrow=ncol(x),ncol=3)
      x@coord[,1] <- x@pos
    }
    x_dist <- posLine(x@coord)
    deli <- x@delineations
    z0 <- max(coord(x, 3)) 
    it <- 0
    for(i in seq_along(deli)){
      if(typeof(deli[[i]])=="list"){
        n_sub_d <- length(deli[[i]])
          for(j in n_sub_d:1){
            it<-it+1
            tracePos <- sapply( deli[[i]][[j]][,1], .which, x@traces)
            xprofile <- x_dist[tracePos]
            zabs <- z0 + deli[[i]][[j]][,5] 
            # zpos <- deli[[i]][[j]][,5]
            table_path_name <- paste0(dirpath, name(x), "_", it, "_", 
                                        names(deli[i]), ".txt")
            write.table(cbind(deli[[i]][[j]][, c("xpos","ypos","zpos")], zabs, 
                        xprofile),file=table_path_name, sep = ";", 
                        row.names = FALSE, 
                        col.names = c("x","y","zr","z","xprofile"))
          }
      }else{
        it<-it+1
        tracePos <- sapply( deli[[i]][,1], .which, x@traces)
        xprofile <- x_dist[tracePos]
        zabs <- z0 + deli[[i]][[j]][,5] 
        table_path_name <- paste0(dirpath, name(x), "_", it, "_", 
                                  names(deli[i]), ".txt")
        write.table(cbind(deli[[i]][[j]][, c("xpos","ypos","zpos")], zabs, 
                    xprofile), file = table_path_name, sep = ";", 
                    row.names = FALSE, 
                    col.names =  c("x","y","zr","z","xprofile"))
      }
    }
  }
)

# Plot the delineation on RGL
#
#' @name plotDelineations3D
#' @rdname delineation
#' @export
setMethod("plotDelineations3D", "GPR", 
function(x,sel=NULL,col=NULL,add=TRUE,...){
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
            tracePos <- sapply( deli[[i]][[j]][,1], .which, x@traces)
            xpos <- x@coord[tracePos,1] - x@coordref[1]
            ypos <- x@coord[tracePos,2] - x@coordref[2]
            z0 <- max(coord(x,3))   -   x@coordref[3]
            zpos <- z0 + deli[[i]][[j]][,5] 
            lines3d(ypos,zpos,xpos, col=col[i],...)
          }
        }else{
          tracePos <- sapply( deli[[i]][,1], .which, x@traces)
          xpos <- x@coord[tracePos,1] - x@coordref[1]
          ypos <- x@coord[tracePos,2] - x@coordref[2]
          z0 <- max(coord(x, 3))   -   x@coordref[3]
          zpos <- z0 + deli[[i]][,5] 
          lines3d(ypos,zpos,xpos, col=col[i],...)
        }
      }
    }
  }
)

# Plot the delineation on a 2D plot
#
#' @name plotDelineations
#' @rdname delineation
#' @export
setMethod("plotDelineations", "GPR", function(x,sel=NULL,col=NULL,...){
    if(is.null(dev.list())){
      stop("You must first plot the GPR profile with the function \"plot\"!\n")
    }
    deli <- x@delineations
    n_d <- length(deli)
    if(n_d >0){
      if(length(x@coord) == 0){
        x@coord <- matrix(0,nrow=ncol(x),ncol=3)
        x@coord[,1] <- x@pos
      }
      x_dist <- posLine(x@coord)
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
            tracePos <- sapply( deli[[i]][[j]][,1], .which, x@traces)
            xpos <- x_dist[tracePos]
            zpos <- deli[[i]][[j]][,5]
            lines(xpos, zpos,col=col[i],...)
          }
        }else{
          tracePos <- sapply( deli[[i]][,1], .which, x@traces)
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

# Identify the delineation on a 2D plot
#
#' @name identifyDelineation
#' @rdname delineation
#' @export
setMethod("identifyDelineation", "GPR", function(x,sel=NULL,...){
    if(is.null(dev.list())){
      stop("You must first plot the GPR profile with the function \"plot\"!\n")
    }
    XY <- list()
    deli <- x@delineations
    n_d <- length(deli)
    it <- 0
    if(n_d >0){
      if(length(x@coord) == 0){
        x@coord <- matrix(0,nrow=ncol(x),ncol=3)
        x@coord[,1] <- x@pos
      }
      x_dist <- posLine(x@coord)
      for(i in 1:n_d){
        if(typeof(deli[[i]])=="list"){
          n_sub_d <- length(deli[[i]])
          for(j in 1:n_sub_d){
            tracePos <- sapply( deli[[i]][[j]][,1], .which, x@traces)
            xpos <- x_dist[tracePos]
            zpos <- deli[[i]][[j]][,5]
            it <- it + 1
            XY[[it]] <- cbind(xpos,zpos,rep(it,length(xpos)))
            # it <- it + 1
            # lines(xpos, zpos,col=col[i],...)
          }
        }else{
          tracePos <- sapply( deli[[i]][,1], .which, x@traces)
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

#---------------------- MIGRATION & OFFSET CORRECTION---------------------#
# max_depth = to which depth should the migration be performed
# dz = vertical resolution of the migrated data
# fdo = dominant frequency of the GPR signal

#' Migration of the GPR data
#'
#' @name migration
#' @rdname migration
#' @export
setMethod("migration", "GPR", function(x,type=c("static","kirchhoff"),...){
#     if(missing(type)){
#       type=match.arg(type)
#       suppl_args <- list("type"=type)
#       # cat(type,"\n")
#     }
    type <- match.arg(type)
    if(type == "static"){  
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
        depth_0 <- depthToTime(z=0, time_0 , v=x@vel[[1]], 
                      antsep=x@antsep) *  x@vel[[1]]/ 2
        depth_all <- x@depth* x@vel[[1]]/ 2
        # shift to time0
        sel <- c(round((depth_0-depth_all[1])/x@dz):nrow(x))
        # sel <- c(round((depth_0)/x@dz):nrow(x))
        x <- x[sel,]
      }
      x@data <- .topoShift(x@data,topo,dz = x@dz)
      #x@depth * x@vel[[1]]/ 2
      x@depth <- seq(0,by=x@dz,length.out=nrow(x@data))  
      x@time0 <- rep(0,length(x@time0))
#       proc <- getArgs()
#       proc <- addArg(proc,suppl_args)
#       proc <- paste(proc,"#v=",x@vel[[1]],sep="")
      x@vel=list()  # FIX ME!!
      x@time0 <- rep(0L,ncol(x@data))  # FIX ME!!
#       x@proc <- c(x@proc, proc)
    }else if(type == "kirchhoff"){
      A <- x@data
      topoGPR <- x@coord[,3]
      dx <- x@dx
      dts <- x@dz
      v <- x@vel[[1]]
      # initialisation
      max_depth <- nrow(x)*x@dx
      dz <- 0.25*x@dz
      fdo <- x@freq
      if( length(list(...)) ){
        dots <- list(...)
        if( !is.null(dots$max_depth)){
          max_depth <- dots$max_depth
        }
        if( !is.null(dots$dz)){
          dz <- dots$dz
        }
        if( !is.null(dots$fdo)){
          fdo <- dots$fdo
        }
      }  
      x@data <- .kirMig(x@data, topoGPR = x@coord[,3], dx = x@dx, dts = x@dz, 
                        v = x@vel[[1]], max_depth = max_depth, dz = dz, 
                        fdo = fdo)
      x@depth <- seq(0,by=dz, length.out = nrow(x))
      x@time0 <- rep(0, ncol(x))
      x@dz <- dz
      x@depthunit <- "m"
    }
    proc <- getArgs()
    x@proc <- c(x@proc, proc)
    return(x)
  } 
)

#' Constant-offset correction (time) of the GPR data
#'
#' Time correction for each trace to compensate the offset between transmitter 
#' and receiver antennae (it converts the trace time of the data acquired with
#' a bistatic antenna system into trace time data virtually acquiered with 
#' a monostatic system under the assumption of horizontally layered structure).
#' @name timeCorOffset
#' @rdname timeCorOffset
#' @export
setMethod("timeCorOffset", "GPR", function(x){
  t0 <- mean(x@time0)
  x <- x[floor(t0/x@dz):nrow(x),]
  tcor2 <- (x@depth - mean(x@time0) + x@antsep/0.299)^2 - 
                  (x@antsep/x@vel[[1]])^2
  x <- x[tcor2 > 0,]
  tcor <- sqrt( tcor2[tcor2 > 0] )
  x@depth <- tcor
  x@time0 <- rep(0, ncol(x))
  x@proc <- c(x@proc, "timeCorOffset")
  return(x)
})


#---------------------- INTERPOLATION ---------------------#  
#' Up-sample the GPR data (sinc-interpolation)
#'
#' @name upsample
#' @rdname upsample
#' @export
setMethod("upsample", "GPR", function(x,n){
    n <- abs(round(n))
    if(length(n) == 1){
      n <- rep(n,2)
    }
    x@data <- .upsample(x@data, n=n, type=c("DFT"))
    x@data <- x@data[,1:(ncol(x@data))]
    yvalues <-  (seq(0,by=x@dz,length.out=nrow(x@data)))
    
    xvalues  <- .doubleVector(x@pos,n=n[2])
    yvalues  <- .doubleVector(x@depth,n=n[1])
    #  
    # image(xvalues,yvalues,t(x@data))

    ntr <- ncol(x@data)  # number of traces
    if(ntr != length(xvalues)) stop("ntr!=length(xvalues)")
    if(length(x@coord)>0){
      coord_new <- matrix(ncol=3,nrow=ntr)
      coord_new[,1] <- signal::interp1(x@pos, x@coord[,1], xi = xvalues,   
                                      method = c( "linear"), extrap = TRUE)
      coord_new[,2] <- signal::interp1(x@pos, x@coord[,2], xi = xvalues,   
                                      method = c( "linear"), extrap = TRUE)
      coord_new[,3] <- signal::interp1(x@pos, x@coord[,3], xi = xvalues,   
                                      method = c( "linear"), extrap = TRUE)
      x@coord <- coord_new
    }
    if(length(x@rec)>0){
      rec_new <- matrix(ncol=3,nrow=ntr)
      rec_new[,1] <- signal::interp1(x@pos, x@rec[,1], xi = xvalues,   
                                    method = c( "linear"), extrap = TRUE)
      rec_new[,2] <- signal::interp1(x@pos, x@rec[,2], xi = xvalues,
                                    method = c( "linear"), extrap = TRUE)
      rec_new[,3] <- signal::interp1(x@pos, x@rec[,3], xi = xvalues,   
                                    method = c( "linear"), extrap = TRUE)
      x@rec <- rec_new
    }
    if(length(x@trans)>0){
      trans_new <- matrix(ncol=3,nrow=ntr)
      trans_new[,1] <- signal::interp1(x@pos, x@trans[,1], xi = xvalues,   
                                      method = c( "linear"), extrap = TRUE)
      trans_new[,2] <- signal::interp1(x@pos, x@trans[,2], xi = xvalues,   
                                      method = c( "linear"), extrap = TRUE)
      trans_new[,3] <- signal::interp1(x@pos, x@trans[,3], xi = xvalues,   
                                      method = c( "linear"), extrap = TRUE)
      x@trans <- trans_new
    }
    
    x@traces <- seq.int(1L,by=1L,length.out=ntr)
    #fiducial markers (fid, comments)
    if(length(x@fid) >0){ #&& sum(x@fid != "")>0){
      newfid <- character(length(x@fid)*n[2])
      newfidPos <- which(x@fid!="")
      newfid[newfidPos*n[2]] <- x@fid[newfidPos]
      x@fid <- newfid[1:ntr]
    }
    #annotations
    if(length(x@ann) >0){ # && sum(x@ann != "")>0){
      newAnn <- character(length(x@ann)*n[2])
      newAnnPos <- which(x@ann!="")
      newAnn[newAnnPos*n[2]] <- x@ann[newAnnPos]
      x@ann <- newAnn[1:ntr]
    }
    # trace positions
    x@pos <- xvalues
    # depth/time
    x@depth <- .doubleVector(x@depth,n=n[1])

    x@dx <- x@dx / n[2]
    x@dz <- x@dz / n[1]
#     x@ntr <- ntr
    
    proc <- getArgs()
    x@proc <- c(x@proc, proc)
    return(x)
  } 
)


#----------------------- SAVE/EXPORT ------------------------#
#' Write the GPR object in a file.
#'
#' @name writeGPR
#' @rdname writeGPR
#' @export
setMethod("writeGPR", "GPR", function(x,fPath, format=c("DT1","rds"), 
overwrite=FALSE){
    type <- match.arg(format)
    splitBaseName <- unlist(strsplit(basename(fPath),'[.]'))
    ext <- tail(splitBaseName,1)
#     ext <-  tolower(substr(path,start=nchar(path)-3,stop=nchar(path)))
    if(isTRUE(overwrite)){
      cat("file may be overwritten\n")
    }else{
      fPath <- safeFPath(fPath)
    }
    if(type == "DT1"){
      if("dt1" != ext ){
        stop("Extension should be '.DT1'")
      }
      .writeDT1(x,fPath,overwrite)
    }else if(type == "rds"){
      if("rds" != ext ){
        stop("Extension should be '.rds'")
      }
      x@filepath <- as.character(fPath)
      namesSlot <- slotNames(x)
      xList <- list()
#       xList[["version"]] <- "0.1"
      for(i in seq_along(namesSlot)){
        xList[[namesSlot[i]]] <- slot(x, namesSlot[i])
      }
      saveRDS(xList, fPath)
    }
    #   mod2 <- readRDS("mymodel.rds")
  } 
)


# -------------------------------------------
# ------------writeDT1--------------------------
# -------------------------------------------
# @name  writeDT1 
# @description This function writes *.HD and associated *.DT1
# files  (Sensors & Software)

# @date 07.11.2012 08:33
# @auteur Emanuel Huber
# @param [text]    fileNameHD       (file path of *.hd file)
# @param [text]    fileNameDT1       (file path of *.dt1 file)


# @return list((hd = headerHD, dt1hd = headerDT1, data=myData))
# -------------------------------------------

.writeDT1 <- function(x, fPath, overwrite=FALSE){
  #-------------------------
  # DT1 FILE: traces
  traceData <- x@data  # should ranges between -32768 and 32767
  if(max(traceData)/max(abs(traceData))*32768 <= 32767){
    traceData <- round(traceData/max(abs(traceData))*32768)
  }else{
    traceData <- round(traceData/max(abs(traceData))*32767)
  }
#   cat(range(traceData),"\n")
  if(min(traceData)< -32768 || max(traceData) > 32768){
    stop("problem real > integer conversion")
  }
#   A
#   traceData <- A$data
  storage.mode(traceData) <- "integer"
  
  # DT1 FILE: header
  indexDT1Header=c("traces", "position", "samples","topo", "NA0", "bytes","dz", 
          "stacks","NA1","NA2", "NA3", "NA4", "NA5", "NA6", "recx","recy",
          "recz","transx","transy","transz","time0","zeroflag", "NA7", "time",
          "x8", "com","com1","com2","com3","com4","com5","com6")
  traces_hd <- list()
  traces_hd$traces <- x@traces
  traces_hd$position <- x@pos
  traces_hd$samples <- rep(nrow(x@data),ncol(x@data))
  if(length(x@coord) > 0 && sum(is.na(x@coord)) > 0){
    traces_hd$topo <- x@coord[,3]
  }else{
    traces_hd$topo <- rep.int(0L,ncol(x@data))
  }
  traces_hd$NA0 <- rep.int(0L,ncol(x@data))
#   traces_hd$NA0 <- A$dt1hd$NA1
  traces_hd$bytes <- rep.int(2L,ncol(x@data))
  traces_hd$dz <- rep(x@dz*1000,ncol(x@data)) # time window 
  traces_hd$stacks <- rep(as.numeric(x@hd$NUMBER_OF_STACKS),ncol(x@data))
  traces_hd$NA1 <- rep.int(0L,ncol(x@data))
  traces_hd$NA2 <- traces_hd$NA1 
  traces_hd$NA3 <- traces_hd$NA1 
  traces_hd$NA4 <- traces_hd$NA1 
  traces_hd$NA5 <- traces_hd$NA1 
  traces_hd$NA6 <- traces_hd$NA1 
  if(length(x@rec) > 0 && sum(is.na(x@rec)) > 0){
    traces_hd$recx <- x@rec[,1]
    traces_hd$recy <- x@rec[,2]
    traces_hd$recz <- x@rec[,3]
  }else{
    traces_hd$recx <- rep.int(0L,ncol(x@data))
    traces_hd$recy <- rep.int(0L,ncol(x@data))
    traces_hd$recz <- rep.int(0L,ncol(x@data))
  }
  if(length(x@trans) > 0 && sum(is.na(x@trans)) > 0){
    traces_hd$transx <- x@trans[,1]
    traces_hd$transy <- x@trans[,2]
    traces_hd$transz <- x@trans[,3]
  }else{
    traces_hd$transx <- rep.int(0L,ncol(x@data))
    traces_hd$transy <- rep.int(0L,ncol(x@data))
    traces_hd$transz <- rep.int(0L,ncol(x@data))
  }
  # traces_hd$time0 <- x@time0 
  traces_hd$time0 <-1+round(x@time0/x@dz,2)
  traces_hd$time0 <- traces_hd$NA1 
  traces_hd$zeroflag <- rep.int(0L,ncol(x@data)) 
  traces_hd$NA7 <- traces_hd$NA1 
  aa <-as.POSIXct(x@time[1], origin = "1970-01-01")
  bb <- format(aa, format = "%Y-%m-%d")
  myDay <- as.double(as.POSIXct(as.Date(bb), origin="1970-01-01"))
  traces_hd$time <- x@time - myDay
  traces_hd$x8 <- rep.int(0L,ncol(x@data)) 
  traces_hd$x8[trimStr(x@fid)!=""] <- 1L
  traces_hd$com <- x@fid 
  
  # FILE NAMES
  dirName   <- dirname(fPath)
  splitBaseName <- unlist(strsplit(basename(fPath),'[.]'))
  baseName   <- paste(splitBaseName[1:(length(splitBaseName)-1)],sep="")
  if(dirName == '.'){
    fPath <- baseName
  }else{
    fPath <- paste(dirName,'/',baseName,sep="")
  }
  if(isTRUE(overwrite)){
    cat("file may be overwritten\n")
  }else{
    fPath_orgi <- fPath
    k <- 0
    while(file.exists(paste(fPath,".DT1",sep="")) || 
            file.exists(paste(fPath,".HD",sep=""))){
      fPath <- paste(fPath_orgi,"_",k,sep="")
      k <- k+1
    }
  }
  
  
  # WRITE DT1 FILE
  dt1_file <- file(paste(fPath,".DT1",sep="") , "wb")
  for(i in 1:ncol(x@data)){
    for(j in 1:25){
      realData4 <- traces_hd[[j]][i]
#       realData4 <- A$dt1hd[[j]][i]
      storage.mode(realData4) <- "double"
      writeBin(realData4, dt1_file, size = 4)
    }
    comment28 <- as.character(traces_hd$com[i])
    # nnchar <- 28-nchar(comment28)
    com_add <- paste(c(rep(" ", 28-nchar(comment28)), comment28), 
                      sep = "", collapse = "")
    writeChar(com_add, dt1_file,nchars =28,eos = NULL)
    # suppressWarnings( writeChar(comment28, dt1_file,nchars = 28,eos = NULL))
    # for(k in 1:nnchar){
      # writeChar("^@", dt1_file)
    # }
    # two-byte integers
    writeBin(traceData[,i], dt1_file, size = 2)
  }
  close(dt1_file)
  
  # j<-0
  
  # j<-j+1
  # traces_hd[[indexDT1Header[j]]][i]
  
  #-------------------------
  # HD FILE: traces
  hd_file <- file(paste(fPath,".HD",sep="") , "w+")
  writeLines("1234", con = hd_file, sep = "\r\n")
  if(!is.null(x@hd$gprdevice)){
    writeLines(as.character(x@hd$gprdevice), con = hd_file, sep = "\r\n")
  }else{
    writeLines("Data from RGPR", con = hd_file, sep = "\r\n")
  }
  writeLines(as.character(x@date), con = hd_file, sep = "\r\n")
  writeLines(paste("NUMBER OF TRACES","= ", as.character(ncol(x@data)),sep=""), 
                  con = hd_file, sep = "\r\n")
  writeLines(paste("NUMBER OF PTS/TRC","= ",as.character(nrow(x@data)),sep=""), 
                  con = hd_file, sep = "\r\n")
  writeLines(paste("TIMEZERO AT POINT", "=",
                  as.character(1+round(mean(x@time0)/x@dz,2)), sep=""), 
                  con = hd_file, sep = "\r\n")
  writeLines(paste("TOTAL TIME WINDOW", "=", as.character(x@dz*(nrow(x@data))), 
                  sep=""), con = hd_file, sep = "\r\n")
  startpos <- 0
  if(!is.null(x@hd$startpos)){
    startpos <- x@hd$startpos
  }
  writeLines(paste("STARTING POSITION", "=", as.character(startpos), sep=""), 
                  con = hd_file, sep = "\r\n")
  endpos <- (ncol(x@data)-1)*x@dx
  if(!is.null(x@hd$endpos)){
    endpos <- x@hd$endpos
  }
  writeLines(paste("FINAL POSITION","=",as.character(endpos),sep=""), 
              con = hd_file, sep = "\r\n")
  writeLines(paste("STEP SIZE USED","=",as.character(x@dx),sep=""),
              con = hd_file, sep = "\r\n")
  writeLines(paste("POSITION UNITS","=","m",sep=""), 
              con = hd_file, sep = "\r\n")
  if(x@posunit != "m"){
    warning('Position units were defined as "metres"!\n')
  }
  writeLines(paste("NOMINAL FREQUENCY","=",as.character(x@freq),sep=""), 
              con = hd_file, sep = "\r\n")
  writeLines(paste("ANTENNA SEPARATION","=",as.character(x@antsep),sep=""), 
              con = hd_file, sep = "\r\n")
  pulservoltage <- 0
  if(!is.null(x@hd$PULSER_VOLTAGE_V)){
    pulservoltage <- x@hd$PULSER_VOLTAGE_V
  }
  writeLines(paste("PULSER VOLTAGE (V)", "=", as.character(pulservoltage), 
              sep=""), con = hd_file, sep = "\r\n")
  nstacks <- 1
  if(!is.null(x@hd$NUMBER_OF_STACKS)){
    nstacks <- x@hd$NUMBER_OF_STACKS
  }
  writeLines(paste("NUMBER OF STACKS","=",as.character(nstacks),sep=""), 
              con = hd_file, sep = "\r\n")
  writeLines(paste("SURVEY MODE","=",as.character(x@surveymode),sep=""), 
              con = hd_file, sep = "\r\n")
  
  if(length(x@hd) > 0){
    hdNames <- names(x@hd)
    hdNames <- hdNames[!(hdNames %in% 
c("startpos","endpos","NUMBER_OF_STACKS","PULSER_VOLTAGE_V","gprdevice"))]
    for(i in seq_along(hdNames)){
      hdName <- gsub("_",replacement=" ",as.character(hdNames[i]))
      hdName <- gsub("Serial",replacement="Serial#",hdName)
      hdName <- gsub("CAL tm",replacement="CAL (t/m)",hdName)
      
writeLines(paste(as.character(hdName), "=", as.character(x@hd[[hdNames[i]]]), 
              sep= ""), con = hd_file, sep = "\r\n")
    }
  }
  close(hd_file)
  return(fPath)
}
#-----------------
#' Export a PDF showing the GPR profile.
#'
#' @name exportPDF
#' @rdname exportPDF
#' @export
setMethod("exportPDF", "GPR", 
function(x, fPath = NULL, addTopo = FALSE, clip = NULL, normalize = NULL, 
        nupspl = NULL, ...){
    if(is.null(fPath)){
      stop("fPath must be given\n")
    }
    if(any(dim(x) == 1)){
      stop("no export because dim = 1\n")
    }
    plot(x, clip = clip, addTopo = addTopo, type = "wiggles", 
          pdfName = fPath, normalize = normalize, nupspl = nupspl, ...)
  }
)

#' Export fiducial markers
#' 
#' @name exportFid
#' @rdname exportFid
#' @export
setMethod("exportFid", "GPR", function(x,fPath=NULL){
    # Trace  Position  Comment  PNAME
    if(length(x@fid) > 0){
      tr_start <- 1
      tr_end <- length(x)
      tr <- which(fid(x) != "" & fid(x) != "skip")
      trfid <- fid(x)[tr]
      if(!(tr_start %in% tr)){  
        tr <- c(tr_start,tr)
        trfid <- c("START",trfid)
      }
      if(!(tr_end %in% tr)){
        tr <- c(tr,tr_end)
#         lastF <-regmatches(trfid[length(trfid)], 
#                         regexpr(pattern="[[:digit:]]+", 
#                         trfid[length(trfid)]))
#         if(length(lastF)>0){
#           trfid <- c(trfid, paste0("F", as.numeric(lastF)+1))
#         }else{
          trfid <- c(trfid,"END")
#         }
      }
      trpos <- x@pos[tr]
      FID <- data.frame("TRACE" = tr,"POSITION" = trpos, "COMMENT" = trfid)
      if(is.null(fPath)){
        return(FID)
      }else{
        write.table(FID, fPath, sep=",",row.names = FALSE, 
                    col.names = TRUE, quote=FALSE)
      }
    }else{
      if(length(x@name)>0){
        cat("No fiducials for",x@name,"\n")
      }else{
        cat("No fiducials\n")
      }
      return(NULL)
    }
  } 
)


#' Export the trace coordinates.
#'
#' @name exportCoord
#' @rdname exportCoord
#' @export
setMethod("exportCoord", "GPR", 
  function(x, type = c("SpatialPoints", "SpatialLines", "ASCII"),
  fPath = NULL, folder = NULL,  sep = "\t", driver = "ESRI Shapefile",...){
  type <- match.arg(type)
  if(is.null(fPath)){
    fPath <- x@name
  }
  if(is.null(folder)){
  	folder <- dirname(fPath)
  }
  fPath <- basename(fPath)
  if(type=="SpatialLines"){  
    mySpatLines <- as.SpatialLines(x)
    dfl <- data.frame(z=c(1), row.names = x@name)
    mySpatLinesdf <- sp::SpatialLinesDataFrame(mySpatLines, dfl , 
                      match.ID = TRUE)
    rgdal::writeOGR(mySpatLinesdf, folder, fPath, driver = driver,
                    check_exists = TRUE, overwrite_layer = TRUE,
                    delete_dsn = TRUE)
  }else if(type=="SpatialPoints"){  
    mySpatPoints <- as.SpatialPoints(x)
    rgdal::writeOGR(mySpatPoints, folder, fPath, driver = driver,
                    check_exists = TRUE, overwrite_layer = TRUE,
                    delete_dsn = TRUE)
  }else if(type == "points"){
    stop("use type = SpatialPoints instead.\n")
  }else if(type == "lines"){
    stop("use type = SpatialLines instead.\n")
  }else if(type == "ASCII"){
    xCoord <- x@coord
    colnames(xCoord) <- c("E","N","Z")
    fPath <- file.path(folder, paste0(.fNameWExt(fPath), ".txt"))
    write.table(x@coord, fPath, sep = sep, row.names = FALSE, 
                    col.names = TRUE, quote = FALSE)
  }
})

#' Export the process steps.
#'
#' @name exportProc
#' @rdname exportProc
#' @export
setMethod("exportProc", "GPR", function(x,fPath=NULL,sep="\t", row.names=FALSE,
  col.names=FALSE, ...){
  write.table(x@proc, file = fPath, row.names = row.names,
            col.names = col.names,...)
})
