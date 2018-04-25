#------------------------------------------#
#----------- CLASS DEFINITION -------------#

#' Class GPR
#' 
#' An S4 class to represent a ground-penetrating radar (GPR) data.
#' 
#' @section Survey mode:  Difference between reflection and CMP and WARR
#' \describe{
#'   \item{reflection}{The trace positions increase from trace to trace. The
#'                     antenna separation distance is constant.
#'                     Amplitude = f(depth, position)}
#'   \item{CMP}{The trace positions, \code{x@pos}, are constant and equal to 
#'              zero. The antenna separation distance increases increase from
#'              trace to trace.
#'              Amplitude = f(depth, antsep)}
#'   \item{WARR}{The trace positions, \code{x@pos}, are not constant because
#'              while the signal is emitted from the same position, the signal
#'              the signal is recorded from different positions. 
#'              The antenna separation distance increases increase from
#'              trace to trace.
#'              Is "CMP" a special case of "WARR"?
#'              Amplitude = f(depth, antsep)}
#'   \item{CMPAnalysis}{No trace positions, \code{x@pos = numeric(0)},
#'                      no antenna separation, \code{x@antsep = numeric(0)},
#'                      coherence/semblance = f(depth, velocity)}
#' }
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
#' @name GPR-class
#' @rdname GPR-class
#' @export
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
.gpr <- function(x, name = character(0), description = character(0),
                    fPath = character(0)){
  rec_coord <- cbind(x$dt1$recx, x$dt1$recy, x$dt1$recz)
  trans_coord <- cbind(x$dt1$transx, x$dt1$transy, x$dt1$transz)
  if(sum(is.na(rec_coord)) > 0){
    warning(paste(name,": ",sum(is.na(rec_coord)), 
                "NA's in the receiver coordinates\n"))
  }
  if(sum(is.na(trans_coord)) > 0){
    warning(paste(name,": ",sum(is.na(trans_coord)), 
                "NA's in the transmitter coordinates\n"))
  }
  if(sum(is.na(x$dt1$topo)) > 0){
    warning(paste(name,": ",sum(is.na(x$dt1$topo)), 
                "NA's in the topo coordinates\n"))
  }
  if(sum(abs(rec_coord),na.rm = TRUE) == 0 ){
    rec_coord <- matrix(nrow = 0, ncol = 0) 
  }
  if(sum(abs(trans_coord), na.rm = TRUE)== 0){
    trans_coord <- matrix(nrow = 0, ncol = 0) 
  }
  if(sum(abs(x$dt1$topo),na.rm=TRUE)== 0){
    coord <- matrix(nrow = 0, ncol = 0) 
  }else{
    coord <- matrix(0, nrow = ncol(x$data), ncol = 3)
    coord[,3] <- x$dt1$topo
  }
  #====== HEADER DATA (FILE *.HD) ======#
  pos_used <- integer(nrow(x$hd))
  ttw  <- .getHD(x$hd,"TOTAL TIME WINDOW", position = TRUE)
  if(!is.null(ttw)){
    dz <- ttw[1]/nrow(x$data)
    pos_used[ttw[2]] <- 1L
  }else{
    warning("time/depth resolution unknown! I take dz = 0.4 ns!\n")
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
  dx <- .getHD(x$hd, "STEP SIZE USED", position=TRUE)
  if(!is.null(dx)){
    pos_used[dx[2]] <- 1L
  }else{
    dx <- mean(diff(x$dt1hd$position))
  }
  posunit <- .getHD(x$hd, "POSITION UNITS",number=FALSE, position=TRUE)
  if(!is.null(posunit)){
    pos_used[as.numeric(posunit[2])] <- 1L
  }else{
    posunit <- "m"
  }
  freq <- .getHD(x$hd, "NOMINAL FREQUENCY", position=TRUE)
  if(!is.null(freq)){
    pos_used[freq[2]] <- 1L
  }else{
    freq <- 100
  }
  antsep <- .getHD(x$hd, "ANTENNA SEPARATION", position=TRUE)[1]
  if(!is.null(antsep)){
    pos_used[antsep[2]] <- 1L
  }else{
    antsep <- 1.00
  }
  surveymode = .getHD(x$hd, "SURVEY MODE",number=FALSE, position=TRUE)
  if(!is.null(surveymode)){
    pos_used[as.numeric(surveymode[2])] <- 1L
  }else{
    surveymode <- "reflection"
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
  sup_hd <- list()
  startpos <- .getHD(x$hd, "STARTING POSITION", position=TRUE)
  if(!is.null(startpos)){
    pos_used[startpos[2]] <- 1L
    sup_hd[["startpos"]] <- as.numeric(startpos[1])
  }else{
    startpos <- 0
  }
  endpos <- .getHD(x$hd, "FINAL POSITION", position=TRUE)
  if(!is.null(endpos)){
    pos_used[endpos[2]] <- 1L
    sup_hd[["startpos"]] <- as.numeric(endpos[1])
  }else{
    endpos <- dx[1]*ncol(x$data)
  }
  #--- survey date
  freepos <- which(pos_used[1:5] == 0)
  # 2014-04-10 (new PulseEkko format)
  yyyymmdd <- "^([0-9]{4})([^0-9])([0-9]{2})([^0-9])([0-9]{2})"
  # 10/06/2011 (old PulseEkko format)
  ddmmyyyy <- "^([0-9]{2})([^0-9])([0-9]{2})([^0-9])([0-9]{4})"
  surveyDate <- gsub(pattern ="[^0-9]", replacement = "-", 
                      x$hd[freepos, 2])
  testDate <- nchar(surveyDate) == 10 & 
              (grepl(yyyymmdd, surveyDate) | grepl(ddmmyyyy, surveyDate))
  if(any(testDate)){
    surveyDate <- surveyDate[testDate][1]
    if(grepl(yyyymmdd, surveyDate)){
      d <- as.character(as.Date(surveyDate, "%Y-%m-%d"))
      pos_used[which(testDate)[1]] <- 1L
    }else if(grepl(ddmmyyyy, surveyDate)){
      d <- as.character(as.Date(surveyDate, "%d-%m-%Y"))
      pos_used[which(testDate)[1]] <- 1L
    }
  }else{
    warnings("Could not understand the survey date\n")
    d <- character(0)
  }
  #--- device type
  freepos <- which(pos_used[1:5] == 0)
#   GPR_device <-  paste(x$hd[2,1],x$hd[2,2],sep="")
  GPR_device <-  x$hd[freepos, 2]
  testDevice <- grepl("^(Data.)", GPR_device)
  if(any(testDevice)){
    sup_hd[["gprdevice"]] <- GPR_device[testDevice][1]
    pos_used[which(testDevice)[1]] <- 1L
  }  
  x$hd2 <- x$hd[!pos_used,]
  if(nrow(x$hd2)>0){
    key <-  trimStr(x$hd2[,1])
    test <- key!=""
    key <- key[test]
    key2 <- gsub("[[:punct:]]", replacement = "", key)
    key2 <- gsub(" ", replacement = "_", key2)
    nameL <- trimStr(x$hd2[test,2])
    names(nameL) <- as.character(key2)
    sup_hd2 <- as.list(nameL)
    sup_hd <- c(sup_hd, sup_hd2)
  }
  dorigin <- d
  if(length(dorigin) == 0){
	dorigin <- "1970-01-01"
  }
  traceTime <- as.double(as.POSIXct(x$dt1$time, origin = as.Date(dorigin)))
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
        time = traceTime,                       # x$dt1$time
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
        hd = sup_hd                      # header
  )
}




.gprRD3 <- function(x, name = character(0), description = character(0),
                    fPath = character(0)){  
  #coord <- matrix(nrow = 0, ncol = 0) 
  #if(!is.null(x$coords)){
    #coord <- cbind(ll2dc(x$coords$latitude),
    #               ll2dc(x$coords$longitude),
    #               as.numeric(gsub('[^0-9.]', "", x$coords$height)))
    #coord <- cbind(x$coords$latitude,
    #               x$coords$longitude,
    #               x$coords$height)
  #}
  #====== HEADER DATA (FILE *.HD) ======#
  pos_used <- integer(nrow(x$hd))
  # OK
  ttw  <- .getHD(x$hd,"TIMEWINDOW", position = TRUE)
  if(!is.null(ttw)){
    dz <- ttw[1] / nrow(x$data)
    pos_used[ttw[2]] <- 1L
  }else{
    warning("time/depth resolution unknown! I take dz = 0.4 ns!\n")
    dz <- 0.4
    ttw  <- nrow(x$data) * dz
  }
  
  sup_hd <- list()
  # OK
  startpos <- .getHD(x$hd, "START POSITION", position=TRUE)
  if(!is.null(startpos)){
    pos_used[startpos[2]] <- 1L
    sup_hd[["startpos"]] <- as.numeric(startpos[1])
  }else{
    startpos <- 0
  }
  # OK
  endpos <- .getHD(x$hd, "STOP POSITION", position=TRUE)
  if(!is.null(endpos)){
    pos_used[endpos[2]] <- 1L
    sup_hd[["startpos"]] <- as.numeric(endpos[1])
  }else{
    endpos <- dx[1]*ncol(x$data)
  }
  
  # OK
  dx <- .getHD(x$hd, "DISTANCE INTERVAL", position=TRUE)
  if(!is.null(dx)){
    if(dx[1] == 0){
      dx <- (endpos[1] - startpos[1])/ncol(x$data)
    }
    pos_used[dx[2]] <- 1L
  }else{
    dx <- mean(diff(x$dt1hd$position))
  }
  
  # OK
  # X <- "GX450 HDR (v.1)=3" 
  X <- .getHD(x$hd, "ANTENNAS", position = TRUE, number = FALSE)
  X2 <- strsplit(X[1], " ")
  gsubwrap <- function(x, ...){
    grep('[0-9]{2,3}', x, value = TRUE)
  }
  freq <- as.numeric(gsub('[^0-9]', '', sapply(X2, gsubwrap)))
  # freq <- as.numeric(gsub('[^0-9]', '', freqS[1]))
  if(!is.null(freq) && !is.na(freq)){
    pos_used[X[2]] <- 1L
  }else{
    freq <- 100
  }
  # OK
  antsep <- .getHD(x$hd, "ANTENNA SEPARATION", position=TRUE)
  if(!is.null(antsep)){
    pos_used[antsep[2]] <- 1L
  }else{
    antsep <- 1.00
  }
  x$hd2 <- x$hd[!pos_used,]
  if(nrow(x$hd2)>0){
    key <-  trimStr(x$hd2[,1])
    test <- key!=""
    key <- key[test]
    key2 <- gsub("[[:punct:]]",replacement="",key)
    key2 <- gsub(" ",replacement="_",key2)
    nameL <- trimStr(x$hd2[test,2])
    names(nameL) <- as.character(key2)
    sup_hd2 <- as.list(nameL)
    sup_hd <- c(sup_hd, sup_hd2)
  }
  new("GPR",   version="0.1",
        data = byte2volt()*x$data,
        traces = 1:ncol(x$data),
        fid = character(0),
        #coord = coord,
        coord = matrix(nrow=0, ncol = 0),
        pos = seq(0, by = dx[1], length.out = ncol(x$data)),
        depth = seq(0, by = dz, length.out = nrow(x$data)),
        rec = matrix(nrow = 0, ncol = 0),
        trans = matrix(nrow = 0, ncol = 0),
        time0 = rep(0, ncol(x$data)),
        time = numeric(0),
        proc = character(0),
        vel = list(0.1),
        name = name,
        description = description,
        filepath = fPath,
        dz = dz, 
        dx = dx[1],
        depthunit = "ns",
        posunit = "m",
        freq = freq[1], 
        antsep = antsep[1], 
        surveymode = "reflection",
        date = character(0), #format(Sys.time(), "%d/%m/%Y"),
        crs = character(0),
        hd = sup_hd
  )
}
  
.gprSEGY <- function(x, name = character(0), description = character(0),
                    fPath = character(0)){  
  
  if( all(diff(x$hdt[4,]) > 0) || all(diff(x$hdt[5,]) > 0)){
    x_coord <- matrix(0, nrow = ncol(x$data), ncol = 3)
    x_coord[,1] <- x$hdt[4,]
    x_coord[,2] <- x$hdt[5,]
    x_pos <- posLine(x_coord)
    x_dx <- mean(diff(x_pos))
  }else{
    x_coord <- matrix(nrow = 0, ncol = 0)
    x_pos <- seq_len(ncol(x$data))
    x_dx <- 1
  }
  x_dz <- x$hd$TIME_SAMPLING
  x_depth <- seq(0, by = x_dz, length.out = x$hd$NB_SAMPLES)
  
  if(x$hd$POS_UNIT == "meter"){
    x_posunit <- "m"
  }else{
    x_posunit <- "feet"
    warning("Position unit 'feet' no yet implemented!")
  }
  if(length(name) == 0){
    x_name <- paste0("LINE", x$hd$LINE_NUMBER)
  }else{
    x_name <- name
  }
  # Antenna frequency
  antfreq0 <- grep("(antenna).*([0-9])+", x$hd$EBCDIC, ignore.case = TRUE, 
                    value = TRUE)
  antfreq <- as.numeric(gsub("[^0-9]", "", antfreq0))
  if(length(antfreq) > 0){
    ant <- list(f = c(12.5, 25, 50, 100, 110, 200, 225, 450,   900, 1200),
                s = c( 8,    4,  2,   1,   1, 0.5, 0.5, 0.25, 0.17, 0.075))
    antsep <- approx(ant$f, ant$s, xout = antfreq)$y
    message("Antenna separation (", antsep,
            " m) estimated from antenna frequency (", antfreq, " MHz).",
            "\nCorrect if wrong.")
  }else{
    antfreq <- numeric(0)
    antsep <- numeric(0)
    message("Please, add antenna frequency and antenna separation.")
  }
  # Date
  dd <- character(0)
  traceTime <- as.double(as.POSIXct(x$hdt[1,] * 3600 + x$hdt[2,] * 60 + 
                                    x$hdt[3,], origin = "1960-01-01"))
  dateSurvey <- grep("(date).*([0-9])+", x$hd$EBCDIC, ignore.case = TRUE, 
                     value = TRUE)
  dateSurvey2 <- gsub("date", "-", dateSurvey, ignore.case = TRUE)
  dateSurvey3 <- gsub("^(\\D)+", "", dateSurvey2)
  dateSurvey3 <- gsub("\\D", "/", dateSurvey3)
  if(length(dateSurvey3) > 0){
    dateSurvey4 <- strsplit(dateSurvey3, "\\D")[[1]]
    if(length(dateSurvey4) == 3){
      if(nchar(dateSurvey4[1]) == 4){
        dd <- as.Date(dateSurvey3, format = "%Y/%m/%d")
      }else if(nchar(dateSurvey4[3]) == 4){
        dd <- as.Date(dateSurvey3, format = "%d/%m/%Y")
      }else{
        dd <- as.Date(dateSurvey3, format = "%d/%m/%y")
      }
      traceTime <- as.double(as.POSIXct(x$hdt[1,] * 3600 + x$hdt[2,] * 60 + 
                                        x$hdt[3,], origin = dd))
    }
  }
  new("GPR",   version="0.1",
        data = byte2volt()*x$data,
        traces = 1:ncol(x$data),
        fid = character(0),
        #coord = coord,
        coord = x_coord,
        pos = x_pos,
        depth = x_depth,
        rec = matrix(nrow = 0, ncol = 0),
        trans = matrix(nrow = 0, ncol = 0),
        time0 = rep(0, ncol(x$data)),
        # time = x$hdt[1,] * 3600 + x$hdt[2,] * 60 + x$hdt[3,],
        time = traceTime,
        proc = character(0),
        vel = list(0.1),
        name = x_name,
        description = description,
        filepath = fPath,
        dz = x_dz, 
        dx = x_dx,
        depthunit = "ns",
        posunit = x_posunit,
        freq = antfreq, 
        antsep = antsep,     # check
        surveymode = "reflection",
        date = as.character(dd), #format(Sys.time(), "%d/%m/%Y"),
        crs = character(0),
        hd = x$hd
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
setMethod("readGPR", "character", function(fPath, desc = ""){
    ext <- .fExt(fPath)
    # DT1
    #if(file.exists(fPath)){
      if("DT1" == toupper(ext)){
        name <- .fNameWExt(fPath)
        A <- readDT1(fPath)
        x <- .gpr(A, name = name, fPath = fPath, description = desc)
        #return(x)
      }else if("rds" == tolower(ext)){
        x <- readRDS(fPath)
        if(class(x)=="GPR"){
          x@filepath <- fPath
          #return(x)
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
            rec = x[['rec']],                # x$dt1$recx,x$dt1$recy,x$dt1$recz
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
          x <- y
          #return(y)
        }
      }else if("RD3" == toupper(ext)){
        name <- .fNameWExt(fPath)
        A <- readRD3(fPath)
        x <- .gprRD3(A, name = name, fPath = fPath, description = desc)
        #return(x)
      }else if("SGY" == toupper(ext) || "SEGY" == toupper(ext)){
        name <- .fNameWExt(fPath)
        A <- readSEGY(fPath)
        x <- .gprSEGY(A, name = name, fPath = fPath, description = desc)
        #return(x)
      }else{
        stop(paste0("File extension not recognised!\n",
                    "Must be '.DT1', '.rd3', 'sgy', 'segy' or '.rds'"))
      }
      if(grepl("CMP", x@surveymode)){
        x@surveymode <- "CMP"
        if(length(x@rec) == 0 || length(x@trans) == 0){
          x@antsep <- seq(x@antsep, by = x@dx, length.out = length(x))
        }else{
          x@antsep <- sqrt(colSums((x@rec - x@trans)^2))
        }
      }
      return(x)
    #}else{
    #  stop(fPath, "does not exist!")
    #}
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
  names(myPoints) <- c("E", "N", "Z")
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
    myArg <- as.list(match.call(definition = sys.function(-2),
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
              description = paste0("coercion of ", as.character(d_name), 
                                  " (",typeof(x), ") into GPR"),
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

# getGroupMembers("Math")
#' Basic mathematical functions
#'
#' Methods for the base Math methods \link[methods]{S4groupGeneric}
#' @param x An object of the class RGPR.
#' @details Currently implemented methods include:
#' \itemize{
#'  \item{"abs", "sign", "sqrt", "ceiling", "floor", "trunc",
#'        "exp", "expm1", "log", "log10", "log2", "log1p", "cos",
#'        "cosh", "sin", "sinh", "tan", "tanh"}
#'  }
#' @examples
#' data(frenkeLine00)
#' A <- exp(frenkeLine00)
#' @rdname Math-methods
#' @aliases Math-GPR-method
setMethod(
  f="Math",
  signature="GPR",
  definition=function(x){
    x@data <- switch(.Generic,
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
    proc(x) <- getArgs()
    return(x)
  }
)

# ?groupGeneric
# > getGroupMembers("Arith")
# [1] "+"   "-"   "*"   "^"   "%%"  "%/%" "/" 
.GPR.add <- function(a, b){
  #FIXME #TODO: case where a (or b) is a vector trace
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
  if(!is.null(dim(a)) && !is.null(dim(b))){
    if(dim(a)[2] == 1 && dim(b)[2] > 1){
      a <- as.vector(a)
    }else if(dim(b)[2] == 1 && dim(a)[2] > 1){
      b <- as.vector(b)
    }
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
#' @rdname Arith-methods
#' @aliases Arith,GPR,ANY-method
setMethod(
  f= "Arith",
  signature=c(e1="GPR",e2="ANY"), 
  definition=.GPR.arith
)

#' @name Arith
#' @rdname Arith-methods
#' @aliases Arith,GPR,GPR-method
setMethod(
  f= "Arith",
  signature=c(e1="GPR",e2="GPR"), 
  definition=.GPR.arith
)
#' @name Arith
#' @rdname Arith-methods
#' @aliases Arith,ANY,GPR-method
setMethod(
  f= "Arith",
  signature=c(e1="ANY",e2="GPR"), 
  definition=.GPR.arith
)





#------------------------------
# ###' @rdname GPR-extract-methods
#' extract parts of GPR
#'
#' Object of the class GPR can be manipulated as matrix
#' @param x Object of class GPR
#' @param i integer
#' @param j integer
#' @name GPR-subset
#' @docType methods
#' @rdname GPR-subset
setMethod(
  f= "[",
  signature="GPR",
  definition=function(x,i,j,drop){
    rval <- x@data
    if(missing(i) && missing(j)){
      return(as.vector(x@data))
    }
    if(missing(i) || length(i) == 0) i <- seq_len(nrow(rval))
    if(length(dim(rval)) == 2) {
      # drop. <- ifelse(length(i) == 1, FALSE, drop)
      drop <- FALSE
      if(missing(j)){
        # rval <- rval[i, , drop = drop.]
        rval <- rval[i, , drop = drop]
        x@depth <- x@depth[i]
      }else { 
        if(length(j) == 0) j <- seq_len(ncol(rval))
        # rval <- rval[i, j, drop = drop.]
        rval <- rval[i, j, drop = drop]
        x@depth <- x@depth[i]
        x@traces <- x@traces[j]
        x@pos <- x@pos[j]
        x@time0 <- x@time0[j]
        x@time <- x@time[j]
        x@fid <- x@fid[j]
        if(length(x@antsep)>0)  x@antsep <- x@antsep[j]
        if(length(x@coord)>0)  x@coord <- x@coord[j,,drop=FALSE]
        if(length(x@rec)>0) x@rec <- x@rec[j,,drop=FALSE]
        if(length(x@trans)>0) x@trans <- x@trans[j,,drop=FALSE]
        if(!is.null(x@hd$startpos) && !is.null(x@hd$endpos)){
          if( !is.na(x@hd$startpos) && !is.na(x@hd$endpos) ){
            trpos <- seq(x@hd$startpos, x@hd$endpos,by=x@dx)
            x@hd$endpos <- trpos[j[length(j)]]
            x@hd$startpos <- trpos[j[1]]
          }else{
            x@hd$startpos <- NULL
            x@hd$endpos <- NULL
          }
        }
      }
      if(drop && length(rval) == 1){ rval <- c(rval)}
    }else if(length(i) > 0){
      rval <- rval[i]
      x@depth <- x@depth[i]
    }
    x@dz <- abs(mean(diff(x@depth)))
    x@dx <- abs(mean(diff(x@pos)))
    x@data <- rval
    return(x)
  }
)

#-------------------------------
#' @name [<-
#' @rdname GPR-subset
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

#' Return data header
#'
#' Return data header
#' @rdname gethd
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
    x@name <- value
    x@proc <- c(x@proc, "name<-")
    return(x)
  }
)

#' Depth unit of the GPR data
#' 
#' @name depthunit
#' @rdname depthunit
#' @export
setMethod("depthunit", "GPR", function(x){
  return(x@depthunit)
} 
)

#' @name depthunit<-
#' @rdname depthunit
#' @export
setReplaceMethod(
  f="depthunit",
  signature="GPR",
  definition=function(x,value){
    value <- as.character(value)[1]
    x@depthunit <- value
    x@proc <- c(x@proc, "depthunit<-")
    return(x)
  }
)

#' Position unit of the GPR data
#' 
#' @name posunit
#' @rdname posunit
#' @export
setMethod("posunit", "GPR", function(x){
  return(x@posunit)
} 
)

#' @name posunit<-
#' @rdname posunit
#' @export
setReplaceMethod(
  f="posunit",
  signature="GPR",
  definition=function(x,value){
    value <- as.character(value)[1]
    x@posunit <- value
    x@proc <- c(x@proc, "posunit<-")
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
  definition=function(x, value){
    x@description <- as.character(value)[1]
    x@proc <- c(x@proc, "description<-")
    return(x)
  }
)

#' Filepath of the GPR data
#' 
#' @rdname filepath-methods
#' @aliases filepath,GPR-method
setMethod("filepath", "GPR", function(x){
    return(x@filepath)
  } 
)

#' @rdname filepath-methods
#' @aliases filepath<-,GPR-method
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
  definition=function(x,value){
    vals <- value
    if(!is.matrix(value)){
      vals <- matrix(value,nrow=1,ncol=length(value))
    }
    traces <- (value[,1])
    annnames <- as.character(value[,2])
    valuesList <- (tapply(annnames, traces, identity))
    test <- unlist(lapply(valuesList, paste, sep = "", collapse = "#"), 
                    use.names = FALSE)
    x@ann <- character(length(x))
    x@ann[as.numeric(names(test))] <- test
    x@proc <- c(x@proc, "ann<-")
    # FIXME > sapply(test, trimStr)
    return(x)
  }
)

#' Coordinates of the GPR data
#' 
#' @rdname coord-methods
#' @aliases coord,GPR-method
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


#' @rdname coord-methods
#' @aliases coord<-,GPR-method
setReplaceMethod(
  f="coord",
  signature="GPR",
  definition=function(x,value){
    value <- as.matrix(value)
    if(ncol(x@data) == nrow(value) && ncol(value) == 3){
      x@coord <- value
      x <- trRmDuplicates(x)
      x@proc <- c(x@proc, "coord<-")
    }else{
      stop("Dimension problem!!")
    }
    return(x)
  }
)

#' Survey date
#' 
#' Return NULL if no date exists, else an object of the class 'Date'
#' @name svDate
#' @rdname svDate
#' @export
setMethod("svDate", "GPR", function(x){
    if(length(x@date) > 0){
	  return(as.Date(x@date))
    }else{
	  return(NULL)
	}
  } 
)

#' @param x An object of the class 'GPR'
#' @param value An object of the class 'Date'
#' @name svDate<-
#' @rdname svDate
#' @export
setReplaceMethod(
  f="svDate",
  signature="GPR",
  definition=function(x,value){
    value <- value[1]
    if(class(value) == "Date"){
      x@date <- as.character(value)
      x@proc <- c(x@proc, "svDate<-")
      return(x)
    }else{
      stop("'value' must be from the class 'Date'!")
	}
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
  if(length(x@vel) == 1){
    return(x@vel[[1]])
  }else{
    return(x@vel)
  }
} 
)

#' @name vel<-
#' @rdname vel
#' @export
setReplaceMethod(
  f="vel",
  signature="GPR",
  definition=function(x, value){
    if(typeof(value) != "list"){
      value <- list(as.numeric(value))
    }
    x@vel <- value
    x@proc <- c(x@proc, "vel<-")
    return(x)
  }
)

setMethod("trTime", "GPR", function(x){
    dorigin <- x@date
    if(length(dorigin) == 0){
      dorigin <- "1970-01-01"
    }
    traceTime <- as.double(as.POSIXct(x@time, origin = as.Date(dorigin)))
    return(traceTime)
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
#' @seealso \code{\link{firstBreak}} to estimate the first wave break.
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

#' Depth/time of the GPR data
#' 
#' @name depth
#' @rdname depth
#' @export
setMethod("depth", "GPR", function(x){
  return(x@depth)
} 
)
#' @name depth<-
#' @rdname depth
#' @export
setReplaceMethod(
  f = "depth",
  signature = "GPR",
  definition = function(x, value){
    if(length(value) == length(x@depth)){
      x@depth <- value
    }else{
      stop("length(value) != length(x@depth)")
    }
    x@proc <- c(x@proc, "depth<-")
    return(x)
  }
)

#' Position of the GPR traces
#' 
#' @name pos
#' @rdname pos
#' @export
setMethod("pos", "GPR", function(x){
  return(x@pos)
} 
)
#' @name pos<-
#' @rdname pos
#' @export
setReplaceMethod(
  f = "pos",
  signature = "GPR",
  definition = function(x, value){
    if( length(x@coord) > 0 ){
      warning(c("'pos' will not be used for display because ",
                "'coord' already exists"))
    }
    if(length(value) == length(x@pos)){
      x@pos <- value
    }else{
      stop("length(value) != length(x@depth)")
    }
    x@proc <- c(x@proc, "pos<-")
    return(x)
  }
)

#------------------------------------------------------------------------------#
# FIXME - START
#' Antenna separation
#' 
#' For common-offset GPR data, the antenna separation is constant.
#' For multi-offset GPR data (e.g., CMP, WARR), the antenna separation increase
#' from trace to trace.
#' @name antsep
#' @rdname antsep
#' @export
setMethod("antsep", "GPR", function(x){
  return(x@antsep)
} 
)
#' @name antsep<-
#' @rdname antsep
#' @export
setReplaceMethod(
  f = "antsep",
  signature = "GPR",
  definition = function(x, value){
    if(isCMP(x)){
      if(length(value) != ncol(x)){
        stop("length(value) != ncol(x)")
      }else{
        x@antsep <- value
      }
    }else{
      # not CMP, not WARR => common-offset
      if(length(value) > 1) warning("Only first element is used!")
      x@antsep <- value[1]
    }
    x@proc <- c(x@proc, "antsep<-")
    return(x)
  }
)

#' Survey mode of the GPR data
#' 
#' @name surveymode
#' @rdname surveymode
#' @export
setMethod("surveymode", "GPR", function(x){
  return(x@surveymode)
} 
)

#' @name surveymode<-
#' @rdname surveymode
#' @export
setReplaceMethod(
  f="surveymode",
  signature="GPR",
  definition=function(x, value){
    value <- as.character(value)[1]
    # define a function surveymodeValues() that return a character vector
    if(toupper(value) %in% c("CMP", "REFLEXION", "WARR", "CMPANALYSIS", 
                             "CMP/WARR")){
      x@surveymode <- value
    }else{
      stop("value must be either 'CMP', 'reflexion', 'WARR', 'CMPanalysis',
           or 'CMP/WARR'")
    }
    x@proc <- c(x@proc, "surveymode<-")
    return(x)
  }
)

#' Return TRUE is surveymode is CMP
#' 
#' @name isCMP
#' @rdname isCMP
#' @export
setMethod("isCMP", "GPR", function(x){
  (grepl("CMP", toupper(x@surveymode)) || 
    grepl("WARR", toupper(x@surveymode))) && 
      !grepl("CMPANALYSIS", toupper(x@surveymode))
} 
)



#' Fiducial markers of the GPR data
#' 
#' @name fid<-
#' @rdname fid
#' @export
setReplaceMethod(
  f="fid",
  signature="GPR",
  definition=function(x,value){
    value <- as.character(value)
    x@fid <- value
    x@proc <- c(x@proc, "fid<-")
    return(x)
  }
)

# CHECK ME : COMPARE FID<- with ANN<- (!!!!!)
#' @name fid
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
setMethod("ampl", "GPR", function(x, FUN = mean, ...){
    x@data[] <- apply(abs(x@data), 1, FUN, ...)
    # funName <- getFunName(FUN)
    # proc(x) <- getArgs( addArgs = c('FUN' = funName))
    proc(x) <- getArgs()
    return(x)
  } 
)

#' DEPRECATED - Processing steps applied to the data
#' 
#' DEPRECATED - use \code{proc} instead!
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
    warning("DEPRECATED! Use 'proc()' instead!")
    return(x@proc)
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
#' proc(A)
#' @name proc
#' @rdname proc
#' @export
setMethod("proc", "GPR", function(x){
    return(x@proc)
  } 
)

#' Add a processing step
#' 
#' @name proc
#' @rdname proc
#' @export
setReplaceMethod(
  f="proc",
  signature="GPR",
  definition=function(x,value){
    value <- as.character(value)
    x@proc <- c(x@proc, value)
    return(x)
  }
)

#================= PROCESSING ===============#
#----------------- DC-SHIFT
#' Direct-Current shift removal
#' 
#' The direct-current shift is estimated for each traces based on a specified 
#' number of time samples (normally the samples before time-zero). Then, the
#' direct-current shift of every trace is substracted from every trace.
#' @param x An object of the class `GPR`.
#' @param n [\code{integer(1)}]\cr
#'   My argument.
#'   Default is 1.
#' @param u Number of time samples used to evaluate the DC-shift. 
#' @param FUN A function to apply on the first `u` time samples (default is 
#' `mean`; alternatively `median` could be used or any user defined function).
#' @name dcshift
#' @rdname dcshift
#' @export
setMethod("dcshift", "GPR", function(x, u, FUN = mean){
    shift <- matrix(apply(x[u,],2, FUN), nrow = nrow(x), 
                    ncol=ncol(x), byrow = TRUE)
    x <-  x - shift
    # funName <- getFunName(FUN)
    # proc(x) <- getArgs(addArgs = c('FUN' = getFunName(FUN)))
    proc(x) <- getArgs()
    # proc(x) <- paste0("dcshift>u=", head(u,1),":",tail(u,1), "+", 
                      # "FUN=",funName)
    return(x)
  } 
)

#----------------- FIRST-BREAK
#' First wave break
#'
#' Pick the time corresponding to the first break of each trace in the GPR profile.
#' Return a vector containing the first break times.
#'
#' @param x An object of the class \code{GPR}
#' @param method A length-one character vector. \code{"coppens"} corresponds to
#'              the modified Coppens method, \code{"threshold"} to the 
#'              threshold method, and \code{"MER"} to the modified energy ratio
#'              method.
#' @param thr A length-one numeric vector defining the threshold  signal 
#'              amplitude (in \%) at which time zero is picked (only for the
#'              threshold method).
#' @param w A length-one numeric vector defining the length of leading window 
#'          (only for the modified Coppens and modified energy ratio 
#'          methods). Recommended value: about one period of the first-arrival 
#'          waveform. w is defined on a time basis.
#' @param ns A length-one numeric vector defining the length of the edge 
#'           preserving smoothing window (only for the modified Coppens 
#'           method). Recommended value: between one and two signal periods.
#'           When \code{ns = NULL} the value of \code{ns} is set to 
#'           \code{1.5 * w}.
#' @param bet A length-one numeric vector defining the stabilisation 
#'            constant (only for the modified Coppens method). Not critical. 
#'            When \code{bet = NULL} the value of \code{bet} is set to 
#'            20\% of the maximal signal amplitude. 
#' @seealso \code{\link{time0}} to set time zero and 
#'          \code{\link{time0Cor}} to shift the traces such that they start
#'          at time zero.
#' @references
#' \describe{
#'   \item{Modified Coppens method}{Sabbione J.I. and Velis D. (2010) 
#'        Automatic first-breaks picking: New strategies and algorithms. 
#'        Geophysics, 75(4): 67-76.}
#'   \item{Modified Energy Ratio (MER) method}{Han L., Wong J., and John C. 
#'        (2010) Time picking on noisy microseismograms. In: Proceedings of the
#'        GeoCanada 2010 Convention - Working with the Earth, Calgary, AB, 
#'        Canada, p. 4}
#' }
#' @name firstBreak
#' @rdname firstBreak
#' @export
setMethod("firstBreak", "GPR", function(x, method = c("coppens", "coppens2",
  "threshold",  "MER"), thr = 0.12, w = 11, ns = NULL, bet = NULL){
    method <- match.arg(method, c("coppens", "coppens2", "threshold", "MER"))
    if(method == "coppens"){
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
      fb <- apply(xs, 2, .firstBreakModCoppens, w = w, ns = ns, bet = bet)
      fb <- x@depth[fb] # fb * x@dz
    }else if(method == "coppens2"){
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
      fb <- .firstBreakModCoppens2(xs, w = w, ns = ns, bet = bet)
      fb <- x@depth[fb] # fb * x@dz
    }else if(method == "threshold"){
      thres <- thr * max(x)
      fb <- apply(abs(x@data), 2, .firstBreakThres, thr = thres, x@depth)
    }else if(method == "MER"){
      w <- round(w / x@dz)
      fb <- .firstBreakMER(x@data, w)
      fb <- x@depth[fb]
    }
    if(any(is.na(fb))){
      warning("First break could not be picked for some traces. \n",
              "That's no luck, but good news is you can try with another method.\n",
              "This is probably because your traces have a too low S/N ratio." )
    }
    return(fb)
  } 
)

#--------------- DATA EDITING FUNCTIONS
#' Shift traces vertically by an amount of depth (time) units. New traces are interpolated.
#'
#' @param x A object of the class GPR
#' @param ts A numeric vector defining the amount of depth the traces have to
#'              be shifted
#' @param method A length-one character vector indicating the interpolation
#'               method. \code{"none"} means that the trace is shifted by the
#'               amount of points that is the closest to amount of depth 
#'               \code{ts}.
#' @param crop If TRUE (defaults), remove the rows containing only zero's 
#'              (no data).,
#' @return An object of the class GPR.
#' @seealso \code{\link{time0Cor}} to shift the traces such that they start
#'          at time zero.
#' @name traceShift
#' @rdname traceShift
#' @export
setMethod("traceShift", "GPR", function(x,  ts, method = c("spline", 
            "linear", "nearest", "pchip", "cubic", "none"), crop = TRUE){
    method <- match.arg(method, c("spline", "linear", "nearest", "pchip", 
                                  "cubic", "none"))
    if(length(ts) == 1){
      ts <- rep(ts, ncol(x))
    }
    xshift <- upsample(x, n = c(2,1))
    xshift@data <- .traceShift(xshift@data, ts = ts, tt = xshift@depth, 
                               dz = xshift@dz, method = method)
    x@data <- xshift@data[seq(1, length.out = nrow(x), by = 2), ]
    if(crop == TRUE){
      testCrop <- apply(abs(x@data),1,sum)
      x <- x[!is.na(testCrop), ]
    }
    # x <- .shiftThisTrace( x,  ts, method, crop = TRUE)
    proc(x) <- getArgs()
    # x@proc <- c(x@proc, proc)
    return(x)
  }
)

.shiftThisTrace <- function(x,  ts, method = c("spline", "linear", "nearest", 
                                               "pchip", "cubic", "none"), 
                            crop = TRUE){
  method <- match.arg(method, c("spline", "linear", "nearest", "pchip", 
                                "cubic", "none"))
  if(length(ts) == 1){
    ts <- rep(ts, ncol(x))
  }
  xshift <- upsample(x, n = c(2,1))
  xshift@data <- .traceShift(xshift@data, ts = ts, tt = xshift@depth, 
                             dz = xshift@dz, method = method)
  x@data <- xshift@data[seq(1, length.out = nrow(x), by = 2), ]
  if(crop == TRUE){
    testCrop <- apply(abs(x@data),1,sum)
    x <- x[!is.na(testCrop),]
  }
  return(x)
}

#' Time zero correction
#'
#' \code{time0Cor} shift the traces vertically such that they start at
#' time zero (time zero of the data can be modified with the function).
#' New traces are interpolated.
#'
#' When \code{keep = NULL} the amount of time kept is equal to
#' time taken by the air wave to travel from the transmitter to the
#' receiver.
#' @param x A object of the class GPR
#' @param t0 A numeric vector with length equal either to \code{NULL}, or one 
#'           or to the number traces.
#'           The traces will be shifted to \code{t0}. 
#'           If \code{t0 = NULL} `time0(x)` will be used instead. 
#'           If \code{t0} is the time-zero, set \code{keep = 0}.
#' @param method A length-one character vector defining the interpolation 
#'               method that are from the function 'interp1' 
#'               from the 'signal' package.
#' @param keep A length-one numeric vector indicating in time units how much of
#'             the trace has to be kept before time zero.
#' @param crop If TRUE (defaults), remove the rows containing only zero's 
#'              (no data).
#' @param c0 Propagation speed of the GPR wave through air (used only when
#'           \code{keep = NULL}).
#' @return An object of the class GPR.
#' @examples
#' data(frenkeLine00)
#' tfb <- firstBreak(frenkeLine00)
#' t0 <- firstBreakToTime0(tfb, frenkeLine00, c0 = 0.299)
#' time0(frenkeLine00) <- t0
#' frenkeLine00_2 <- time0Cor(frenkeLine00, method = "pchip")
#' @seealso \code{\link{time0}} to set time zero and \code{\link{firstBreak}} 
#'          to estimate the first wave break.
#'          \code{\link{firstBreakToTime0}} to convert the first wave break
#'          into time zero.
#' @name time0Cor
#' @rdname time0Cor
#' @export
setMethod("time0Cor", "GPR", function(x, t0 = NULL,  method = c("spline", 
                      "linear", "nearest", "pchip", "cubic", "none"), 
                      crop = TRUE, keep = 0){
    method <- match.arg(method, c("spline", 
                                  "linear", "nearest", "pchip", "cubic", 
                                  "none"))
    #if(is.null(keep)){
      #keep <- x@antsep/c0
    #}
    if(is.null(t0)){
      ts <- -x@time0 + keep
    }else{
      if(any(is.na(t0))){
        stop("Woops, time zero selected have NA values. \n",
             "This is not acceptable, time zero must have a numeric value in order to be corrected. \n",
             "If this is because first break could not be picked, \n",
              "you might want to consider removing these traces from your radargram.")
      } else{
        if(length(t0) == 1){
          t0 <- rep(t0, length(x@time0))
        }
        ts <- -t0 + keep
      }
    }
    # xshift <- upsample(x, n = c(2,1))
    # xshift@data <- .traceShift(xshift@data, ts, x@depth, x@dz, method)
    # x@data <- xshift@data[seq(1, length.out = nrow(A), by = 2), ]
    # if(crop == TRUE){
    #   testCrop <- apply(abs(Anew),1,sum)
    #   x <- x[!is.na(testCrop),]
    # }
    # xshift <- traceShift(x,  ts = ts, method = method, crop = TRUE)
    # x@data <-xshift@data
    #x <- traceShift(x,  ts = ts, method = eval(method), crop = TRUE)
    x <- traceShift( x,  ts, method, crop = TRUE)
    x@time0 <- x@time0 + ts
    # x@proc <- x@proc[-length(x@proc)] # remove proc from traceShift()
    proc(x) <- getArgs()
    return(x)
  }
)

setGeneric("FUN", function(x,  t0) standardGeneric("FUN"))

#' @export
setMethod("FUN", "GPR", function(x, t0){
  x <- traceShift(x, -t0, method = c("pchip"), crop = TRUE)
  #x@proc <- x@proc[-length(x@proc)] # remove proc from traceShift()
  proc(x) <- getArgs()
  return(x)
}
)

#' Constant-offset correction (time) of the GPR data
#'
#' Time correction for each trace to compensate the offset between transmitter 
#' and receiver antennae (it converts the trace time of the data acquired with
#' a bistatic antenna system into trace time data virtually acquiered with 
#' a monostatic system under the assumption of horizontally layered structure).
#' If all the traces have the same time-zero, this function does not change the 
#' trace but only the time (time scale). If the traces have different
#' time-zero, the traces are first aligned to have the same time-zero 
#' (spline interpolation)
#' @param x A object of the class GPR
#' @param t0 A numeric vector with length equal either to \code{NULL}, or one 
#'           or to the number traces.
#'           If \code{t0 = NULL} `time0(x)` will be used.
#' @param c0 Propagation speed of the GPR wave through air (used only when
#'           \code{keep = NULL}).
#' @seealso \code{\link{time0}} to set time zero and 
#'          \code{\link{firstBreakToTime0}} to convert the first wave break
#'          into time zero.
#' @name timeCorOffset
#' @rdname timeCorOffset
#' @export
# should use time0Cor() !!!!!
setMethod("timeCorOffset", "GPR", function(x, t0 = NULL){
  if(length(x@antsep) == 0 || (!is.numeric(x@antsep))){
    stop("You must first define the antenna separation",
         "with `antsep(x)<-...`!")
  }
  if(is.null(x@vel) || length(x@vel)==0){
    stop("You must first define the antenna separation",
         "with `vel(x)<-...`!")
  }
  #----------------------------------------------------------------------------#
  #FIXME > use here
  if(!is.null(t0)){
   time0(x) <- t0
  }
  time0Cor(x, method = "spline")
  # !!! pb if negative values in sqrt()!!!!
  tcor2 <- x@depth^2 - (x@antsep/x@vel[[1]])^2
  test <- tcor2 >= 0
  x <- x[test]
  x@depth <- sqrt(tcor2[test])
  # x@time0 is already = 0
  x@proc <- x@proc[-length(x@proc)] # remove proc from traceShift()
  x@proc <- c(x@proc, "timeCorOffset")
  return(x)
  #----------------------------------------------------------------------------#
  # if(is.null(t0)){
  #   tol <- sqrt(.Machine$double.eps)
  #   # all not equal
  #   if(abs(max(x@time0) - min(x@time0)) > tol){
  #     x <- time0Cor(x, method = "spline")
  #   }
  #   t0 <- mean(x@time0)
  # }else{
  #   if(length(t0) > 1){
  #     t0 <- mean(t0)
  #     warning("'length(t0)' should be equal to 1! I take the mean of 't0'!")
  #   }
  # }
  # x <- x[floor(t0/x@dz):nrow(x),]
  # tcor2 <- (x@depth - t0)^2 - (x@antsep/x@vel[[1]])^2
  # #tcor2 <- (x@depth - mean(x@time0) + x@antsep/0.299)^2 - 
  # #                (x@antsep/x@vel[[1]])^2
  # x <- x[tcor2 > 0,]
  # tcor <- sqrt( tcor2[tcor2 > 0] )
  # x@depth <- tcor
  # x@time0 <- rep(0, ncol(x))
  # x@proc <- x@proc[-length(x@proc)] # remove proc from traceShift()
  # x@proc <- c(x@proc, "timeCorOffset")
  # return(x)
})


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
#'            the GPR pulse width. w is defined on a time basis.
#' @return An object of the class GPR whose traces are dewowed.
#' @examples
#' data(frenkeLine00)
#' A <- dewow(frenkeLine00, type = "Gaussian")
#' A
#' @name dewow
#' @rdname dewow
#' @export
setMethod("dewow", "GPR", function(x, type = c("MAD", "Gaussian"), w){
  type <- match.arg(type, c("MAD", "Gaussian"))
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
  proc(x) <- getArgs()
#   x@proc <- c(x@proc, proc)
  return(x) 
})

#----------------- 1D-SCALING (GAIN)
#' Gain compensation
#' 
#' @name gain
#' @rdname gain
#' @export
setMethod("gain", "GPR", function(x, 
          type = c("power", "exp", "agc"),...){
  type <- match.arg(type, c("power", "exp", "agc"))
  x@data[is.na(x@data)] <-0
  if(type=="power"){
    x@data <- .gainPower(x@data, dts = x@dz, ...)
  }else if(type=="exp"){
    x@data <- .gainExp(x@data, dts = x@dz, ...)
  }else if(type=="agc"){
    x@data <- .gainAgc(x@data, dts = x@dz, ...)
  }
  proc(x) <- getArgs()
#   x@proc <- c(x@proc, proc)
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
    type <- match.arg(type, c("median", "hampel", "Gaussian"))
    w <- 50 * x@dz   # argument initialization
    if(type == "median"){
      if( length(dots <- list(...)) ){
#         dots <- list(...)
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
      if( length(dots <-  list(...)) ){
#         dots <- list(...)
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
      if( length(dots <-  list(...)) ){
#         dots <- list(...)
        if( !is.null(dots$w)){
          w <- dots$w
        }
      }
      x@data <- mmand::gaussianSmooth(x@data, sigma = w)
    }
    proc(x) <- getArgs()
#     x@proc <- c(x@proc, proc)
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
setMethod("filter2D", "GPR", function(x, type = c("median3x3", "adimpro"), 
                                      ...){
    type <- match.arg(type, c("median3x3", "adimpro"))
    if(type == "median3x3"){
      x@data <-  .medianFilter3x3(x@data)
    }else if( type == "adimpro"){
      IMG <- x@data
      IMG <- (IMG-min(IMG))/(max(IMG)-min(IMG))
      adimg <- adimpro::make.image(IMG)
      # img.smooth <- adimpro::awsimage(adimg, hmax = 2)
      # img.smooth <- adimpro::awsimage(adimg, hmax = 2)
      # img.smooth <- adimpro::awsaniso(adimg, hmax = 2,...)
      img.smooth <- adimpro::awspimage(adimg,...)
      AA <- adimpro::extract.image(img.smooth)
      AAA <- ( (AA - mean(AA))/sd(AA) ) * sd(x@data)
      x@data <- AAA
    }
    proc(x) <- getArgs()
#     x@proc <- c(x@proc, proc)
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
  proc(x) <- getArgs()
#   x@proc <- c(x@proc, proc)
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
  proc(x) <- getArgs()
#   x@proc <- c(x@proc, proc)
  return(x)
  } 
)

#' Trace scaling
#'
#' @name traceScaling
#' @rdname traceScaling
#' @export
setMethod("traceScaling", "GPR", function(x, 
            type = c("stat","min-max","95","eq","sum", "rms", 
                     "mad", "invNormal")){
    x@data <- scaleCol(x@data, type = type)
    proc(x) <- getArgs()
#   x@proc <- c(x@proc, proc)
    return(x)
  }
)

#' Trace average
#'
#' Average traces in a radargram along the distance (horizontal) axis using
#' a moving window. This can be used to increase signal to noise ratio. Note that if 
#' the moving window length is not defined, all traces are averaged into one single trace.
#' 
#' @param x An object of the class GPR
#' @param w A length-one integer vector equal to the window length of the 
#'          average window. If \code{w = NULL} a single trace corresponding to
#'          the average trace of the whole profile is returned.
#' @param FUN A function to compute the average (default is \code{mean})
#' @param ... Additional parameters for the FUN functions
#' @return An object of the class GPR. When \code{w = NULL}, this function 
#'         returns a GPR object with a single trace corresponding to the 
#'         average trace of the whole radargram. When \code{w} is equal to a
#'         strictly positive interger this function returns a GPR object with
#'         a size identical to x where each trace corresponds to the average
#'         of the \code{w} neighbouring traces centered on the considered trace.
#' @examples
#' data("frenkeLine00")
#' 
#' f0 <- frenkeLine00
#' 
#' f1 <- traceAverage(f0)
#' plot(f1)
#' # substract the average trace
#' plot(f0 - f1)
#' 
#' f2 <- traceAverage(f0, w = 20)
#' plot(f2)
#' plot(f0 - f2)
#' 
#' f3 <- traceAverage(f0, w = 20, FUN = median)
#' plot(f3)
#' plot(f0 - f3)
#' @name traceAverage
#' @rdname traceAverage
#' @export
setMethod("traceAverage", "GPR", function(x, w = NULL, FUN = mean, ...){
    FUN <- match.fun(FUN)
    if(is.null(w)){
      xdata <- x@data
      x <- x[,1]
      x@data <- as.matrix(apply(xdata, 1, mean, ...))
      x@time0 <- mean(x@time0)
      x@time <- mean(x@time)
      x@coord <- matrix(ncol = 0, nrow = 0)
      x@rec <- matrix(ncol = 0, nrow = 0)
      x@trans <- matrix(ncol = 0, nrow = 0)
    }else{
      x@data <- wapplyMat(x@data, width = w, by = 1, FUN = FUN, MARGIN = 1, ...)
    }
    # funName <- getFunName(FUN)
    # proc(x) <- getArgs( addArgs = c('FUN' = funName))
    proc(x) <- getArgs()
#   x@proc <- c(x@proc, proc)
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
    proc(x) <- getArgs()
#   x@proc <- c(x@proc, proc)
    return(x)
  } 
)

#' Frequency-wavenumber filter
#'
#' @name fkFilter
#' @rdname fkFilter
#' @export
setMethod("fkFilter", "GPR", function(x, fk = NULL, L = c(5 , 5), npad = 1){
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
    proc(x) <- getArgs()
#   x@proc <- c(x@proc, proc)
    return(x)
    
  } 
)


#--------------- EIGENIMAGE RECONSTRUCTION

#' Eigenimage filter
#'
#' Decompose the GPR data (radargram) using singular value decomposition (SVD) 
#' and return the reconstructed data for the selected singular values 
#' (eigenvalues). This method is sometimes refered to in the litterature as 
#' the Karhunen-Loeve (KL) transformation or the eigenimage decomposition.
#'
#' @param x           An object of the class GPR
#' @param eigenvalue  An integer vector specifying the eigenvalues selected. 
#'                    \code{eigenvalue = 1:5} returns the image reconstructed 
#'                    using the first five eigenvalues while \code{c(2,4,6)}
#'                    will return the image reconstructed for these three 
#'                    specific eigenvalues. 
#'                    If \code{NA}, a plot of the eigenvalues spectrum 
#'                    will be displayed and the user will be prompted to enter 
#'                    the pair of selected eigenvalues separated by a comma. 
#'                    That is for example \code{1,2} will return the image 
#'                    reconstructred using tthe first two eigenvalues. 
#'                    If \code{NULL}, the image is reconstructed using 
#'                    all eigenvalues.
#' @param center      Logical or numeric. If \code{TRUE}, centering is done by 
#'                    subtracting the layer means (omitting \code{NA}'s), and 
#'                    if \code{FALSE}, no centering is done. If center is a 
#'                    numeric vector with length equal to the 
#'                    \code{nlayers(x)}, then each layer of \code{x} has the 
#'                    corresponding value from center subtracted from it.
#' @param scale       Logical or numeric. If \code{TRUE}, scaling is done by 
#'                    dividing the (centered) layers of \code{x} by their 
#'                    standard deviations if center is \code{TRUE}, and the 
#'                    root mean square otherwise. 
#'                    If scale is \code{FALSE}, no scaling is done. If scale is
#'                    a numeric vector with length equal to \code{nlayers(x)}, 
#'                    each layer of \code{x} is divided by the corresponding 
#'                    value. Scaling is done after centering.
#'
#' @return An object of the class GPR.
#' 
#' @references
#' \itemize{
#'   \item{Textbook: Sacchi (2002) Statistical and Transform Methods
#'         in Geophysical Signal Processing}
#'         }
#'         
#' @examples  
#' x1 <- eigenFilter(x, eigenvalue = c(1,3))
#' plot(x)
#' plot(x1)
#'
#' @name eigenFilter
#' @rdname eigenFilter
#' @export
setMethod("eigenFilter", "GPR", function(x, eigenvalue = NA, center = TRUE, 
                                         scale = FALSE){

  ev <- unique(eigenvalue)
  X <- scale(x@data, center = center, scale = scale)
  Xsvd <- svd(X)
  lambda <- Xsvd$d^2
  
  if(max(ev) > ncol(X)){
    stop("The number of eigenvalues selected cannot exceed the number ",
         "of GPR traces.")
  }
  if(min(ev) < 0){
    stop("The eigenvalues number must be strictly positive.")
  }
  
  if(is.null(ev)){
    ev <- c(seq_len(ncol(X)))
  }
  
  if(any(is.na(ev))){
    windows()
    plot(seq_along(lambda), lambda, type = "b", col = "blue", pch = 16,
         xlab = "Eigenvalue Index", ylab = "Eigenvalue")
    ev <- readline("What eigenvalues do you want to use to reconstruct the ",
                   "radargram ?")
    ev <- as.numeric(unlist(strsplit(ev, split = ",")))
  }
  d <- numeric(length(Xsvd$d))
  d[ev] <- Xsvd$d[ev]
  Xnew <- Xsvd$u %*% diag(d) %*% t(Xsvd$v)
  
  # Xeigen <- array(NA, dim = c(dim(Xsvd$u), length(ev)))
  # for(i in seq_along(ev)){
  #   Xeigen[,,i] <- Xsvd$d[ev[i]] * Xsvd$u[,ev[i]] %*% t(Xsvd$v[,ev[i]])
  # }
  # 
  # if(length(ev)>1){
  #   Xnew <- rowSums(Xeigen, dims=2)
  # } else{
  #   Xnew <- Xeigen[,,1]
  # }
  
  
  # if(!is.null(attr(X, 'scaled:scale')) && !is.null(attr(X, 'scaled:center'))){
  #   Xnew <- t(apply(Xnew, 1, function(r) r * attr(X, 'scaled:scale') + 
  #                     attr(X, 'scaled:center')))
  # }else if(is.null(attr(X, 'scaled:scale')) && 
  #          !is.null(attr(X, 'scaled:center'))){
  #   Xnew <- t(apply(Xnew , 1, function(r) r + attr(X, 'scaled:center')))
  # }else if(!is.null(attr(X, 'scaled:scale')) && 
  #          is.null(attr(X, 'scaled:center'))){
  #   Xnew <- t(apply(Xnew , 1, function(r) r * attr(X, 'scaled:scale')))
  # }

  x@data <- unscale(X, Xnew)

  # if(is.null(eigenvalue)){
  #   proc(x) <- getArgs(addArgs = list('eigenvalue' = eigenvalue, 
  #                                     'center' = as.character(center), 
  #                                     'scale' = as.character(scale)))
  # } else{
  #   proc(x) <- getArgs(addArgs = list('eigenvalue' = ev, 
  #                                     'center' = as.character(center), 
  #                                     'scale' = as.character(scale)))
  # }
  proc(x) <- getArgs()
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
    x@data <- apply(x@data, 2, phaseRotation, phi)
    proc(x) <- getArgs()
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
#     x@proc <- c(x@proc,"conv1D")
    proc(x) <- "conv1D"
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
#'   \item \code{nf}: A length-one numeric vector defining the filter length.
#'   \item \code{mu}: A length-one numeric vector defining the amount of noise.
#' }
#' @section Wavelet deconvolution:
#' The required arguments for \code{method = "wavelet"} are:
#' \itemize{
#'   \item \code{h}: A numeric vector corresponding to the wavelet used to
#'                   deconvolve the GPR data.
#'   \item \code{mu}: A length-one numeric vector defining the amount of noise.
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
    proc(x) <- getArgs()
#     x@proc <- c(x@proc, proc)
    toReturn[["x"]] <- x
    return(toReturn)
  }
)




#-------------------------------------------#
#---------------- SETMETHOD ----------------#
# Print methods
# setMethod("print", "GPR", function(x) print.GPR(x))
# > 1. helper function:
.GPR.print   <-  function(x, digits=5){
  topaste <- c(paste("***","Class GPR", "***\n"))
  topaste <- c(topaste,   paste0("name        = ", x@name, "\n"))
  if(length(x@filepath) > 0){
    topaste <- c(topaste, paste0("filepath    = ", x@filepath, "\n"))
  }
  nbfid <- sum(trimStr(x@fid)!= "")
  if(nbfid > 0){
    topaste <- c(topaste, paste0(nbfid, " fiducial(s)\n"))
  }
  if(length(x@description) > 0){
    topaste <- c(topaste, paste0("description = ", x@description, "\n"))
  }
  if(length(x@date) > 0){
    topaste <- c(topaste, paste0("survey date = ", x@date,"\n"))
  }
  topaste <- c(topaste, paste0(x@surveymode,", ",x@freq, " MHz, ", 
                "Window length = ",(nrow(x@data)-1)*x@dz, " ", x@depthunit,
                ", dz = ",x@dz, " ", x@depthunit, "\n"))
  topaste <- c(topaste, paste0(ncol(x@data), " traces, ", 
                diff(range(x@pos))," ",x@posunit,"\n"))
  if(length(x@proc)>0){
    topaste <- c(topaste, paste("> PROCESSING\n"))
    for(i in seq_along(x@proc)){
      topaste <- c(topaste, paste0("  ",i,". ", x@proc[i],"\n"))
    }      
  }
  topaste <- c(topaste, paste("****************\n"))
  return(topaste)      
}    

# #' @rdname show
#' Print GPR
#'
#' @method print GPR 
#' @name print
#' @rdname show
# > 2. S3 function:
print.GPR <- function(x, ...){
  jj <- .GPR.print(x, ...)
  cat(jj)
  return(invisible(jj))
}
# > 3. And finally a call to setMethod():
# #' @rdname show
#' Show some information on the GPR object
#'
#' Identical to print().
#' @name show
#' @aliases show-method
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
      
#' Add a GPR trace points on a plot
#'
#' @method points GPR 
#' @name points
#' @rdname points
#' @export
points.GPR <- function(x,...){
  if(length(x@vel)>0){  
    v <- x@vel[[1]]
  }else{
    v <- 0
  }
  if(any(dim(x) == 1)){
#     z <- seq(-x@time0, by = x@dz, length.out = length(x@data))
    z <- x@depth - x@time0
    points(z, x@data,...)
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
  dots <- list()
  type <- "raster"
  addTopo <- FALSE
  normalize <- NULL
  nupspl <- NULL
  addAnn <- TRUE
  addFid <- TRUE
  #   clim <- NULL
  clip <- NULL
  xlim <- NULL
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
    if( !is.null(dots$xlim)){
      xlim <- dots$xlim
      dots$xlim <- NULL
    }
    if(!is.null(dots$ylim)){
      if(!is.null(zlim)){
        cat("You specified both, 'zlim' and 'ylim'.\n")
        cat("These have the same meaning, I only consider 'zlim'.\n")
      }else{
        zlim <- dots$ylim
        dots$ylim <- NULL
      }
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
    title(paste0(x@name, ": trace #", x@traces," @", round(x@pos,2), 
                 x@posunit), outer=TRUE)
#     par(op)
   }else{
     clab <- "mV"
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
    }else if(is.null(clip)){
      # clip below the 0.01-quantile and above the 0.99-quantile
      x@data <- .clip(x@data, quantile(as.vector(x@data), 0.99, na.rm = TRUE),
                      quantile(as.vector(x@data), 0.01, na.rm = TRUE))
    }
    if(addFid == FALSE){
      x@fid <- character(length(x@fid))
    }
    xlab <- x@posunit
    if(isCMP(x)){
      if(length(x@antsep) == ncol(x)){
        xvalues <- x@antsep
        xlab <- paste0("antenna separation (", x@posunit, ")")
      }else{
        stop("length(antsep(x)) != ncol(x). You must correctly define ",
             "the antenna separation distance with 'antsep(x) <- ...'")
      }
    }else if(toupper(x@surveymode) == "CMPANALYSIS"){
      xvalues <- x@pos
      clab <- ""
      xlab <- paste0("velocity (", x@posunit, "/", x@depthunit, ")")
    }else if( length(x@coord) > 0 ){
      xvalues <- posLine(x@coord)
    }else{
      xvalues <- x@pos
    }
    if(is.null(xlim)){
      xlim <- range(xvalues)
    }
    type <- match.arg(type, c("raster","wiggles"))
    if(type == "raster"){
      if(grepl("[s]$",x@depthunit) && addTopo){
        x <- migration(x)
      }
      if(grepl("[m]$",x@depthunit)){
        ylab <- paste0("depth (", x@depthunit, ")")
      }else if( grepl("[s]$",x@depthunit) ){
        ylab <- paste0("two-way travel time (", x@depthunit, ")")
      }
      yvalues <- -rev(x@depth)
      if(is.null(zlim)){
        zlim <- range(yvalues)
      }
      do.call(plotRaster, c(list(z = x@data, x = xvalues, y = yvalues, 
                     main = main, ylim = zlim, xlim = xlim,
                     xlab = xlab, ylab = ylab, note = x@filepath,
                     time_0 = x@time0, antsep = x@antsep, v = v, 
                     addFid = addFid, fid = x@fid, surveymode = x@surveymode,
                     addAnn = addAnn, annotations = x@ann, clab = clab,
                     depthunit = x@depthunit, posunit = x@posunit), dots))
    }else if(type=="wiggles"){
      if(addTopo && length(x@coord)>0){
        topo <- x@coord[,3]
      }else{
        topo = NULL
      }
      if(grepl("[m]$",x@depthunit)){
        ylab <- paste0("depth (", x@depthunit, ")")
      }else if(grepl("[s]$",x@depthunit)){
        if(addTopo){
          ylab <- "depth (m)"
        }else{
          ylab <- paste0("two-way travel time (", x@depthunit, ")")
        }
      }
      yvalues <- -rev(x@depth)
      if(is.null(zlim)){
        zlim <- range(yvalues)
      }
#       if(length(x@coord)>0){
#         xvalues <- posLine(x@coord)
#       }else{
#         xvalues <- x@pos
#       }
      do.call(plotWig, c(list(z = x@data, x = xvalues, y = yvalues, 
                    main=main, ylim = zlim,
                    xlab = xlab, ylab = ylab, note = x@filepath, 
                    time_0 = x@time0, antsep = x@antsep, v = v, 
                    surveymode = x@surveymode,
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
      cat(ylim,"  range = ", range(coord(x, 2)),"\n")
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
    # Problem: interpolate also coord!!!")
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
      z0 <- rep(max(coord(x, 3)), length(x)) - refCoord[3]
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
            all = FALSE, plotLog = TRUE, ...){
#   op <- par(no.readonly=TRUE)
  AMP <- apply(abs(x@data),1,FUN,...)
  ylab <- "mV"
  if(plotLog == TRUE){
    AMP <- log(AMP)
    ylab <- "log(mV)"
  }
  #z <- seq(0,by=x@dz,length.out=length(AMP))
  z <- depth(x)
  if(!add){
    par(mar=c(5, 4, 4, 2)+0.1)
      plot(z, AMP, type = "l", xlab = x@depthunit, ylab = ylab, ...)
    if(all == TRUE){
      if(plotLog == TRUE){
        invisble(apply(log(abs(x@data)), 2, lines, x = z, 
                          col=rgb(0.2,0.2,0.2,7/max(ncol(A),7))))
      }else{
        invisble(apply((abs(x@data)), 2, lines, x = z, 
                          col=rgb(0.2,0.2,0.2,7/max(ncol(A),7))))
      }
    }
    title(x@name)
  }else{
    if(all == TRUE){
      if(plotLog == TRUE){
        invisble(apply(log(abs(x@data)), 2, lines, x = z, 
                          col=rgb(0.2,0.2,0.2,7/max(ncol(A),7))))
      }else{
        invisble(apply((abs(x@data)), 2, lines, x = z, 
                          col=rgb(0.2,0.2,0.2,7/max(ncol(A),7))))
      }
    }
    lines(z, AMP, ...)
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
    type <- match.arg(type, c("f-x","f-k"))
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


#--- REMOVE DUPLICATED TRACES (TRACES WITH (ALMOST) SAME POSITIONS) ---#
#' Remove traces with duplicated trace positions
#' 
#' checks for duplicates trace positions (up to precision defined by 'tol') 
#' and remove them from 'x' (object of the class GPR or GPRsurvey). 
#' If there is a series of consecutive traces with an inter-distance smaller
#' than the tolerance threshold defined by the computer precision, 
#' the function starts by removing traces every two traces and repeat 
#' the procedure until the trace inter-distances are larger that the threshold.
#' Example with 5 traces:
#' \itemize{
#'   \item distance between trace 1 and 2 < tol
#'   \item distance between trace 2 and 3 < tol
#'   \item distance between trace 3 and 4 < tol
#'   \item distance between trace 4 and 5 < tol
#' }
#' The algorithm first remove trace 2 and 4 and recompute 
#' the inter-trace distances:
#' \itemize{
#'   \item distance between trace 1 and 3 < tol
#'   \item distance between trace 3 and 5 > tol
#' }  
#' The algorithm remove trace 3. END!
#' @param x   An object of the class GPR
#' @param tol Length-one numeric vector: if the horizontal distance between two 
#'            consecutive traces is smaller than \code{tol}, then
#'            the second trace is removed.
#'            If \code{tol = NULL}, \code{tol} is set equal to
#'            \code{sqrt(.Machine$double.eps)}.
#' @name trRmDuplicates 
#' @rdname trRmDuplicates            
#' @export
setMethod("trRmDuplicates", "GPR", function(x, tol = NULL){
  if(length(x@coord) == 0 ){
    warning("No trace coordinates!")
    return(x)
  }
  dist2D <- posLine(x@coord[, 1:2], last = FALSE)
  # in 'x' and 'topo'
  if(is.null(tol))  tol <- sqrt(.Machine$double.eps)
  tdbl <- which(abs(diff(dist2D)) < tol)
  # x <- x[, -(tdbl + 1)]
  diff(tdbl)
  check <- 0L
  while(length(tdbl) > 0){
    rmTr <- c()
    skip <- FALSE
    for(i in seq_along(tdbl)){
      if(i > 1 && (tdbl[i] - 1 == tdbl[i - 1])){
        tdbl[i] <- -999
        next
      }
      rmTr <- c(rmTr, tdbl[i] + 1)
      check <- check + 1L
    }
    x <- x[, -rmTr]  # remove trace in x
    dist2D <- posLine(x@coord[, 1:2], last = FALSE)
    tdbl <- which(abs(diff(dist2D)) < tol)
  }
  message(check, " duplicated trace(s) removed from 'x'!")
  x@proc <- c(x@proc, "trRmDuplicates")
  return(x)
})

#' Interpolate trace positions from measurement (e.g., GPS).
#'
#' @param x      An object of the class GPR.
#' @param topo   A \eqn{m \times 4} numeric matrix, with \code{m} the number of 
#'               traces in 'x'. The columns names of topo must be
#'               "E", "N", "Z" and "TRACE".
#' @param plot   A length-one boolean vector. If TRUE some control
#'               plots are displayed.
#' @param r      A 'RasterLayer' object from the package 'raster' from which
#'               trace elevation \code{z} will be extracted based on the 
#'               trace position \code{(x, y)} on the raster.
#' @param tol    Length-one numeric vector: if the horizontal distance between 
#'               two consecutive trace positions is smaller than \code{tol}, 
#'               then the traces in between as well as the second trace
#'               position are removed.
#'               If \code{tol = NULL}, \code{tol} is set equal to
#'               \code{sqrt(.Machine$double.eps)}.
#' @param method A length-three character vector defining the interpolation
#'               methods (same methods as in \code{signal::interp1}:
#'               "linear", "nearest", "pchip", "cubic", and "spline"). 
#'               First element for the interpolation of the 
#'               inter-trace distances, 
#'               second element for the interpolation of the horizontal 
#'               trace positions, and third element for the interpolation
#'               of the vertical trace positions.
#' @name interpPos 
#' @rdname interpPos
#' @export
setMethod("interpPos", "GPR", 
          function(x, topo, plot = FALSE, r = NULL, tol = NULL, 
                   method = c("linear", "spline", "pchip"), ...){
  if(all(is.na(topo[,"TRACE"]))){
    stop(paste0(x@name, ": no link between the measured points",
                   "and the GPR traces!\n"))
  }
  # if we have measured some points before the line start or 
  # after the end of the GPR Line, we delete them
  test <- which(!is.na(topo[,"TRACE"]))
  #trueEnd <- max(which(test))
  #trueBeg <- min(which(test))
  topo <- topo[min(test):max(test), ]
  #--- 3D topo Distance ---#
  if(!is.null(r)){
    topo[,"Z"] <- raster::extract(r, topo[, c("E","N")], method = "bilinear")
    if(sum(is.na(topo[, "Z"])) > 0){
      stop("\n'extract(r, topo[, c(\"E\",\"N\")], method = \"bilinear\")' ",
           "returns 'NA' values!\n", 
           "Not all GPR positions fall within raster extent or\n",
           "there are 'NA' values in raster 'r'!")
    }
  }
  # check for duplicates in TRACE ID and remove them!
  topo <- rmRowDuplicates(topo, topo[, "TRACE"])
  # order topo by increasing traceNb
  topo <- topo[order(topo[, "TRACE"]), ]
  #--- REMOVE DUPLICATED TRACES (TRACES WITH (ALMOST) SAME POSITIONS) ---#
  dist2D <- posLine(topo[, c("N", "E")], last = FALSE)
  # in 'x' and 'topo'
  if(is.null(tol))  tol <- sqrt(.Machine$double.eps)
  tdbl <- which(abs(diff(dist2D)) < tol)
  if(length(tdbl) > 0){
    check <- 0
    for(i in seq_along(tdbl)){
      dtr <- topo[tdbl[i]+1, "TRACE"] - topo[tdbl[i], "TRACE"]
      # traces to remove
      w <- topo[tdbl[i], "TRACE"] + seq_len(dtr)
      x <- x[, -w]  # remove trace in x
      # remove traces in topo and actualise the trace numbers
      topo <- topo[-(tdbl[i] + 1), ] 
      v <- (tdbl[i] + 1):nrow(topo)
      topo[v, "TRACE"] <- topo[v, "TRACE"] -  dtr
      #dist3D <- dist3D[-tdbl[i]]
      tdbl <- tdbl - 1
      check <- check + dtr
    }
    message(length(tdbl), " duplicate trace position(s) removed from 'topo'!")
    message("Accordingly, ", check, " trace(s) removed from 'x'!")
  }
  dist3D <- posLine(topo[, c("N", "E", "Z")], last = FALSE)
  # if there are points measured with the total station
  # that do not have an fiducial (FID) > interpolate them!
  myWarning <- ""
  if(anyNA(topo[,"TRACE"])){
    # dist3D[topo$PNAME %in% FID$PNAME] > distance for the points 
    # also recorded in FID
    myWarning <- "\npoints total station without fiducials"
    test <- !is.na(topo[,"TRACE"])
    intMeth <- ifelse(sum(test) > 2, method[1], "linear")
    traceNb <- signal::interp1(x      = dist3D[test], 
                               y      = topo[test, "TRACE"], 
                               xi     = dist3D,
                               method = intMeth,
                               extrap = TRUE)
    topo[, "TRACE"] <- as.integer(round(traceNb))
    # check for duplicates in TRACE ID and remove them!
    topo <- rmRowDuplicates(topo, topo[, "TRACE"])
  }
  if(all(seq_along(x@pos) %in% topo[, "TRACE"])){
    A <- topo[topo[, "TRACE"] %in% seq_along(x@pos), c("E", "N", "Z")]
    message("No interpolation required because the trace positions\n",
            "of all GPR traces is already available (in object 'topo')!")
  }else{
    #--- INTERPOLATION ---#
    A <- interp3DPath(x = topo[, c("E", "N", "Z")], pos = topo[, "TRACE"], 
                      posi = seq_along(x@pos), r =r,
                      method = method)
    colnames(A) <- c("E", "N", "Z")
  }
  dist3Dint <- posLine(A, last = FALSE)
  message(x@name, ": mean dx = ", round( mean(diff(dist3Dint)), 3 ), 
          ", range dx = [", round( min(diff(dist3Dint)), 3 ),", ", 
          round( max(diff(dist3Dint)), 3 ),"]", myWarning)
  if(plot == TRUE){
    op <- par(no.readonly=TRUE)
    par(mfrow=c(1, 3))
    plot(topo[, "TRACE"], dist3D, pch = 20, col = "red", cex = 2, asp = 1,
         xlab = "trace number", ylab = "trace spacing (3D)",
         xlim = range(seq_along(x@pos)), ylim = range(dist3Dint), 
         main = paste( x@name))
    points(seq_along(x@pos), dist3Dint, pch = 20, col = "blue", cex = 0.6)
    plot(dist3Dint, A[, "Z"], type = "l", asp = 10, 
         xlab = "interpolated trace spacing", 
         ylab = "interpolated elevation",
         main = paste0(x@name, " min dx = ", round(min(diff(dist3Dint)), 2), 
                       "  max dx = ", round(max(diff(dist3Dint)), 2)))
    points(dist3D, topo[,"Z"], pch = 20, col = "red")
    plot(topo[, c("E","N")], col = 1, type = "l", lwd = 2, asp = 1,
         ylim = range(A[, "N"]), xlim = range(A[, "E"]), 
         main = paste0(x@name, " mean dx=", round(mean(diff(dist3Dint)),2)))
    points(topo[, c("E","N")], col = 1, pch = 20, cex = 2)
    lines(A[, "E"], A[, "N"], col = 2, lwd = 1)
    Sys.sleep(1)
    par(op)
  }
  x@coord <- A
  x@proc <- c(x@proc, "interpPos")
  return(x)
}
)

# remove rows of x with duplicated element in v
# length(v) == nrow(x)
# use the R-base function 'duplicated()'
rmRowDuplicates <- function(x, v){
  iRM <- which(duplicated(v))
  if(length(iRM) > 0){
    return( x[-iRM, ])
  }else{
    return(x)
  }
}


# x = topo[, c("E", "N", "Z")] or C("x", "y", "z")
# dist3D <- posLine(x, last = FALSE)
# posi <- seq_along(x@pos)
# pos <- topo[, "TRACE"]
# r
interp3DPath <- function(x, pos, posi, r = NULL,
                         method = c("linear", "spline", "pchip")){
  if(ncol(x) > 3) x <- x[, 1:3]
  dist3D <- posLine(x, last = FALSE)
  #--- INTERPOLATION dist3D ---#
  intMeth <- ifelse(length(dist3D) > 2, method[1], "linear")
  dist3DInt   <- signal::interp1(pos, dist3D, posi, 
                                 method = intMeth , extrap=TRUE)
  #--- INTERPOLATION N,E,Z ---#
  ENZ <- matrix(nrow = length(posi), ncol = 3)
  intMeth <- ifelse(length(dist3D) > 2, method[2], "linear")
  ENZ[, 1] <- signal::interp1(dist3D, x[, 1], dist3DInt, 
                              method = intMeth, extrap = NA)
  ENA <- is.na(ENZ[, 1])
  ENZ[ENA, 1] <- signal::interp1(dist3D, x[, 1], dist3DInt[ENA], 
                              method = "linear", extrap = TRUE)
  ENZ[, 2] <- signal::interp1(dist3D, x[, 2], dist3DInt, 
                              method = intMeth , extrap = NA)
  ZNA <- is.na(ENZ[, 2])
  ENZ[ZNA, 2] <- signal::interp1(dist3D, x[, 2], dist3DInt[ZNA], 
                                 method = "linear", extrap = TRUE)
  if(is.null(r) && ncol(x) == 3){
    intMeth <- ifelse(length(dist3D) > 2, method[3], "linear")
    ENZ[, 3] <- signal::interp1(dist3D, x[, 3], dist3DInt, 
                                method = intMeth, extrap = NA)
  }else if(!is.null(r)){
    ENZ[, 3] <- raster::extract(r, cbind(ENZ[, 1], ENZ[, 2]), 
                                method = "bilinear")
  } 
  lastNA  <- max(which(!is.na(ENZ[, 3])))
  firstNA <- min(which(!is.na(ENZ[, 3])))
  if(firstNA > 1){
    ENZ[1:(firstNA-1), 3] <- ENZ[firstNA, 3]
  } 
  if(lastNA < length(ENZ[, 3])){
    ENZ[(lastNA+1):length(ENZ[, 3]), 3]<- ENZ[lastNA, 3]
  }
  # message(x@name, ": mean dx = ", round(mean(diff(dist3DInt)), 3), 
  #         "  range dx = ",round(min(diff(dist3DInt)), 3)," - ", 
  #         round(max(diff(dist3DInt)), 3))
  return(ENZ)
}


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
setMethod("reverse", "GPR", function(x, id = NULL,  tol = 0.3){
    xnew <- x
    xnew@data <- x@data[,length(x):1]
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
    proc(xnew) <- getArgs()
    return(xnew)
  }
)

#' Shift estimation between two GPR profiles.
#'
#' @name shiftEst
#' @rdname shiftEst
#' @export
setMethod("shiftEst", "GPR", function(x, y = NULL, 
          method=c("phase", "WSSD"), dxy = NULL, ...){
  return( displacement(x@data, y@data, method=method, dxy = dxy) )
})

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
      type <- match.arg(type, c("raster","wiggles"))
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
          depth_0 <- depthToTime(z = 0, time_0, v = velo, antsep = x@antsep) * 
                                  velo/ 2
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
setMethod("addDelineation", "GPR", function(x, itp, 
          name = NULL, type = c("raster", "wiggles"), addTopo = FALSE, ...){
    if(is.null(dev.list())){
      stop("You must first plot the GPR profile with the function \"plot\"!\n")
    }
    xsave <- x
    # itp <- locator(type="l", n=n)
    topo <- rep(0,length(x))
    type <- match.arg(type, c("raster","wiggles"))
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
        if(length(x@vel)>0){  
          velo <- x@vel[[1]]
        }else{
          velo <- 0
        }
        topo <- x@coord[,3]
        topo <- topo - max(topo)
        yvalues <- yvalues * velo/ 2
        time_0 <- mean(x@time0)
        depth_0 <- depthToTime(z = 0, time_0, v = velo, antsep = x@antsep) * 
                                      velo/ 2
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
setReplaceMethod("rmDelineations", "GPR", function(x,value=NULL){
    deli <- x@delineations
    n_d <- length(deli)
    if(!is.null(value) && n_d > 0 && value != "all"){
      n_tot <- sum(sapply(deli, .lengthList))
      it <- 0
      value <- n_tot - value + 1
      for(i in n_d:1){
        if(typeof(deli[[i]]) == "list"){
          n_sub_d <- length(deli[[i]])
          for(j in n_sub_d:1){
            it <- it + 1
            # itdel <- 0
            if(it %in% value){
            # if(value==it){
              # x@delineations[[i]][[j]] <- NULL
              # j <- j - itdel
              # cat("---- j=", j,"  ,   j-itdel=",j- itdel,"\n")
              x@delineations[[i]][j] <- NULL
              # itdel <- itdel + 1
              if(length(x@delineations[[i]])==0 || 
                  is.null(unlist(x@delineations[[i]], use.names = FALSE))){
                x@delineations[i] <- NULL
                # i<-i-1
                break
              }
              # print(x@delineations[[i]])
            }
          }
        }else{
          it <- it + 1
          if(it %in% value){
          # if(value==it){
            x@delineations[i] <- NULL
            # i <- i-1
            # break
            # print(x@delineations)
          }
        }
      }
    }else if(n_d <1){
      warning("No delineation to delete\n")
    }else if(is.null(value)){
      stop("You must specified the no of the delineation you want to delete!\n")
    }else if(value=="all"){
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
      message("*** delineated lines ****")
      it <- 0
      for(i in 1:n_d){
        if(typeof(deli[[i]])=="list"){
          n_sub_d <- length(deli[[i]])
          message(names(deli[i]))
          for(j in 1:n_sub_d){
            it <- it + 1
            tracePos <- sapply( deli[[i]][[j]][,1], .which, x@traces)
            xpos <- x_dist[tracePos]
            zpos <- deli[[i]][[j]][,4]
            message(it, ". length = ", round(diff(range(xpos)), 2), 
                    ";  depth = ", round(diff(range(zpos)), 2),
                    ";  ", length(xpos), " pts")
            # lines(xpos, zpos,col=col[i],...)
          }
        }else{
          it <- it + 1
          tracePos <- sapply( deli[[i]][,1], .which, x@traces)
          xpos <- x_dist[tracePos]
          zpos <- deli[[i]][,4]
          message(it, ". length =", round(diff(range(xpos)), 2),
                  "; depth =", round(diff(range(zpos)), 2),
                  "; number of pts =", length(xpos))
          # lines(xpos, zpos,col=col[i],...)
        }
        message("- - - - - - - - - - -")
      }
    }else{
      message("No lines were delineated!")
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
        it <- it+1
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
            lines3d(ypos, zpos, xpos, col = col[i], ...)
          }
        }else{
          tracePos <- sapply( deli[[i]][,1], .which, x@traces)
          xpos <- x@coord[tracePos,1] - x@coordref[1]
          ypos <- x@coord[tracePos,2] - x@coordref[2]
          z0 <- max(coord(x, 3))   -   x@coordref[3]
          zpos <- z0 + deli[[i]][,5] 
          lines3d(ypos, zpos, xpos, col = col[i], ...)
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
      message("No lines were delineated!")
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
      message("No lines were delineated!")
    }
  }
)

#------------------------- CMP ANALYSIS -----------------------------------#

#' Normal Move-Out correction
#' 
#' Remove the Normal Move-Out (NMO) from the trace given a constant velocity: 
#' this is a non-linear 
#' correction of the time axis that require interpolation. Note that
#' only the conventional NMO correction is currently implemented. The 
#' conventional NMO introduces a streching effect. A nonstretch NMO will
#' be implemented in a near future.
#' 
#' Assuming a horizontal reflecting plane and homogeneous medium, the two-way
#' bistatic travel time of the reflected wave 
#' for an antenna separation \eqn{x} follows directly from the Pythagorean 
#' theorem:
#' \deqn{t_{TWT}(x,z) = \sqrt{\frac{x^2}{v^2} + \frac{4z^2}{v^2}}}
#' where \eqn{t_{TWT}(x)} is the two-way travel time at antenna
#' separation \eqn{x} of the wave reflected at depth \eqn{z} with propagation
#' velocity \eqn{v}. This equation defines an hyperbola (keep \eqn{z} constant,
#' increase the antenna separation \eqn{x} and you obtain a hyperbola similar
#' to the reflection signals you obtain with common-mid point survey).
#' The idea behind NMO-correction is to correct the signal for the antenna 
#' separation (offset) and therefore to transform the signal to the signal we 
#' would have recorded with zero offset (\eqn{x = 0}). We write the vertical
#' two-way traveltime at zero offset 
#' \deqn{t_0 = t_{TWT}(x = 0) = \frac{2z}{v}}
#' Therefore, the NMO-correction \eqn{\Delta_{NMO}} is
#' \deqn{\Delta_{NMO} = t_{TWT}(x) - t_0}  
#' \deqn{\Delta_{NMO} = t_0 (\sqrt{1 + \frac{x^2}{v^2 t_0^2}} - 1)}
#' @param x An object of the class \code{GPR}
#' @param v A length-one numeric vector defining the radar wave velocity in 
#'          the ground
#' @rdname NMOCor-methods
#' @aliases NMOCor,GPR-method
#' @export
#' @references
#' \itemize{
#'   \item{Tillard and Dubois (1995) Analysis of GPR data: wave propagation
#'         velocity determination. Journal of Applied Geophysics, 33:77-91}
#'   \item{Shatilo and Aminzadeh (2000) Constant normal-moveout (CNMO) 
#'         correction: a technique and test results. Geophysical Prospecting,
#'         473-488}
#' }
setMethod("NMOCor", "GPR", function(x, v = NULL){
  if(!isCMP(x)){
    stop("survey mode of x is not multi-offset. ",
         "update survey mode:\n",
         "  surveymode(x) <- 'CMP'\n",
         " or\n",
         "  surveymode(x) <- 'WARR'\n")
  }
  if(is.null(v)){
    v <- x@vel[[1]]
  } 
  x <- .NMOCor(x, v = v, asep = x@antsep)
  proc(x) <- getArgs()
  return(x)
  }
)

.NMOCor <- function(x, v = NULL, asep = NULL){
  if(is.null(asep)){
    if(length(x@rec) == 0 || length(x@trans) == 0){
      asep <- seq(x@antsep, by = x@dx, length.out = length(x))
    }else{
      asep <- sqrt(colSums((x@rec - x@trans)^2))
    }
  }
  t0 <- round(x@time0[1]/x@dz)*x@dz
  tx0 <- x@depth - t0   # t(x = 0)
  #x <- x[(t0/x@dz + 1):nrow(x),]
  #x@time0 <- 0
  x_nmoCor <- x
  x_nmoCor@data[] <- 0
  if(is.null(v)){
    v <- x@vel[[1]]
  }
  for(i in seq_along(x)){
    tt <- sqrt( tx0^2 + (asep[i]^2 )/v^2 )
    tt[tx0 < 0] <- NA
    valreg <- signal::interp1(x = tx0, y = x@data[, i], xi = tt, 
                              method = "spline", extrap = NA)
    # deltaT <- sqrt( x@depth^2 + (asep[i]^2 )/v^2 )
    # newT <- 2*x@depth - deltaT
    # test <- newT > 0
    # valreg <- signal::interp1(x = newT[test], y = x@data[, i], 
    #                           xi = x@depth[test], 
    #                           method = "cubic", extrap = NA)
    x_nmoCor@data[,i] <- valreg
  }
  x_nmoCor@data[is.na(x_nmoCor@data)] <- 0
  x_nmoCor@data[is.infinite(x_nmoCor@data)] <- 0
  x_nmoCor@pos <- asep
  return(x_nmoCor)
}

semblance <- function(x){
  S <- sum(rowSums(x, na.rm = TRUE)^2) / 
           sum(rowSums(x^2, na.rm = TRUE)) * nrow(x)
  #S <- sum((apply(x, 1, sum, na.rm = TRUE))^2) /
  #  sum(apply((x)^2, 1, sum, na.rm = TRUE)) * nrow(x)
  return(S)
}


signalNoiseRatio <- function(x){
  ysvd <- svd(x)
  n <- length(ysvd$d)
  # estimator of the noise variance
  x_sig2 <- sum(ysvd$d[-1])/(n-1)
  # estimator of the signal energy
  P <- (ysvd$d[1] - x_sig2)/n
  return( P/x_sig2 )
}
	
signalNoiseRatio2 <- function(x){
  ysvd <- svd(x)
  m <- nrow(x)
  n <- length(ysvd$d)
  W <- m * log( 0 + (sum(ysvd$d)/n)^n / prod(ysvd$d) )^n
  # estimator of the noise variance
  x_sig2 <- sum(tail(ysvd$d, n-1))/(n-1)
  # estimator of the signal energy
  P <- (ysvd$d[1] - x_sig2)/n
  return( W * P/x_sig2 )
}


#' Velocity Analysis of CMP Gather
#' 
#' Transform the space-time domain of the radargram into a velocity-time domain to obtain 
#' the velocity spectrum (i.e. change in wave velocity with depth or time). This is achieved by applying
#' Normal Move-Out (NMO) corrections to the radargram for the range of selected velocities 
#' and computing a coherency measure for each result. In RGPR, the coherency measure can be defined using 
#' different functions: "semblance", "winsemblance", "wincoherence", "wincoherence2".   
#' 
#' either use 'rec' and 'trans' to compute the distance between the antennas
#' or give the distance between the antennas (asep)
#' or seq(x@antsep, by = x@dx, length.out = length(x))
#'
#' \describe{
#'   \item{semblance}{also described as the ratio of input to output
#'         energy (Niedell and Taner, 1971)}
#'   \item{semblance2}{windowed semblance}       
#'   \item{wincoherence}{Windowed coherence measure based on 
#'         eigen-decomposition that estimates the 
#'         signal-to-noise ratio for high resolution velocity analysis
#'         (Sacchi, 2002)}
#'   \item{wincoherence2}{Windowed coherence measure based on a log-generalized
#'         likelihood ratio which tests the hypothesis of equality of 
#'         eigenvalues (Key and Smithson, 1990)}
#' }
#' @param x An object of the class \code{GPR}
#' @param method A length-one character vector 
#' @param v A numeric vector defining at which velocities the analysis is
#'          performed
#' @param w A length-one numeric vector defining the window length for the
#'          methods 'wincoherence' and 'wincoherence2'.           
#' @rdname CMPAnalysis-methods
#' @aliases CMPAnalysis,GPR-method
#' @references
#' \itemize{
#'   \item{Neidell and Taner (1971) Semblance and other coherency measures
#'         for multichannel data. 
#'         Geophysics, 36(3):482-497.}
#'   \item{Key and Smithson (1990) New approach to seismic-reflection
#'         event detection and velocity determination. 
#'         Geophysics, 55(8):1057-1069.}
#'   \item{Textbook: Sacchi (2002) Statistical and Transform Methods
#'         in Geophysical Signal Processing}
#' }
#' @export
setMethod("CMPAnalysis", "GPR", function(x, method = c("semblance", 
                                         "winsemblance",   "wincoherence", 
                                         "wincoherence2"), v = NULL, 
                                          w = NULL){
  method <- match.arg(method, c("semblance", "winsemblance", 
                                "wincoherence", "wincoherence2"))
  if(is.null(v)){
    vlim <- x@vel[[1]] * c(0.5, 1.5)
    v <- seq(vlim[1], vlim[2], length = 50)
  }
  x <- time0Cor(x, method = "pchip")
  #TODO: create a new object > check slot consistency
  # as(matrix(0, nrow = nrow(x), ncol = length(v)), "GPR")
  x_velAna <- .NMOCor(x, v = max(v), asep = x@antsep)
  x_velAna <- x_velAna[, rep(1, length(v))]
  x_velAna@data[] <- 0
  # x_velAna <- matrix(0 nrow=nrow(test), ncol=length(vv))
  x_velAna@pos <- v
  if(method %in% c("wincoherence", "wincoherence2", "winsemblance")){
    if(is.null(w)){
      w <- ceiling(nrow(x))/20
    }
    wi <- round(w/x@dz)
    if(wi > ncol(x) || wi < 0 ) stop("w too large or too small")
    vabove <- seq_len(ceiling(w/2)) + 1
    # FIXME > use apply(x, i, FUN)
    if(method == "wincoherence"){
      for(i in seq_along(v)){
        y <- .NMOCor(x, v = v[i], asep = x@antsep)
        x_velAna@data[,i] <- wapplyRowC(y@data, width = wi, by = 1, 
                                     FUN = signalNoiseRatio)
        # x_velAna@data[floor(w/2) + seq_along(test),i] <- test
      }
    }else if(method == "wincoherence2"){
      for(i in seq_along(v)){
        y <- .NMOCor(x, v = v[i], asep = x@antsep)
        x_velAna@data[,i] <- wapplyRowC(y@data, width = wi, by = 1, 
                                     FUN = signalNoiseRatio2)
        # x_velAna@data[floor(w/2) + seq_along(test),i] <- test
      }
    }else if(method == "winsemblance"){
      for(i in seq_along(v)){
        y <- .NMOCor(x, v = v[i], asep = x@antsep)
        x_velAna@data[,i] <- wapplyRowC(y@data, width = wi, by = 1, 
                                        FUN = semblance)
        # x_velAna@data[floor(w/2) + seq_along(test),i] <- test
      }
    }
    x_velAna@data[vabove,] <- 0
  }else if(method == "semblance"){
    for(i in seq_along(v)){
      y <- .NMOCor(x, v = v[i], asep = x@antsep)
      x_velAna@data[,i] <- semblance(y@data)
      #x_velAna@data[,i] <- (apply(y@data, 1, sum, na.rm = TRUE))^2 / 
      #                    apply((y@data)^2, 1, sum, na.rm = TRUE)
    }
  }
  x_velAna@data[is.na(x_velAna@data)] <- 0
  x_velAna@data[is.infinite(x_velAna@data)] <- 0
  x_velAna@surveymode <- "CMPANALYSIS"
  x_velAna@antsep <- 0
  x_velAna@time0 <- 0
  proc(x_velAna) <- getArgs()
  return(x_velAna)
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
setMethod("migration", "GPR", function(x, type = c("static", "kirchhoff"),...){
    if(length(x@antsep) == 0 || (!is.numeric(x@antsep))){
    stop("You must first define the antenna separation",
          "with `antsep(x)<-...`!")
    }
    if(is.null(x@vel) || length(x@vel)==0){
      stop("You must first define the antenna separation",
          "with `vel(x)<-...`!")
    }
    type <- match.arg(type, c("static", "kirchhoff"))
    if(type == "static"){  
      ntr <- ncol(x@data)
      if(ncol(x@coord) == 3 && length(x@coord[,3])>= ntr){
        topo <- x@coord[1:ntr,3]
      }else{
        topo <- rep.int(0L, ntr)
        message("no topo!")
      }
      if(x@depthunit == "ns"){
        # "migration"
        message("time to depth conversion with constant velocity", x@vel[[1]])
        x@dz <-  x@dz * x@vel[[1]]/ 2
        x@depthunit <- "m"
        time_0 <- mean(x@time0)
        # depth_0 <- time_0 * x@vel[[1]]/ 2
        depth_0 <- depthToTime(z = 0, time_0 , v = x@vel[[1]], 
                               antsep = x@antsep) *  x@vel[[1]]/ 2
        depth_all <- x@depth * x@vel[[1]]/ 2
        # shift to time0
        sel <- c(round((depth_0 - depth_all[1]) / x@dz):nrow(x))
        # sel <- c(round((depth_0)/x@dz):nrow(x))
        x <- x[sel,]
      }
      x@data <- .topoShift(x@data,topo,dz = x@dz)
      x@depth     <- seq(0, by = x@dz, length.out = nrow(x@data))  
      x@time0     <- rep(0,length(x@time0))
      x@vel       <- list()  # FIX ME!!
      x@time0     <- rep(0L,ncol(x@data))  # FIX ME!!
      x@coord[,3] <- max(x@coord[,3])
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
      FUN <- sum
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
        if( !is.null(dots$FUN)){
          FUN <- dots$FUN
        }
      }  
      x@data      <- .kirMig(x@data, topoGPR = x@coord[,3], dx = x@dx,
                             dts = x@dz, v = x@vel[[1]], max_depth = max_depth, 
                             dz = dz, fdo = fdo, FUN = FUN)
      x@depth     <- seq(0,by=dz, length.out = nrow(x))
      x@time0     <- rep(0, ncol(x))
      x@dz        <- dz
      x@depthunit <- x@posunit          # check!!!
      x@coord[,3] <- max(x@coord[,3])
    }
    proc(x) <- getArgs()
    return(x)
  } 
)


#---------------------- INTERPOLATION ---------------------#  
#' Up-sample the GPR data (1D and 2D sinc-interpolation)
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

#' Trace interpolation at regularly spaced positions 
#'
#' @name regInterpPos
#' @rdname regInterpPos
#' @export
setMethod("regInterpPos", "GPR", function(x, type = c("linear", "cosine"), 
          dx = NULL){
    type <- match.arg(type, c("linear", "cosine"))
    if(length(x@coord)>0){
      xpos <- posLine(x@coord)
    }else{
      xpos <- x@pos
    }
    if(is.null(dx)){
      dx <- min(abs(diff(xpos)))
    }
    xo <- seq(min(xpos), max(xpos), by = dx)
    xnew <- x[,rep(1, length(xo))]
    for(k in seq_along(xo)){
      if( any(di <- abs(xo[k] - xpos) < 10^-3) ){
        ki <- which(di)[1]
        xnew@data[, k] <- x@data[,ki]
        xnew@time[k] <- x@time[ki]
        xnew@time0[k] <- x@time0[ki]
        if(length(x@coord) > 0){
          xnew@coord[k,] <- x@coord[ki,]
        }
        if(length(x@rec) > 0){
          xnew@rec[k,] <- x@rec[ki,]
        }
        if(length(x@trans) > 0){
          xnew@trans[k,] <- x@trans[ki,]
        }
      }else{
        testm <- xpos < xo[k]
        testp <- xpos > xo[k]
        km <- ifelse(any(testm), tail(which(testm),1), 0)
        kp <- ifelse(any(testm), which(testp)[1], 0)
        w <- (xpos[kp] - xo[k])/(xpos[kp] - xpos[km])
  #       wm <- 1/(xo[k] - xpos[km]) 
  #       wp <- 1/(xpos[kp] - xo[k])
  #       xnew@data[, k] <- (wm*x@data[,km] + wp*x@data[,kp])/(wm + wp)
        if(type == "linear"){
        }else if(type == "cosine"){
          w <- (1-cos(w*pi))/2
        }
        xnew@data[,k] <- w * x@data[,km] + (1 - w) * x@data[,kp]
        xnew@time[k] <- round( w * x@time[km] + (1 - w) * x@time[kp] )
        xnew@time0[k] <- w * x@time0[km] + (1 - w) * x@time0[kp]
        if(length(x@coord) > 0){
          xnew@coord[k,] <- w * x@coord[km,] + (1 - w) * x@coord[kp,]
        }
        if(length(x@rec) > 0){
          xnew@rec[k,] <- w * x@rec[km,] + (1 - w) * x@rec[kp,]
        }
        if(length(x@trans) > 0){
          xnew@trans[k,] <- w * x@trans[km,] + (1 - w) * x@trans[kp,]
        }
      }
    }
    xnew@traces <- seq_along(xo)
    xnew@pos <- xo
    xnew@fid <- interpFid(xpos, xo, x@fid)
    xnew@ann <- interpFid(xpos, xo, x@ann)
    xnew@dx <- round(dx,3)
    proc(xnew) <- getArgs()
#     xnew@proc <- c(xnew@proc, proc)
    return(xnew)
  }
)

# interpolate position fiducial/annotation
  interpFid <- function(xposold, xposnew, fidOld){
    fidNew <- rep("", length(xposnew))
    fids <- which(fidOld != "")
    for(i in seq_along(fids)){
      fidpos <- which.min(abs(xposold[fids[i]] - xposnew))
      fidNew[fidpos] <- fidOld[fids[i]]
    }
    return(fidNew)
  }

  
#---------------------- STRUCTURE TENSOR ---------------------#
setMethod("strTensor", "GPR", function(x,  blksze = c(2, 4),
                        kBlur   = list(n = 1, m = 1, sd = 1), 
                        kEdge   = list(n = 5, m = 5, sd = 1), 
                        kTensor = list(n = 5, m = 5, sd = 1),
                        thresh = 0.02, what = c("tensor", "mask"), ...){
    O <- .strucTensor(P = x@data, dxy = c(x@dx, x@dz), 
                blksze = blksze,
                kBlur   = kBlur, 
                kEdge   = kEdge, 
                kTensor = kTensor,
                thresh = thresh)  
    output <- list()
    whatref <- c("tensor", "vectors", "values", "polar", "mask")
    what <- what[what %in% whatref]
    if(length(what) == 0){
      stop(paste0("argument 'what' only accepts a character vector composed of",
          " at least one of the following words:\n",
          "'tensor', 'vectors', 'values', 'polar', 'mask'"))
    }
    if( "orientation" %in% what){ 
      xOrient <- x
      xAni    <- x
      xEnergy <- x
      xOrient@data       <- O$polar$orientation
      xOrient@surveymode <- "orientation"
      xEnergy@data       <- O$polar$energy
      xEnergy@surveymode <- "energy"
      xAni@data          <- O$polar$anisotropy
      xAni@surveymode    <- "anisotropy"
      output[["orientation"]] <- list("energy"      = xEnergy,
                                      "anisotropy"  = xAni,
                                      "orientation" = xOrient)
    }
    if( "tensor" %in% what){
      xJxx <- x
      xJyy <- x
      xJxy <- x
      xJxx@data <- O$tensor$xx
      xJyy@data <- O$tensor$yy
      xJxy@data <- O$tensor$xy
      xJxx@surveymode <- "tensorxx"
      xJyy@surveymode <- "tensoryy"
      xJxy@surveymode <- "tensorxy"
      output[["tensor"]] <- list("xx" = xJxx,
                                 "yy" = xJyy,
                                 "xy" = xJxy)
    }
    if( "mask" %in% what){
      mask <- x
      mask@data <- O$mask
      mask@surveymode <- "mask"
      output[["mask"]] <- mask
    }
    return(output)
  }
)


#----------------------- SAVE/EXPORT ------------------------#
#' Write the GPR object in a file.
#'
#' @name writeGPR
#' @rdname writeGPR
#' @export
setMethod("writeGPR", "GPR", function(x, fPath = NULL, 
      type = c("DT1", "rds", "ASCII", "xyzv"),
      overwrite = FALSE, ...){
    type <- match.arg(tolower(type), c("dt1", "rds", "ascii", "xyzv"))
    fPath <- ifelse(is.null(fPath), x@name, 
                    file.path(dirname(fPath), .fNameWExt(fPath)))
    ext <- switch(type,
                  "dt1" = ".dt1",
                  "rds" = ".rds",
                  "ascii" = ".txt",
                  "xyzv" = ".txt")
    fPath <- paste0(fPath, ext)
    testFile <- file.exists(fPath)
    if(isTRUE(overwrite)){
      if(testFile) message("File overwritten\n")
    }else if(testFile){
      stop("File already exists. Cannot overwrite!\n")
    }
    x@filepath <- fPath
    switch(type,
           "dt1" = {.writeDT1(x, fPath)},
           "rds" = {namesSlot <- slotNames(x)
                    xList <- list()
                    # xList[["version"]] <- "0.1"
                    for(i in seq_along(namesSlot)){
                      xList[[namesSlot[i]]] <- slot(x, namesSlot[i])
                    }
                    saveRDS(xList, fPath)},
            # idea: add header data
            "ascii" = {write.table(as.matrix(x), file = fPath, 
                       quote = FALSE, col.names = x@pos, row.names = x@depth,
                       ...)},
            "xyzv" = {xyzv <- matrix(nrow=prod(dim(x)), ncol = 4)
                      colnames(xyzv) <- c("x", "y", "z", "v")
                      xyzv[, 4]  <- as.vector(as.matrix(x))
                      xyzv[,1:3] <-  kronecker(x@coord, matrix(1,nrow(x),1))
                      xyzv[,3]   <- rep(max(xyzv[,3]), ncol(x)) - 
                                        rep(x@depth, times = ncol(x))
                      write.table(xyzv, file = fPath, quote = FALSE, 
                      col.names = TRUE, row.names = FALSE, ...)}
          )
    invisible(return(x))
  } 
)


#-----------------
#' Export a PDF showing the GPR profile.
#'
#' @name exportPDF
#' @rdname exportPDF
#' @export
setMethod("exportPDF", "GPR", 
function(x, fPath = NULL, addTopo = FALSE, clip = NULL, normalize = NULL, 
        nupspl = NULL,  type = "wiggles",...){
    if(is.null(fPath)){
      stop("fPath must be given\n")
    }
    if(any(dim(x) == 1)){
      stop("no export because dim = 1\n")
    }
    plot(x, clip = clip, addTopo = addTopo, type = type, 
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
      if(length(x@coord) > 0 && ncol(x@coord) == 3){
        FID$E <- x@coord[tr,1]
        FID$N <- x@coord[tr,2]
        FID$Z <- x@coord[tr,3]
      }
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
  fPath = NULL, driver = "ESRI Shapefile", ...){
  type <- match.arg(type, c("SpatialPoints", "SpatialLines", "ASCII"))
  fPath <- ifelse(is.null(fPath), x@name, 
                    file.path(dirname(fPath), .fNameWExt(fPath))) 
  if(type=="SpatialLines"){  
    mySpatLines <- as.SpatialLines(x)
    dfl <- data.frame(z=c(1), row.names = x@name)
    spldf <- sp::SpatialLinesDataFrame(mySpatLines, dfl , 
                      match.ID = TRUE)
    rgdal::writeOGR(obj = spldf, dsn = dirname(fPath), layer = basename(fPath), 
                    driver = driver, check_exists = TRUE, 
                    overwrite_layer = TRUE, delete_dsn = TRUE)
  }else if(type=="SpatialPoints"){  
    spp <- as.SpatialPoints(x)
    rgdal::writeOGR(obj = spp, dsn = dirname(fPath), layer = basename(fPath), 
                    driver = driver, check_exists = TRUE, 
                    overwrite_layer = TRUE, delete_dsn = TRUE)
  }else if(type == "points"){
    stop("use type = SpatialPoints instead.\n")
  }else if(type == "lines"){
    stop("use type = SpatialLines instead.\n")
  }else if(type == "ASCII"){
    xCoord <- x@coord
    colnames(xCoord) <- c("E","N","Z")
    fPath <- paste0(fPath, ".txt")
    write.table(xCoord, fPath, row.names = FALSE, 
                    col.names = TRUE, quote = FALSE, ...)
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
