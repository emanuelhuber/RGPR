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
    version      = "character",   # version of the class
    data         = "matrix",     # one column per trace
    traces       = "numeric",  # numbering of each trace (from 1 to ntr)
    depth        = "numeric",  # depth position
    pos          = "numeric",    # position  of the traces
    time0        = "numeric",  # time-zero (first air-wave arrival)
    time         = "numeric",   # time of the trace recording
    fid          = "character",   # fiducial marks, defaults = rep("", ncol(x))!
    ann          = "character",  # annotation (e.g. intersections)
    coord        = "matrix",   # coordinates (x,y,z) of each traces
    rec          = "matrix",     # coordinates (x,y,z) of the receiver antenna
    trans        = "matrix",   # coordinates (x,y,z) of the transmitter antenna
    coordref     = "numeric", # coordinates references
    freq         = "numeric",   # antenna frequency
    dz           = "numeric",   # time/depth sampling
    dx           = "numeric",     # spatial trace sampling
    antsep       = "numeric",   # antenna separation
    name         = "character",  # name of the profile
    description  = "character",  # description of the pro
    filepath     = "character",  # filepath of the profile
    depthunit    = "character", # time/depth unit
    posunit      = "character",  # spatial unit
    surveymode   = "character", # survey mode (reflection/CMP)
    date         = "character",    # date of the survey , format %Y-%m-%d
    crs          = "character",  # coordinate reference system of coord
    proc         = "character",  # processing steps
    vel          = "list",      # velocity model
    delineations = "list",  # delineated lines
    hd           = "list"      # header from *.dt1 file
  )
)




#============================== READ GPR DATA =================================#

#' Extract frequency from string
#' 
#' Extract with regex the antenna frequency in a string
#' @export
freqFromString <- function(s){
  if(grepl("MHz", s, ignore.case = TRUE)){
    a <- regexpr("[0-9]+.MHZ",  s, ignore.case = TRUE, perl = TRUE)
  }else{
    a <- regexpr("[0-9]+",  s, ignore.case = TRUE, perl = TRUE)
  }
  b <- regmatches(s,  a)
  as.numeric(gsub("[^0-9]", "", b))
}
# s <- "1230 fds 200-MHZ 12.3"
# s <- "1230MLF"
# s <- "D1230MLF"
# freqFromString(s)


#' @export
antSepFromAntFreq <- function(antfreq, verbose = TRUE){
  ant <- list(f = c(12.5, 25, 50, 100, 110, 200, 225, 450,   900, 1200),
              s = c( 8,    4,  2,   1,   1, 0.5, 0.5, 0.25, 0.17, 0.075))
  antsep <- approx(ant$f, ant$s, xout = antfreq)$y
  antsep <- round(antsep, 3)
  if(verbose){
    message("Antenna separation (", antsep, " m) estimated from antenna", 
            " frequency (", antfreq, " MHz).",
            "\nCorrect if wrong with 'antsep(x) <- ...'")
  }
  if(is.na(antsep)) antsep <- numeric(0)
  return(antsep)
}



# x = classical GPR list
.gpr <- function(x, fName = character(0), desc = character(0),
                 fPath = character(0), Vmax = 50){
  rec_coord <- cbind(x$dt1$recx, x$dt1$recy, x$dt1$recz)
  trans_coord <- cbind(x$dt1$transx, x$dt1$transy, x$dt1$transz)
  if(sum(is.na(rec_coord)) > 0){
    warning(paste(fName,": ",sum(is.na(rec_coord)), 
                  "NA's in the receiver coordinates\n"))
  }
  if(sum(is.na(trans_coord)) > 0){
    warning(paste(fName,": ",sum(is.na(trans_coord)), 
                  "NA's in the transmitter coordinates\n"))
  }
  if(sum(is.na(x$dt1$topo)) > 0){
    warning(paste(fName,": ",sum(is.na(x$dt1$topo)), 
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
  # antfreq <- freqFromString(.getHD(x$hd, "NOMINAL FREQUENCY", position=TRUE))
  antfreq <- .getHD(x$hd, "NOMINAL FREQUENCY", position=TRUE)
  if(!is.null(antfreq)){
    pos_used[antfreq[2]] <- 1L
    antfreq <- freqFromString(antfreq[1])
  }else{
    antfreq <- 0
    message("Antenna frequency set to 0 MHz. Set it with 'antfreq(x) <- ... '")
  }
  antsep <- .getHD(x$hd, "ANTENNA SEPARATION", position=TRUE)[1]
  if(!is.null(antsep)){
    pos_used[antsep[2]] <- 1L
  }else{
    antsep <- 0
    message("Antenna separation set to 0 ", posunit, 
            ". Set it with 'antsep(x) <- ... '")
    # antsep <- antSepFromAntFreq(antfreq[1])
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
    d <- format(Sys.time(), "%Y-%m-%d")
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
  new("GPR",   
      version     = "0.2",
      data        = byte2volt(Vmax = Vmax)*x$data,
      traces      = x$dt1$traces,            # x$dt1$traces
      fid         = trimStr(x$dt1$com),         # x$dt1$fid    <-> x$dt1$x8
      coord       = coord,                    # x$dt1$topo  of the traces
      pos         = x$dt1$pos,                  # x$dt1$position  of the traces
      depth       = seq(0, by = dz, length.out = nrow(x$data)),
      rec         = rec_coord,      # x$dt1$recx,x$dt1$recy,x$dt1$recz
      trans       = trans_coord,
      time0       = time_0,                   # x$dt1$time0
      time        = traceTime,                       # x$dt1$time
      proc        = character(0),              # processing steps
      vel         = list(0.1),                  # m/ns
      name        = fName,
      description = desc,
      filepath    = fPath,
      dz          = dz,  
      dx          = dx[1],                       # "STEP SIZE USED"
      depthunit   = "ns",
      posunit     = posunit[1],
      freq        = antfreq[1], 
      antsep      = antsep[1], 
      surveymode  = surveymode[1],
      date        = d,
      crs         = character(0),
      hd          = sup_hd                      # header
  )
}


.gprImpulseRadar <- function(x, fName = character(0), desc = character(0),
                             fPath = character(0), Vmax = 50){ 
  rec_coord <- matrix(nrow = 0, ncol = 0)
  trans_coord <- matrix(nrow = 0, ncol = 0)
  coord <- matrix(nrow = 0, ncol = 0)
  pos_used <- integer(nrow(x$hd))
  nBytes <- .getHD(x$hd, "DATA VERSION", position = TRUE)
  if(!is.null(nBytes)){
    pos_used[nBytes[2]] <- 1L
  }else{
    nBytes <- 16
  }
  nTr <- ncol(x$data)
  dx <- .getHD(x$hd, "USER DISTANCE INTERVAL", position = TRUE)
  if(!is.null(dx)){
    pos_used[dx[2]] <- 1L
  }else{
    dx <- 1
  }
  ttw  <- .getHD(x$hd,"TIMEWINDOW", position = TRUE)
  if(!is.null(ttw)){
    dz <- ttw[1]/nrow(x$data)
    pos_used[ttw[2]] <- 1L
  }else{
    warning("time/depth resolution unknown! I take dz = 0.4 ns!\n")
    dz <- 0.4
    ttw  <- nrow(x$data) * dz
  }
  nT0 <- .getHD(x$hd, "ZERO LEVEL", position = TRUE)
  if(!is.null(nT0)){
    pos_used[nT0[2]] <- 1L
  }else{
    nT0 <- 1
  }
  if(!is.null(x$time)){
    traceTime <- as.double(as.POSIXct(paste(x$time[,2], x$time[,3])))
  }else{
    traceTime <- rep(0, nTr)
  }
  afreq <- .getHD(x$hd, "ANTENNA", position = TRUE, number = FALSE)
  if(!is.null(afreq)){
    antfreq <- freqFromString(afreq[1])
    pos_used[as.integer(afreq[2])] <- 1L
  }else{
    antfreq <- 0
    message("Antenna frequency set to 0 MHz. Set it with 'antfreq(x) <- ... '")
  }
  antsep <- .getHD(x$hd, "ANTENNA SEPARATION", position = TRUE)
  if(!is.null(antsep)){
    pos_used[antsep[2]] <- 1L
  }else{
    # antsep[1] <- antSepFromAntFreq(antfreq)
    antsep[1] <- 0
    message("Antenna separation set to 0 ", "m", 
            ". Set it with 'antsep(x) <- ... '")
  }
  surveyDate <- .getHD(x$hd, "DATE", position = TRUE, number = FALSE)
  if(!is.null(surveyDate)){
    d <- as.character(as.Date(surveyDate[1], "%Y-%m-%d"))
    pos_used[surveyDate[2]] <- 1L
  }else{
    surveyDate[1] <- 0
  }
  x$hd2 <- x$hd[!pos_used,]
  if(nrow(x$hd2) > 0){
    key <-  trimStr(x$hd2[,1])
    test <- key!=""
    key <- key[test]
    key2 <- gsub("[[:punct:]]", replacement = "", key)
    key2 <- gsub(" ", replacement = "_", key2)
    nameL <- trimStr(x$hd2[test,2])
    names(nameL) <- as.character(key2)
    sup_hd <- as.list(nameL)
  }
  
  new("GPR",   version="0.2",
      data = byte2volt(Vmax = Vmax, nBytes = nBytes[1])*x$data,
      traces = seq_len(nTr),                       # trace number
      fid = rep("", nTr),                          # markes/fid
      coord = coord,                               # trace coordinates
      pos = seq(0, by = dx[1], length.out = nTr),  # trace position
      depth = seq(0, by = dz, length.out = nrow(x$data)),
      rec = rec_coord,                             # recorder coordinates
      trans = trans_coord,                         # transmitter coordinates
      time0 = rep((nT0[1] - 1) * dz, nTr),            # time-zero
      time = traceTime,                            # sampling time
      proc = character(0),                         # processing steps
      vel = list(0.1),                             # m/ns
      name = fName,
      description = desc,
      filepath = fPath,
      dz = dz, 
      dx = dx[1],                                   # "STEP SIZE USED"
      depthunit = "ns",
      posunit = "m",
      freq = antfreq, 
      antsep = antsep[1], 
      surveymode = "reflection",
      date = d,
      crs = character(0),
      hd = sup_hd                      # header
  )
}


.gprRD3 <- function(x, fName = character(0), desc = character(0),
                    fPath = character(0), nBytes = 16, Vmax = 50){  
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
  # s <- "GX450 HDR (v.1)=3" 
  X <- .getHD(x$hd, "ANTENNAS", position = TRUE, number = FALSE)
  # X2 <- strsplit(X[1], " ")
  # gsubwrap <- function(x, ...){
  #   grep('[0-9]{2,3}', x, value = TRUE)
  # }
  # antfreq <- as.numeric(gsub('[^0-9]', '', sapply(X2, gsubwrap)))
  antfreq <- freqFromString(X[1])
  # freq <- as.numeric(gsub('[^0-9]', '', freqS[1]))
  if(!is.null(antfreq) && !is.na(antfreq)){
    pos_used[X[2]] <- 1L
  }else{
    antfreq <- 0
    message("Antenna frequency set to 0 MHz. Set it with 'antfreq(x) <- ... '")
  }
  # OK
  antsep <- .getHD(x$hd, "ANTENNA SEPARATION", position=TRUE)
  if(!is.null(antsep)){
    pos_used[antsep[2]] <- 1L
  }else{
    # antsep <- antSepFromAntFreq(antfreq)
    antsep <- 0
    message("Antenna separation set to 0 ", "m", 
            ". Set it with 'antsep(x) <- ... '")
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
  new("GPR",   
      version     = "0.2",
      data        = byte2volt(Vmax = Vmax, nBytes = nBytes)*x$data,
      traces      = 1:ncol(x$data),
      fid         = rep("", ncol(x$data)),
      #coord = coord,
      coord       = matrix(nrow=0, ncol = 0),
      pos         = seq(0, by = dx[1], length.out = ncol(x$data)),
      depth       = seq(0, by = dz, length.out = nrow(x$data)),
      rec         = matrix(nrow = 0, ncol = 0),
      trans       = matrix(nrow = 0, ncol = 0),
      time0       = rep(0, ncol(x$data)),
      time        = numeric(0),
      proc        = character(0),
      vel         = list(0.1),
      name        = fName,
      description = desc,
      filepath    = fPath,
      dz          = dz, 
      dx          = dx[1],
      depthunit   = "ns",
      posunit     = "m",
      freq        = antfreq[1], 
      antsep      = antsep[1], 
      surveymode  = "reflection",
      date        = format(Sys.time(), "%Y-%m-%d"),
      crs         = character(0),
      hd         = sup_hd
  )
}


.gprSGY <- function(x, fName = character(0), desc = character(0),
                    fPath = character(0), Vmax = 50){
  n <- ncol( x$DTR$data)
  data_xyz <- matrix( c(x$DTR$trHD[16,], x$DTR$trHD[17,], x$DTR$trHD[12,]),
                      nrow = n, ncol = 3, byrow = FALSE)
  data_pos <- posLine(data_xyz)
  
  if(!all(diff(data_pos) > 0)){
    data_pos <- seq_len(n)
    x$BHD$xyz <- data_xyz
    data_xyz <- matrix(nrow = 0, ncol = 0)
    message("No increasing inter-trace distances, ",
            "I ignore the trace coordinates. ",
            "Check the coordinates in 'gethd(x)$xyz'")
  }
  
  # check date
  # 22 = year, 23 = day of the year
  if(all(c(x$DTR$trHD[22, 1], x$DTR$trHD[23, 1]) == 0)){
    data_date <- format(Sys.time(), "%Y-%m-%d")
  }else{
    data_date <- as.character(format(as.Date(x$DTR$trHD[23, 1] - 1, 
                                             origin = paste0(x$DTR$trHD[22, 1], "-01-01")),
                                     "%Y-%m-%d"))
  }
  
  # check time
  data_time <- seq_len(n)
  
  data_dt <- x$DTR$trHD[21,1]*10^3
  
  antfreq <- 0
  message("Antenna frequency set to 0 MHz. Set it with 'antfreq(x) <- ... '")
  antsep <- 0
  message("Antenna separation set to 0 ", "m", 
          ". Set it with 'antsep(x) <- ... '")
  
  # extract information from textual header data (only if valid encoding)
  scl <- 1
  data_crs <- character(0)
  # if(all(validEnc(x$THD))){
    k <- verboseF(grepl("epsg:", x$THD, ignore.case = TRUE), verbose = FALSE)
    epsg_code <- regmatches(x$THD[k], regexpr("epsg:[0-9]+" , x$THD[k], ignore.case = TRUE))
    if(length(epsg_code) >= 1){
      epsg_code <- gsub("[^0-9.]", "", epsg_code[1], ignore.case = TRUE)
      data_crs <- paste0("+init=epsg:", epsg_code)
    }
    
    k <- verboseF(grepl("scale factor ", x$THD, ignore.case = TRUE), verbose = FALSE)
    scale_fact <- regexpr("scale factor (?<scale>[0-9.]+)", x$THD[k], 
                         perl = TRUE, ignore.case = TRUE)
    if(length(k) > 1 && scale_fact[1] != -1){
      i1 <- attr(scale_fact, "capture.start")
      i2 <- i1 + attr(scale_fact, "capture.length") -1
      scl <- as.numeric(substr(x$THD[k], i1, i2))
    }
  # }
  
  y <- new("GPR",   
           version      = "0.2",
           data        = byte2volt(Vmax = Vmax, nBytes = x$BHD$DATA_BYTES) * x$DTR$data,
           traces      = x$DTR$trHD[1,],
           fid         = rep("", n),
           coord       = data_xyz * scl,
           pos         = data_pos,
           depth       = seq(0, by = data_dt, length.out = nrow(x$DTR$data)),
           rec         = matrix(nrow = 0, ncol = 0),
           trans       = matrix(nrow = 0, ncol = 0),
           time0       = rep(0, n),
           time        = data_time,
           proc        = character(0),
           vel         = list(0.1),
           name        = fName,
           description = desc,
           filepath    = fPath,
           dz          = data_dt, 
           dx          = mean(diff(data_pos)),
           depthunit   = "ns",
           posunit     = "m",
           freq        = antfreq, 
           antsep      = antsep,     # check
           surveymode  = "reflection",
           date        = data_date,
           crs         = data_crs,
           hd          = c(x$THD, x$BHD)
  )
  
  if( identical(x$DTR$trHD[1, ], x$DTR$trHD[2, ])){
    # plot(y)
    return(y)
  }else{
    trc_seq <- x$DTR$trHD[2, ]
    trc_seq_unique <- unique(trc_seq)
    if(length(trc_seq_unique) > 1){
      # Y <- list()
      Ys <- GPRsurveyEmpty(length(trc_seq_unique))
      for(i in seq_along(trc_seq_unique)){
        # Y[[i]] <- y[, trc_seq %in% trc_seq_unique[i]]
        Ys[[i]] <- y[, trc_seq %in% trc_seq_unique[i]]
      }
      message("I return an object of the class 'GPRsurvey' (contains many ",
              "GPR lines). ",
              "'plot(x)' will display the position of the traces. ",
              "To plot a single line, use 'plot(x[[1]])'")
      return(Ys)
    }else{
      return(y)
    }
  }
}

.gprSEGY <- function(x, fName = character(0), desc = character(0),
                     fPath = character(0), Vmax = 50){  
  
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
  if(length(fName) == 0){
    x_name <- paste0("LINE", x$hd$LINE_NUMBER)
  }else{
    x_name <- fName
  }
  # Antenna frequency
  antfreq0 <- grep("(antenna).*([0-9])+", x$hd$EBCDIC, ignore.case = TRUE, 
                   value = TRUE)
  antfreq <- as.numeric(gsub("[^0-9]", "", antfreq0))
  # antfreq <- freqFromString(x$hd$EBCDIC) FIXME.
  if(length(antfreq) == 0){
    antfreq <- 0
    message("Antenna frequency set to 0 MHz. Set it with 'antfreq(x) <- ... '")
  }
  antsep <- 0
  message("Antenna separation set to 0 ", "m", 
          ". Set it with 'antsep(x) <- ... '")
  # if(length(antfreq) > 0){
  #   antsep <- antSepFromAntFreq(antfreq)
  # }else{
  #   antfreq <- numeric(0)
  #   antsep <- numeric(0)
  #   message("Please, add antenna frequency and antenna separation.")
  # }
  # Date
  dd <- format(Sys.time(), "%Y-%m-%d")
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
  new("GPR",   version="0.2",
      data = byte2volt(Vmax = Vmax)*x$data,
      traces = 1:ncol(x$data),
      fid = rep("", ncol(x$data)),
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
      description = desc,
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

.gprTXT <- function(A, fName = character(0), desc = character(0),
                    fPath = character(0), Vmax = NULL){  
  
  if(!is.null(A$depth) && !is.null(A$pos)){
    x <- list(data = byte2volt(Vmax = Vmax)*A$data,
              pos = A$pos,
              depth = A$depth,
              name = fName,
              filepath = fPath)
  }else{
    x <- list(data = byte2volt(Vmax = Vmax)*A$data)
  }
  y <- as(x, "GPR") 
  if(desc != "") description(y) <- desc
  return(y)
}

.gprDZT <- function(x, fName = character(0), desc = character(0),
                    fPath = character(0), Vmax = 50, ch = 1){
  
  #  take the channel ch
  if(ch > length(x$data)){
    stop("The data has only ", length(x$data), "channel(s)")
  }
  x$data <- x$data[[ch]]
  antName <- x$hd$ANT[ch]
  
  dd <- as.Date(x$hd$DATE, format = "%Y-%m-%d")
  dd <- as.character(dd)
  if(is.na(dd)){
    dd <- format(Sys.time(), "%Y-%m-%d")
  }
  ttime <- yy <- 1/x$hd$SPS * (seq_len(ncol(x$data)) - 1)
  traceTime <- as.double(as.POSIXct(strptime(paste(dd, "01:30:00"), 
                                             "%Y-%m-%d %H:%M:%S") )) + ttime
  if(length(fName) == 0){
    x_name <- "LINE"
  }else{
    x_name <- fName
  }
  # defaults
  x_posunit   <- "m"
  x_depthunit <- "ns"
  x_pos       <- x$pos[1:ncol(x$data)]
  x_depth     <- x$depth[1:nrow(x$data)]
  x_dx        <- 1 / x$hd$SPM

  # Fiducial markers > each class has a different name (letter)
  x_fid       <- rep("", ncol(x$data))
  test <- which(x$hd$MRKS < 0)
  fidval <- LETTERS[as.numeric(as.factor(x$hd$MRKS[test]))]
  ufidval <- unique(fidval)
  for( i in seq_along(ufidval)){
    test2 <- which(fidval == ufidval[i])
    fid_nb <- seq_along(test2)
    x_fid[test][test2] <- paste0(ufidval[i], 
                                 sprintf(paste0("%0", max(nchar(fid_nb)), "d"), 
                                                     fid_nb))
  }
  
  if(!is.null(x$dzx)){
    # spatial/horizontal units
    # pos
    if(!is.null(x$dzx$pos)){
      x_pos <- x$dzx$pos
      # x_dx <- mean(diff(x_pos))
    }
    # spatial sampling
    if(!is.null(x$dzx$dx)){
      x_dx <- x$dzx$dx
    } 
    # if(!is.null(x$dzx$unitsPerScan)){
    #   x_dx <- x$dzx$unitsPerScan
    # }
    # fids/markers
    if(all(x_fid == "") &&
       !is.null(x$dzx$markers) && 
       length(x$dzx$markers) == ncol(x$data)){
      x_fid <- x$dzx$markers
    }
    # else if(all(x_fid == "") && !is.null(x$dzx$unitsPerMark)){
    #   x_fid <- rep("", ncol(x$data))
    #   x_fid_id <- which((x_pos %% x$dzx$unitsPerMark) == 0)
    #   x_fid[x_fid_id] <- "FID"
    # }
    if(!is.null(x$dzx$hUnit)){
      x_posunit <-x$dzx$hUnit
      if(grepl("in", x_posunit)){
        x_pos <- x_pos * 0.0254
        x_posunit <- "m"
      }
    }
    # # depth/vertical units
    # if(!is.null(x$dzx$vUnit)){
    #   x_depthunit <- x$dzx$vUnit
    #   if(grepl("in", x_depthunit)){
    #     x_depth <- x_depth * 0.0254
    #     x_depthunit <- "m"
    #   }
    # }
  }
  antfreq <- switch(antName,
                    '3200'   = numeric(0), # adjustable
                    '3200MLF' = numeric(0), # adjustable
                    '500MHz' = 500,
                    '3207' = 100,
                    '3207AP' = 100,
                    '5106' = 200,
                    '5106A' = 200,
                    '50300' = 300,
                    '350' = 350,
                    '350HS' = 350,
                    '50270' = 270,
                    '50270S' = 270,
                    '50400' = 400,
                    '50400S' = 400,
                    '800' = 800,
                    '3101' = 900,
                    '3101A' = 900,
                    '51600' = 1600,
                    '51600S' = 1600,
                    '62000' = 2000,
                    '62000-003' = 2000,
                    '62300' = 2300,
                    '62300XT' = 2300,
                    '52600' = 2600,
                    '52600S' = 2600,
                    'D50800' = 800,
                    numeric(0))  # 800,300,
  if(length(antfreq) == 0){
    # estimate anntenna frequency from the name (it it contains ### MHz)
    antfreq <- freqFromString(antName)
  }
  if(length(antfreq) == 0){
    antfreq <- 0
    message("Antenna frequency set to 0 MHz. Set it with 'antfreq(x) <- ... '")
   # antsep <- numeric(0)
  }
  #else{
  #}
  v <- 2 * x$hd$DEPTH / x$hd$RANGE
  
  # antenna sparation could be estimated from frequency...
  # antsep <- antSepFromAntFreq(antfreq)
  antsep <- 0
  message("Antenna separation set to 0 ", x_posunit, 
          ". Set it with 'antsep(x) <- ... '")
  
  new("GPR",   
      version      = "0.2",
      data        = byte2volt(Vmax = Vmax, nBytes = x$hd$BITS) * x$data,
      traces      = 1:ncol(x$data),
      fid         = x_fid,
      #coord = coord,
      coord       = matrix(nrow = 0, ncol = 0),
      pos         = x_pos,
      depth       = x$depth[1:nrow(x$data)],
      rec         = matrix(nrow = 0, ncol = 0),
      trans       = matrix(nrow = 0, ncol = 0),
      time0       = rep(0, ncol(x$data)),
      # time = x$hdt[1,] * 3600 + x$hdt[2,] * 60 + x$hdt[3,],
      time        = traceTime,
      proc        = character(0),
      vel         = list(v),
      name        = x_name,
      description = desc,
      filepath    = fPath,
      dz          =  x$hd$RANGE /  (x$hd$NSAMP - 1 ), 
      dx          = x_dx,
      depthunit   = x_depthunit,
      posunit     = x_posunit,
      freq        = antfreq, 
      antsep      = antsep,     # check
      surveymode  = "reflection",
      date        = as.character(dd), #format(Sys.time(), "%d/%m/%Y"),
      crs         = character(0),
      hd          = x$hd
  )
}

# readSGY <- function(dsn, fName = "", fPath = "", desc = "", 
#                     Vmax = 50, verbose = TRUE){
#   x <- suppressMessages( suppressWarnings(rgdal::readOGR(dsn = dsn)))
#   i <- grep("SAMPLE_ARRAY", names(x))
#   # xi <- x[i]
#   # 
#   # slotNames(xi)
#   # 
#   # dim(xi@data)
#   
#   x_data <- t(as.matrix(x[i]@data))
#   x_dz <- mean(x[["SAMPLE_INTERVAL"]]/1000)
#   
#   # plot3D::image2D(as.matrix(xi@data))
#   y <- list(version = "0.2",
#             data = x_data * byte2volt(Vmax = Vmax),
#             name = fName,
#             description = desc,
#             filepath = fPath,
#             surveymode = "reflection",
#             traces = seq_len(ncol(x_data)),
#             freq = 0,                      # FIXME
#             dx = 1,                        # FIXME
#             time0 = rep(0, ncol(x_data)),  # FIXME
#             # time = ,
#             fid = rep("", ncol(x_data)),   # FIXME
#             ann = rep("", ncol(x_data)),   # FIXME
#             dz = x_dz,
#             depth = seq(0, by = x_dz, length.out = nrow(x_data)),
#             pos = seq_len(ncol(x_data)),   # FIXME
#             antsep = 0,
#             posunit = "m",
#             depthunit = "ns"
#   )
#   message("Set trace position with either 'pos(x) < -...' or 'coord(x) < -...'")
#   message("Set antenna frequency with 'antfreq(x) < -...' ")
#   message("Set antenna separation distance with 'antsep(x) < -...' ")
#   as(y, "GPR")
#   
# }

.gprVOL <- function(x, fName = "", fPath = "", desc = "", Vmax = 50){
  if(is.null(x$hd$zmin) && !is.null(x$hd$dz)){
    x_depth <- seq(x$hd$zmin, by = as.numeric(x$hd$dz), length.out = x$hd$z_dim)
  }else{
    x_depth <- seq_len(x$hd$z_dim)
  }
  if(x$hd$dim == "2D"){
    y <- new("GPR",   
        version      = "0.2",
        data        = byte2volt(Vmax = Vmax, nBytes = x$hd$bits) * x$data,
        traces      = 1:ncol(x$data),
        fid         = rep("", ncol(x$data)),
        #coord = coord,
        coord       = matrix(nrow = 0, ncol = 0),
        pos         = 1:ncol(x$data),
        depth       = 1:nrow(x$data),
        rec         = matrix(nrow = 0, ncol = 0),
        trans       = matrix(nrow = 0, ncol = 0),
        time0       = rep(0, ncol(x$data)),
        # time = x$hdt[1,] * 3600 + x$hdt[2,] * 60 + x$hdt[3,],
        time        = rep(0, ncol(x$data)),
        proc        = character(0),
        vel         = list(),
        name        = fName,
        description = desc,
        filepath    = fPath,
        dz          =  1, 
        dx          = 1,
        depthunit   = "ns",
        posunit     = "m",
        freq        = 0, 
        antsep      = 0,     # check
        surveymode  = "reflection",
        date        = format(Sys.time(), "%d/%m/%Y"),
        crs         = character(0),
        hd          = x$hd
    )
  }
  if(x$hd$dim == "3D"){
   y <- new("GPRcube",
         version      = "0.2",
         name         = fName,
         date         = format(Sys.time(), "%d/%m/%Y"),  
         freq         = 0,
         filepaths    = fPath,
         x            = seq_len(x$hd$x_dim),
         y            = seq_len(x$hd$y_dim),
         data         = x$data * byte2volt(Vmax = Vmax, nBytes = x$hd$bits),
         coord        = numeric(0),
         posunit      = "m",
         crs          = character(0),
         depth        = x_depth,
         depthunit    = "ns",
         vel          = list(),               
         delineations = list(),
         obs          = list(),
         transf       = numeric()
    )
  }
  return(y)
}


.gprUtsi <- function(x, fName = fName, fPath = fPath, 
         desc = desc, Vmax = Vmax){
  
  y <- new("GPR", 
           version     = "0.2",
           data        = x[["data"]] * byte2volt(Vmax = Vmax, 
                                                 nBytes = x$hd$bits),
           traces      = 1:ncol(x[["data"]]),       # trace numbering
           pos         = 1:ncol(x[["data"]]),                # trace position
           depth       = 1:nrow(x[["data"]]),
           time0       = rep(0, ncol(x[["data"]])),  
           time        = rep(0, ncol(x[["data"]])), # time of trace records
           proc        =  character(0),       
           vel         = list(0.1),                 # m/ns
           name        = fName,
           description = desc,
           filepath    = fPath,
           fid         = trimStr(x[['fid']]),
           dz          = 1, 
           dx          = 1, 
           depthunit   = "ns",
           posunit     = "m",
           freq        = 0,
           antsep      = 0, 
           surveymode  = "reflection",
           date        = format(Sys.time(), "%Y-%m-%d"),
           crs         = character(0),
           hd          = list())
  return(y)
}

#' Read a GPR data file
#' 
#' Note: argument \code{fPath} is depreacted. Use \code{dsn} instead.
#' 
#' Supported file format
#' \itemize{
#'   \item Sensors & Software file format (*.dt1 , *.hd).
#'         \code{readGPR(dsn = 'xline.dt1')}
#'   \item MALA file format (*.rd3, *.rad).
#'         \code{readGPR(dsn = 'xline.rd3')}
#'   \item RadSys Zond GPR device (*.sgy). 
#'         \strong{Note: it is not the SEG-Y file format)}.
#'         \code{readGPR(dsn = 'xline.sgy')}  
#'   \item GSSI file format (*.dzt).
#'         \code{readGPR(dsn = 'xline.dzt')}
#'   \item ASCII file format (*.txt): either 4-column format 
#'         (x,t,amplitude) or matrix-format (without header/rownames).
#'         \code{readGPR(dsn = 'xline.txt')}  
#'   \item R object file format (*rds). These files are created by saving the
#'         \code{GPR} object with 
#'         \code{writeGPR(x, fPath = 'xline.rds', type = "rds")}.
#'         \code{readGPR(dsn = 'xline.txt')}  
#' }
#' @param dsn data source name: either the filepath to the GPR data (character),
#'            or an open file connection.
#' @param desc Short description of the file (character).
#' @param dsn2 data source name for additional file connection if \code{dsn} is
#'            an open file connection (e.g., open file connection to '*.hd'
#'            file if \code{ds} is an open file connection to a '*.dt1' file).
#' @param format lenth-one character vector required if the file extension is
#'               not appearent in the filepath or the connection (either
#'               \code{dt1}, \code{rad}, \code{dzt}, \code{sgy}, \code{iprb},
#'               \code{txt}, \code{rds})
#' @param Vmax length-one numeric vector: nominal analog input voltage used 
#'             for the byte to volt transformation. 
#'             It assumes that \code{Vmin = -Vmax}. If \code{Vmax = NULL},
#'             no bytes to Volt transformation is applied.
#' @param fPath Filepath (character). DEPRECATED. Use \code{dsn} instead.
#' @param ch For multi-frequency GSSI files (*.dzt), which channel is red.
#' @param verbose (boolean). If \code{FALSE}, all messages and warnings are
#'                suppressed (use with care).
#' @return The GPR data as object of the class RGPR.
#' @seealso \code{\link{writeGPR}}
#' @examples
#' \dontrun{
#' # argument dsn is a file path
#' x1 <- readGPR(dsn = "data/RD3/DAT_0052.rd3")
#' y1 <- readGPR("data/FILE____050.DZT")
#' 
#' # argument dsn is a connection
#' con <- file("data/RD3/DAT_0052.rd3", "rb")   # binary mode
#' con2 <- file("data/RD3/DAT_0052.rad", "rt")  # text mode
#' x2 <- readGPR(dsn = con, dsn2 = con2)
#' close(con)
#' close(con2)
#' 
#' con <- file(dsn = "data/FILE____050.DZT", "rb")
#' y1 <- readGPR(con)
#' close(con)
#' }
#' @name readGPR
#' @rdname readGPR
#' @export
readGPR <- function(dsn, desc = "", dsn2 = NULL, format = NULL, Vmax = 50,
                    fPath, ch = 1, verbose = TRUE){
  # @aliases readGPR-methods
  # setMethod("readGPR", "character", function(fPath, desc = "", ...){
  if(!missing(fPath)){
    if(missing(dsn)){
      dsn <- fPath
    }
    warning("Use argument 'dsn' instead of 'fPath' because ",
            "argument 'fPath' is deprecated.")
  }
  if( inherits(dsn, "connection") ){
    summaryCon <- summary.connection(dsn)
    fPath <- summaryCon$description
    if(!is.null(format)){
      ext <- format[1]
    }else{
      ext <- .fExt(summaryCon$description)
    }
    fName <- .fNameWExt(fPath)
  }else{
    fPath <- dsn
    ext <- .fExt(fPath)
    fName <- .fNameWExt(fPath)
  }
  # DT1
  if("DT1" == toupper(ext) || "HD" == toupper(ext)){
    # fName <- .fNameWExt(fPath)
    A <- verboseF( readDT1(dsn, dsn2), verbose = verbose)
    x <- verboseF( .gpr(A, fName = fName, fPath = fPath, 
                        desc = desc, Vmax = Vmax),  verbose = verbose)
  }else if("rds" == tolower(ext)){
    x <- verboseF( readRDS(dsn), verbose = verbose)
    if(class(x) == "GPR"){
      x@filepath <- fPath
    }else if(class(x) == "list"){
      versRGPR <- x[["version"]]
      if(versRGPR == "0.1"){
        for(i in seq_along(x[['delineations']])){
          x[['delineations']][[i]][, 5] <- -x[['delineations']][[i]][, 5]
        }
      }
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
    }
  }else if("RD3" == toupper(ext) || "RAD" == toupper(ext)){
    # fName <- .fNameWExt(fPath)
    A <- verboseF( readRD3(dsn, dsn2), verbose = verbose)
    x <- verboseF( .gprRD3(A, fName = fName, fPath = fPath, 
                           desc = desc, Vmax = Vmax), verbose = verbose)
  }else if("RD7" == toupper(ext) || "RAD" == toupper(ext)){
    # fName <- .fNameWExt(fPath)
    A <- verboseF( readRD7(dsn, dsn2), verbose = verbose)
    x <- verboseF( .gprRD3(A, fName = fName, fPath = fPath, desc = desc, 
                           nBytes = 32, Vmax = Vmax), verbose = verbose)
  }else if("SGY" == toupper(ext) || "SEGY" == toupper(ext)){
    if( !inherits(dsn, "connection") ){
      dsn <- file(dsn, "rb")
    }
    ENDIAN <- "big"
    THD <- readSGY_textual_file_header(dsn, ENDIAN = "big")
    test <- FALSE
    # if(all(validEnc(THD))){
      test <- any(verboseF(grepl("Prism", THD), verbose = FALSE)) & 
              any(verboseF(grepl("Radar Systems, Inc.", THD), 
                        verbose = FALSE))
    # }
    # read RadSys Zond System
    if( test ){
      A <- verboseF( readSEGY_RadSys_Zond_GPR(dsn), verbose = verbose)
      x <- verboseF( .gprSEGY(A, fName = fName, fPath = fPath, 
                              desc = desc, Vmax = Vmax), verbose = verbose)
    # read classical SEG-Y file
    }else{
      A <- verboseF(readSGY(dsn), verbose = verbose)
      x <- verboseF( .gprSGY(A, fName = fName, fPath = fPath, 
                             desc = desc, Vmax = Vmax), verbose = verbose)
      return(x)
    }
    # A <- tryCatch({verboseF(readSGY(dsn), 
    #                         verbose = verbose)},
    #               error = function(e){return(NULL)})
    # if(is.null(A)){
    #   # z <- readGPR(dsn)
    #   if(in)
    #   A <- verboseF( readSEGY_RadSys_Zond_GPR(dsn), verbose = verbose)
    #   x <- verboseF( .gprSEGY(A, fName = fName, fPath = fPath, 
    #                           desc = desc, Vmax = Vmax), verbose = verbose)
    # }else{
    #   x <- verboseF( .gprSGY(A, fName = fName, fPath = fPath, 
    #                          desc = desc, Vmax = Vmax), verbose = verbose)
    #   return(x)
    # }
  }else if("IPRB" == toupper(ext) || "IPRH" == toupper(ext)){
    # fName <- .fNameWExt(fPath)
    A <- verboseF( readImpulseRadar(dsn, dsn2), verbose = verbose)
    x <- verboseF( .gprImpulseRadar(A, fName = fName, fPath = fPath, 
                          desc = desc, Vmax = Vmax), verbose = verbose)
  }else if("DZT" == toupper(ext)){
    # fName <- .fNameWExt(fPath)
    A <- verboseF( readDZT(dsn), verbose = verbose)
    x <- verboseF( .gprDZT(A, fName = fName, fPath = fPath, 
                 desc = desc, Vmax = Vmax, ch = ch), verbose = verbose)
  }else if("VOL" == toupper(ext)){
    A <- verboseF( readVOL(dsn), verbose = verbose)
    x <- verboseF( .gprVOL(A, fName = fName, fPath = fPath, 
                           desc = desc, Vmax = Vmax), verbose = verbose)
    if(A$hd$dim == "3D"){
      warning("return a 'GPRcube' object with complex numbers.",
              " Current processing and plotting functions are likely to not ",
              "work on the returned object. Please contact me:\n",
              "emanuel.huber@alumni.ethz.ch")
    }else{
      warning("Still experimental. Don't hesitate to contact me:\n",
              "emanuel.huber@alumni.ethz.ch")
    }
    return(x)
  }else if("DAT" == toupper(ext) || "HDR" == toupper(ext)){
    A <- verboseF( readUtsi(dsn), verbose = verbose)
    x <- verboseF( .gprUtsi(A, fName = fName, fPath = fPath, 
                           desc = desc, Vmax = Vmax), verbose = verbose)
  }else if("TXT" == toupper(ext)){
    # fName <- .fNameWExt(fPath)
    A <- verboseF( readTXT(dsn), verbose = verbose)
    x <- verboseF( .gprTXT(A, fName = fName, fPath = fPath, 
                 desc = desc, Vmax = Vmax), verbose = verbose)
  }else{
    stop(paste0("File extension not recognised!\n",
                "Must be '.DT1', '.dzt', '.rd3', '.sgy', '.segy', '.rds'\n",
                "'.iprb', '.iprh', '.dat', or '.vol'."))
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
}



#================================= COERCION ===================================#

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
  myLines <- sp::Lines(list(myLine), ID = x@name)
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
  names(myPoints) <- c("x", "y", "z")
  sp::coordinates(myPoints) = ~x + y
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


#' Coercion to integer
#'
#' @name as.integer
#' @rdname GPRcoercion
#' @export
setMethod("as.integer", "GPR",  function(x, ...) as.integer(as.numeric(x@data)))


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
      version = "0.2",
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
      date = format(Sys.time(), "%Y-%m-%d"),
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
  if(any("data" == tolower(names(x))) && is.matrix(x[["data"]])){
    if(is.null(x[["pos"]]) && !is.null(x[["dx"]]) && is.numeric(x[["dx"]])){
      x[["pos"]] <- (1:ncol(x[["data"]]) -1) * x[["dx"]]
    }else if( is.null(x[["pos"]]) ){
      x[["pos"]] <- (1:ncol(x[["data"]]) -1)*0.25
    }else{
      if(length(x[["pos"]]) != ncol(x[["data"]])){
        stop("length(x[['pos']]) must be equal to ncol(x[['data']]")
      }
      x[["dx"]] <- mean(diff(x[["pos"]]))
    }
    if(is.null(x[["depth"]]) && !is.null(x[["dz"]]) && is.numeric(x[["dz"]])){
      x[["depth"]] <- (1:nrow(x[["data"]]) -1) * x[["dz"]]
    }else if( is.null(x[["depth"]]) ){
      x[["depth"]] <- (1:nrow(x[["data"]]) -1)
    }else{
      if(length(x[["depth"]]) != nrow(x[["data"]])){
        stop("length(x[['depth']]) must be equal to nrow(x[['data']]")
      }      
      x[["dz"]] <- mean(diff(x[["depth"]]))
    }
    if(!is.null(x[["vel"]]) && !is.list(x[["vel"]])){
      x[["vel"]] <- list(x[["vel"]])
    }
    myArg <- as.list(match.call(definition = sys.function(-2),
                                call = sys.call(-2),
                                expand.dots = FALSE )
    )
    d_name <- paste(eval(myArg[2]))
    y <- new("GPR", 
             version     = "0.2",
             data        = x[["data"]],
             traces      = 1:ncol(x[["data"]]),       # trace numbering
             pos         = x[["pos"]],                # trace position
             depth       = x[["depth"]],
             time0       = rep(0, ncol(x[["data"]])),  
             time        = rep(0, ncol(x[["data"]])), # time of trace records
             proc        =  character(0),       
             vel         = list(0.1),                 # m/ns
             name        = as.character(d_name),
             description = paste0("coercion of ", as.character(d_name), 
                                  " (",typeof(x), ") into GPR"),
             filepath    = character(0),
             dz          = x[["dz"]], 
             dx          = x[["dx"]], 
             depthunit   = "ns",
             posunit     = "m",
             freq        = 100,
             antsep      = 1, 
             surveymode  = "reflection",
             date        = format(Sys.time(), "%Y-%m-%d"),
             crs         = character(0),
             hd          = list())
    sNames <- slotNames(y)
    sNames <- sNames[ !(sNames %in% c("data", "pos", "depth", "dz", "dx"))]
    for(i in seq_along(sNames)){
      if(!is.null(x[[sNames[i]]])){
        slot(y, sNames[i], check = TRUE) <- x[[sNames[i]]]
      }
    }
    return(y)
  }else{
    stop("The list must have a 'data' index name")
  }
}    


#======================== MANIPULATING GPR OBJECTS ============================#

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
  f = "apply", 
  signature = "GPR", 
  definition = function(X, MARGIN, FUN, ...){
    x_apply <- apply(X@data, MARGIN, FUN,...)
    if(MARGIN == 1 && is.null(dim(x_apply)) && length(x_apply) == nrow(X)){
      X[, 1:ncol(x_apply)] <- x_apply
    }
    return(x_apply)
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

#' @export
setMethod(   
  # can make the sum of a list of object of class GPR
  f = "sum", 
  signature = "GPR", 
  definition = function(x, ..., na.rm = FALSE){
    dots <- list(...)
    if(length(dots) == 0){
      sum(as.matrix(x), na.rm = na.rm)
    }else{
      z <- lapply(dots, function(x){ sum(x@data, na.rm)})
      Reduce("+", z) + sum(as.matrix(x), na.rm = na.rm)
    }
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
    # proc(x) <- getArgs()
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




#=========================== GETTER / SETTER ==================================#

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
  definition=function(x, i, j, drop){
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
        if(length(x@antsep) > 1) x@antsep <- x@antsep[j]
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
  f = "coord",
  signature = "GPR",
  definition = function(x,value){
    value <- as.matrix(value)
    if(ncol(x@data) == nrow(value) && ncol(value) == 3){
      x@coord <- as.matrix(value)
      x <- trRmDuplicates(x, verbose = FALSE)
      x@proc <- c(x@proc, "coord<-//")
    }else{
      stop("Dimension should be ", nrow(value), "x", ncol(value), "!!")
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

#' Trace projection
#'
#' Project the trace coordinates give a coordinate reference system.
#' @param x Object of the class GPR
#' @param CRSobj object of class \link{CRS}, or of class \code{character} in
#'               which case it is converted to \link{CRS}.
#' @name trProject
#' @rdname trProject
#' @export
setMethod("trProject", "GPR", function(x, CRSobj){
  xsp <- as(x, "SpatialLines")
  xsptrsf <- sp::spTransform(xsp, CRSobj)
  x@coord[, 1:2] <- sp::coordinates(xsptrsf)[[1]][[1]]
  x@crs <- as.character(CRSobj)
  return(x)
})

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
  definition=function(x, value){
    
    value <- as.numeric(value)
    
    #------------------- check arguments
    msg <- checkArgInit()
    msg <- checkArg(value,     msg, "NUMERIC_LEN", c(1, ncol(x)))
    checkArgStop(msg)
    #-----------------------------------
    
    if(length(value) == 1) value <- rep(value, ncol(x))
    x@time0 <- value
    
    x@proc <- c(x@proc, "time0<-")
    return(x)
  }
)

#' Wrapper for \code{time0()<-}
#'
#' @name setTime0
#' @rdname time0
#' @export
setMethod("setTime0", "GPR", function(x, t0, track = TRUE){
  
  t0 <- as.numeric(t0)

  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(t0,     msg, "NUMERIC_LEN", c(1, ncol(x)))
  msg <- checkArg(track,   msg, "LOGICAL_LEN", 1)
  checkArgStop(msg)
  #-----------------------------------
  
  if(length(t0) == 1) t0 <- rep(t0, ncol(x))
  x@time0 <- t0
  
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
})




  
#' Depth/time of the GPR data
#' 
#' @name depth
#' @rdname depth
#' @export
setMethod("depth", "GPR", function(x){
  return(x@depth)
})

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
      dx <- mean(abs(diff(value)))
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
        if(any(diff(value) < 0 )){
          warning("The antenna separations do not increase!",
                  " Increasing values are expected.")
        }
        x@antsep <- value
        x <- trRmDuplicates(x)
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


#' Antenna frequency in MHz
#' 
#' Antenna frequency of the antennas in MHz
#' @name antfreq
#' @rdname antfreq
#' @export
setMethod("antfreq", "GPR", function(x){
  return(x@freq)
} 
)
#' @name antfreq<-
#' @rdname antfreq
#' @export
setReplaceMethod(
  f = "antfreq",
  signature = "GPR",
  definition = function(x, value){
  # not CMP, not WARR => common-offset
  if(length(value) > 1) warning("Only first element is used!")
  x@freq <- as.numeric(value[1])
  x@proc <- c(x@proc, "antfreq<-")
  return(x)
})

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
})


#' Fiducial markers of the GPR data
#' 
#' @name fid<-
#' @rdname fid
#' @export
setReplaceMethod(
  f = "fid",
  signature = "GPR",
  definition = function(x, value){
    value  <- as.character(value)
    x@fid  <- value
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
  f = "proc",
  signature = "GPR",
  definition = function(x,value){
    value <- as.character(value)
    x@proc <- c(x@proc, value)
    return(x)
  }
)

#=============================== TEST FUNCTION: isXXX() =======================#


#' Return TRUE if surveymode is CMP
#' 
#' Return TRUE if surveymode is CMP
#' @name isCMP
#' @rdname isCMP
#' @export
setMethod("isCMP", "GPR", function(x){
  (grepl("CMP", toupper(x@surveymode)) || 
     grepl("WARR", toupper(x@surveymode))) && 
    !grepl("CMPANALYSIS", toupper(x@surveymode))
})


#' Return TRUE if the data are a function of time
#' 
#' @name isTimeUnit
#' @rdname isTimeUnit
#' @export
setMethod("isTimeUnit", "GPR", function(x){
  grepl("[s]$", x@depthunit)
})

#' Return TRUE if the data are a function of length
#' 
#' @name isLengthUnit
#' @rdname isLengthUnit
#' @export
setMethod("isLengthUnit", "GPR", function(x){
  !isTimeUnit(x)
} 
)




#============================= PROCESSING FUNCTIONS ===========================#


#' Trace projection
#'
#' Project the trace coordinates give a coordinate reference system.
#' @param x Object of the class GPR
#' @param CRSobj object of class \link{CRS}, or of class \code{character} in
#'               which case it is converted to \link{CRS}.
#' @name trProject
#' @rdname trProject
#' @export
setMethod("trProject", "GPR", function(x, CRSobj){
  xsp <- as(x, "SpatialLines")
  xsptrsf <- sp::spTransform(xsp, CRSobj)
  x@coord[, 1:2] <- sp::coordinates(xsptrsf)[[1]][[1]]
  x@crs <- as.character(CRSobj)
  return(x)
})

#' Amplitude envelope
#' 
#' Estimate for each trace the amplitude envelope with the Hilbert 
#' transform (instataneous amplitude).
#' @param x An object of the class GPR.
#' @param npad Integer. Padding to avoid Gibbs effect at the begining and
#'             end of the data because of the Hilbert transform.
#' @param FUN DEPRECATED. A function to be applied on each row of the GPR data 
#'            to estimate the wave amplitude as a function of time/depth.
#' @name envelope
#' @rdname envelope
#' @export
setMethod("envelope", "GPR", function(x, npad = 100){
  #if(is.null(FUN)){
  xmax <- max(abs(x), na.rm = TRUE)
  xH <- apply(x, 2, HilbertTransf, npad = npad)
  x2 <- sqrt(x^2 + base::Re(xH)^2)
  test <- abs(x2@data) > xmax
  x2@data[test] <- abs(x@data[test])
  proc(x2) <- getArgs()
  return(x2)
} 
)

#' Amplitude (deprecated)
#' 
#' @name ampl
#' @rdname ampl
#' @export
setMethod("ampl", "GPR", function(x, npad = 100, FUN = NULL, ...){
  warning("Deprecated! Use 'envelope()' instead.")
  if(is.null(FUN)){
    xmax <- max(abs(x), na.rm = TRUE)
    xH <- apply(x, 2, HilbertTransf, npad = npad)
    x <- sqrt(x^2 + base::Re(xH)^2)
    x@data[abs(x@data) > xmax] <- xmax
  }else{
    #funName <- getFunName(FUN)
    x@data[] <- apply(x, 1, FUN, ...)
  }
  # proc(x) <- getArgs( addArgs = c('FUN' = funName))
  proc(x) <- getArgs()
  return(x)
} 
)




#' DEPRECATED - Plot the trace plotEnvelope
#' 
#' Plot the amplitude envelope of each trace. See \code{envelope}.
#' 
#' @param x An object of the class GPR.
#' @param npad Integer. Padding to avoid Gibbs effect at the begining and
#'             end of the data because of the Hilbert transform.
#' @param FUN DEPRECATED. A function to be applied on each row of the GPR data 
#'            to estimate the wave amplitude as a function of time/depth.
#' @param add A length-one boolean vector. If TRUE the amplitude is plotted
#'            on the previous plot. If FALSE (default) a new plot is created.
#' @param all A length-one boolean vector. If TRUE all trace envelope are 
#'            plotted, if FALSE only the mean.
#' @param plotLog A length-one boolean vector. If TRUE the logarithm of the 
#'              amplitude of every trace is ploted on the estimate amplitude.
#'              Default is FALSE.
#' @examples
#' data(frenkeLine00)
#' plotAmpl(frenkeLine00, FUN = median)
#' @name plotEnvelope
#' @rdname envelope
#' @export
setMethod("plotEnvelope", "GPR", function(x, npad = 100, FUN = mean, add = FALSE, 
                                          all = FALSE, plotLog = TRUE, ...){
  #   op <- par(no.readonly=TRUE)
  stop("Deprecated!\nUse 'trPlot(envelope(x))' or ",
       "'trPlot(traceStat(envelope(x)))' instead.")
} 
)

#' DEPRECATED - Plot the trace amplitude
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
setMethod("plotAmpl", "GPR", function(x, npad = 100, FUN = mean, add = FALSE, 
                                      all = FALSE, plotLog = TRUE, ...){
  #   op <- par(no.readonly=TRUE)
  warning("Deprecated!\nUse 'trPlot(envelope(x))' or ",
          "'trPlot(traceStat(envelope(x)))' instead.")
  FUN <- match.fun(FUN)
  trPlot(traceStat(envelope(x), FUN = FUN), ...)
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
#' proc(A)
#' @name processing
#' @rdname processing
#' @export
setMethod("processing", "GPR", function(x){
  stop("DEPRECATED! Use 'proc()' instead!")
  return(x@proc)
} 
)



#' Direct-Current shift removal 
#' 
#' The direct-current offset (DC-shift) is estimated and removed from every 
#' trace individually. 
#' For a given trace, the DC-shift is estimated by a user supplied function 
#' applied on few trace samples, normally the samples before time-zero
#' (e.g., the average of the samples before time-zero).  Then, the
#' DC-shift is substracted from the trace.
#' 
#' The direct-current offset (or DC-shift) is a constant bias over time
#' that slightly shifts the signal amplitude. The DC-shift is best 
#' observed on the trace samples recorded before the signal was emitted. 
#' 
#' Modified slots
#' \itemize{
#'   \item \code{data}: DC-shift removed (data dimensions unchanged).
#'   \item \code{proc}: updated with function name and arguments.
#' }
#' 
#' @param x [\code{GPR class}] An object of the class \code{GPR}.
#' @param u [\code{integer}] Index of the trace samples used to evaluate for
#'          every trace the DC-shift. If \code{u = NULL}, the function takes
#'          for each trace 90\% of the samples before time-zero (the number
#'          of samples can vary from trace to trace).  
#' @param FUN [\code{function}] A function to apply on the \code{u} trace 
#'            samples (default is \code{mean}; alternatively, \code{median} 
#'            could be of interest because it is more robust but slower to 
#'            compute).
#' @param ... [\code{ANY}] Further arguments to be passed to \code{FUN}. 
#'          
#' @return [\code{GPR class}] An object of the class \code{GPR}.
#' 
#' @examples 
#' data("frenkeLine00")
#' x <- frenkeLine00
#' x1 <- dcshift(x, u = 1:100, FUN = median)
#' plot(x - x1)
#' x2 <- dcshift(x)
#' plot(x - x2)
#' 
#' @name dcshift
#' @rdname dcshift
#' @export
setMethod("dcshift", "GPR", function(x, u = NULL, FUN = mean, ...,
                                     track = TRUE){
  
  if(!is.null(u)) u <- as.integer(u)
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(u,   msg, "INDEX_VECTOR_NULL_UPPER", nrow(x))
  msg <- checkArg(FUN, msg, "FUNCTION")
  checkArgStop(msg)
  #    - ----------------------------------
  
  if(is.null(u)){
    if(all(time0(x) > x@depth[1])){
      # Dt <- min(time0(x)) - x@depth[1]  # time before time-zero
      # u <- x@depth[1] + 0:round((Dt*0.9)/x@dz)
      
      # 90% samples before time0 (computed individually for each trace)
      # computation independent of the time axis
      spls <- sapply(time0(x), function(y, d, a= 0.9){floor(a * sum(d <= y))}, 
                      x@depth)
      xDepth <- matrix(seq_along(x@depth), byrow = TRUE, 
                       nrow = ncol(x), ncol = nrow(x))
      # test which samples can be used for the computation
      test <- t(xDepth <= spls)
      f <- function(i, x, y, FUN, ...){
        FUN(x[,i][y[,i]], ...)
      }
      OUT <- sapply(1:ncol(x), f, x@data, test, FUN, ...)
    }else{
      warning("You must define 'u' or reset time-zero.\n",
              "not enough samples before time-zero...")
      return(x)
    }
  }else{
    OUT <- apply(x[u, ], 2, FUN, ...)
  }
  x_shift <- matrix(OUT, nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
  x <-  x - x_shift
  
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
})

#----------------- FIRST-BREAK
#' Time of first wave break
#'
#' Pick the time corresponding to the first break of each trace in the GPR profile.
#' Return a vector containing the first break times.
#' 
#' @param x [\code{GPR class}] An object of the class \code{GPR}
#' @param method [\code{character(1)}] Method to be applied (either
#'              \code{coppens}, \code{threshold} or \code{MER}). 
#'              \code{"coppens"} corresponds to the modified Coppens method, 
#'              \code{"threshold"} to the threshold method, 
#'              and \code{"MER"} to the modified energy ratio method.
#' @param thr [\code{numeric(1)}] Threshold for the signal 
#'              amplitude (in \%) at which time zero is picked (only for the
#'              threshold method). \code{thr} ranges between 0 and 1.
#' @param w [\code{numeric(1)}] Length of the leading window in unit of time
#'          (only for the modified Coppens and modified energy ratio 
#'          methods). Recommended value: about one period of the first-arrival 
#'          waveform.
#' @param ns [\code{numeric(1)}] Length of the edge preserving smoothing 
#'           window in unit of time (only for the modified Coppens 
#'           method). Recommended value: between one and two signal periods.
#'           When \code{ns = NULL} the value of \code{ns} is set to 
#'           \code{1.5 * w}.
#' @param bet [\code{numeric(1)}] Stabilisation constant (only for the 
#'            modified Coppens method). Not critical. 
#'            When \code{bet = NULL} the value of \code{bet} is set to 
#'            20\% of the maximal signal amplitude.
#'            
#' @return [\code{numeric(n)}] The time of the first wave break for every
#'         traces in unit of time (\code{n = ncol(x) =} number of traces).
#'         
#' @seealso \code{\link{firstBreakToTime0}} to convert time of first wave break
#'          into time-zero; 
#'          \code{\link{time0}} and \code{\link{setTime0}} to set time-zero;
#'          \code{\link{estimateTime0}} to estimate first wave break, convert
#'          it to time-zero and set time zero (all in one step);
#'          \code{\link{time0Cor}} to shift the traces such that they start
#'          at time-zero.
#'          
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
#'
#' #' @examples 
#' data("frenkeLine00")
#' fb <- firstbreak(frenkeLine00, w = 10)
#' plot(seq_along(frenkeLine00), fb)
#' 
#' @name firstBreak
#' @rdname firstBreak
#' @export
setMethod("firstBreak", 
          "GPR",
          function(x, method = c("coppens","threshold",  "MER"), 
                   thr = 0.12, w = 11, ns = NULL, bet = NULL){
  #method <- match.arg(method, c("coppens", "threshold", "MER"))
  method <- method[1]
  
  # shorten the file -> computation only up to the max value
  nmax <- nrow(x)
  tst <- which(as.matrix(x) == max(x), arr.ind = TRUE)
  if(length(tst) > 0 ){
    nmax <- max(tst[,"row"])
  }
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(method, msg, "STRING_CHOICE", 
                  c("coppens", "threshold",  "MER"))
  msg <- checkArg(thr   , msg, "PERCENT1")
  # msg <- checkArg(w     , msg, "NUMERIC1_SPOS", round((nmax - 1) * x@dz/1.5))
  msg <- checkArg(w     , msg, "NUMERIC1_SPOS", max(x@depth))
  # msg <- checkArg(ns    , msg, "NUMERIC1_SPOS_NULL", round((nmax - 1) * x@dz))
  msg <- checkArg(ns    , msg, "NUMERIC1_SPOS_NULL", max(x@depth))
  msg <- checkArg(bet   , msg, "NUMERIC1_SPOS_NULL", Inf)
  checkArgStop(msg)
  #-----------------------------------

  
  if( (nmax + w) < nrow(x) )  nmax <- max(tst[,"row"]) + w
  
  if(method == "coppens"){
    xs <- x@data[1:nmax, ]^2
    
    ns <- if(is.null(ns)) round(1.5 * w) else ns
    w <- round(w / x@dz)
    if( (w %% 2) == 0 ) w <- w + 1
    
    ns <- round(ns / x@dz)
    if(ns > nmax) ns <- nmax - 1
    if( (ns %% 2) == 0 )  ns <- ns + 1 
    if(is.null(bet))      bet <- 0.2 * max(xs)
    
    # the vectorized version of "coppens" (though not really faster)
    #  fb <- .firstBreakModCoppens2(xs, w = w, ns = ns, bet = bet)
    # below: the not vectorised version of Coppens...
    fb <- apply(xs, 2, .firstBreakModCoppens, w = w, ns = ns, bet = bet)
    fb <- x@depth[fb] # fb * x@dz
  }else if(method == "threshold"){
    thres <- thr * max(x)
    fb <- apply(abs(x@data), 2, .firstBreakThres, thr = thres, x@depth)
  }else if(method == "MER"){
    w <- round(w / x@dz)
    fb <- .firstBreakMER(x@data[1:nmax, ], w)
    fb <- x@depth[fb]
  }
  if(any(is.na(fb))){
    warning("First break could not be picked for some traces. \n",
            "That's no luck, but good news is that you can try with another ",
            "method.\n", "This is probably because your traces have a ",
            "too low S/N ratio." )
  }
  return(fb)
} 
)

time0Estimation <- function(...){
  stop("DEPRECATED!\n",
       "Use 'estimateTime0()' instead.")
}

#' Estimate and set time-zero
#' 
#' \code{estimateTime0} estimates for each trace individually the first wave 
#' break, computes the corresponding time-zero knowing the propagation speed
#' of the electromagnetic wave through air and returns an object of the class
#' \code{GPR} with updated time-zero. It is possible to apply a function 
#' provided by the user (e.g., \code{FUN}) on time-zero (e.g., to set time-zero
#' equal to the average value of the time-zeros computed for every traces; in
#' this case, all traces would have the same time-zero).
#' 
#' This function is a wrapper for the following commands
#' \itemize{
#'   \item \code{tfb <- firstBreak(x, ...)}
#'   \item \code{t0 <- firstBreakToTime0(tfb, x)}
#'   \item \code{time0(x) <- t0} (if \code{FUN} is not \code{NULL}
#'          \code{time0(x) <- FUN(t0, ...)})
#' }
#' 
#' Modified slots
#' \itemize{
#'   \item \code{time0}: new estimated time-zero.
#'   \item \code{proc}: updated with function name and arguments.
#' }
#'
#' @param x [\code{GPR class}] An object of the class \code{GPR}
#' @param method [\code{character(1)}] Method to be applied (either
#'              \code{coppens}, \code{threshold} or \code{MER}). 
#'              \code{"coppens"} corresponds to the modified Coppens method, 
#'              \code{"threshold"} to the threshold method, 
#'              and \code{"MER"} to the modified energy ratio method.
#' @param thr [\code{numeric(1)}] Threshold for the signal 
#'              amplitude (in \%) at which time zero is picked (only for the
#'              threshold method). \code{thr} ranges between 0 and 1.
#' @param w [\code{numeric(1)}] Length of the leading window in unit of time
#'          (only for the modified Coppens and modified energy ratio 
#'          methods). Recommended value: about one period of the first-arrival 
#'          waveform.
#' @param ns [\code{numeric(1)}] Length of the edge preserving smoothing 
#'           window in unit of time (only for the modified Coppens 
#'           method). Recommended value: between one and two signal periods.
#'           When \code{ns = NULL} the value of \code{ns} is set to 
#'           \code{1.5 * w}.
#' @param bet [\code{numeric(1)}] Stabilisation constant (only for the 
#'            modified Coppens method). Not critical. 
#'            When \code{bet = NULL} the value of \code{bet} is set to 
#'            20\% of the maximal signal amplitude. 
#' @param c0     [\code{numeric(1)}] Propagation speed of the GPR wave 
#'               through air in unit of space per unit of time 
#'               (generally in m/ns).
#' @param FUN [\code{function}] A function to apply on the 
#'            estimated time-zero of every traces (e.g., \code{mean} or 
#'            \code{median} to get set a single time-zero value to the data).
#' @param ... [\code{ANY}] Further arguments to be passed to \code{FUN}.
#'  
#' @return [\code{GPR class}] An object of the class \code{GPR}.
#'          
#' @seealso \code{\link{firstBreak}} to estimate the first wave break;
#'          \code{\link{firstBreakToTime0}} to convert the first wave break
#'          into time zero.
#'          \code{\link{time0}} and \code{\link{setTime0}} to set time-zero;
#'          \code{\link{time0Cor}} to shift the traces such that they start
#'          at time-zero.
#'          
#' @examples 
#' data("frenkeLine00")
#' x <- frenkeLine00
#' x1 <- estimateTime0(x, w = 10)
#' time0(x1)
#' x2 <- estimateTime0(x, w = 10, FUN = mean)
#' time0(x2)
#' 
#' @name estimateTime0
#' @rdname estimateTime0
#' @export
setMethod("estimateTime0", "GPR", 
          function(x, method = c("coppens", "threshold", "MER"), 
                   thr = 0.12, w = 11, ns = NULL, bet = NULL, c0 = 0.299, 
                   FUN = NULL, ..., track = TRUE){
  
  method <- method[1]
  
  # shorten the file -> computed only for argument checking
  nmax <- nrow(x)
  tst <- which(as.matrix(x) == max(x), arr.ind = TRUE)
  if(length(tst) > 0 ){
    nmax <- max(tst[,"row"])
  }
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(method, msg, "STRING_CHOICE", 
                  c("coppens", "threshold",  "MER"))
  msg <- checkArg(thr   , msg, "PERCENT1")
  msg <- checkArg(w     , msg, "NUMERIC1_SPOS", round((nmax - 1) * x@dz/1.5))
  msg <- checkArg(ns    , msg, "NUMERIC1_SPOS_NULL", round((nmax - 1) * x@dz))
  msg <- checkArg(bet   , msg, "NUMERIC1_SPOS_NULL", Inf)
  msg <- checkArg(c0    , msg, "NUMERIC1_SPOS", Inf)
  msg <- checkArg(FUN   , msg, "FUNCTION_NULL")
  checkArgStop(msg)
  #-----------------------------------
  
  tfb <- firstBreak(x, method = method, thr = thr, w = w, 
                    ns = ns, bet = bet)
  t0 <- firstBreakToTime0(tfb, x, c0 = c0)
  
  if(!is.null(FUN)) t0 <- FUN(t0, ...)
  
  x <- setTime0(x, t0, track = FALSE)

  # x@proc <- x@proc[-length(x@proc)] # remove proc "time0()<-"
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
})

#--------------- DATA EDITING FUNCTIONS
#' Shift trace vertically
#'
#' Shift traces vertically by an amount of depth (time) units. New traces 
#' are interpolated.
#' 
#' Modified slots
#' \itemize{
#'   \item \code{data}: trace shifted. The number of rows of data may 
#'         be smaller if \code{crop = TRUE}.
#'   \item \code{proc}: updated with function name and arguments.
#' }
#'
#' @param x      [\code{GPR class}] An object of the class \code{GPR}
#' @param ts     [\code{numeric}] Amount of time (or depth, depending on the
#'               trace unit) to shift the traces. 
#'               \code{ts} is eiter a single value (all the traces are shifted by 
#'               the same amount \code{ts}) or a vector with \eqn{m} elements 
#'               (\eqn{m} is equal to the number of traces).
#' @param method [\code{character(1)}] Interpolation method to be applied
#'               (one of \code{pchip} \code{linear}, \code{nearest}, 
#'               \code{spline}, \code{cubic}, \code{none}, 
#'               see also \code{\link[signal]{interp1}}). 
#'                \code{"none"} means that the trace is shifted by the
#'               amount of trace samples the closest to \code{ts} without
#'               interpolation.
#' @param crop   [\code{logical(1)}] 
#'               If \code{TRUE} (default), remove the rows containing only 
#'               zero's (no data).
#'              
#' @return [\code{GPR class}] An object of the class GPR.
#' 
#' @seealso \code{\link{time0Cor}} to shift the traces such that they start
#'          at time-zero.
#' @name traceShift
#' @rdname traceShift
#' @export
setMethod("traceShift", 
          "GPR", 
          function(x, ts, method = c("pchip", "linear", "nearest", "spline", 
                                      "cubic", "none"), crop = TRUE, 
                   track = TRUE){
            
  # method <- match.arg(method, c("spline", "linear", "nearest", "pchip", 
                                # "cubic", "none"))
  method <- method[1]
  if(length(ts) == 1){
    ts <- rep(ts, ncol(x))
  }
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(ts,     msg, "NUMERIC_LEN", c(1, ncol(x)))
  msg <- checkArg(method, msg, "STRING_CHOICE", 
                 c("pchip", "linear", "nearest", "spline", "cubic", "none"))
  msg <- checkArg(crop,   msg, "LOGICAL_LEN", 1)
  checkArgStop(msg)
  #-----------------------------------
  
  if(any(ts != 0)){
    x <- .traceShift(x, ts = ts, method = method, crop = crop)
    if(isTRUE(track)) proc(x) <- getArgs()
  }else{
    warning("Nothing shifted because all 'ts' values are equal to zero!")
  }

  return(x)
})

# private function
.traceShift <- function(x, ts, method = c("pchip", "linear", "nearest", "spline", 
                                          "cubic", "none"), crop = TRUE){
  x@data <- .traceShiftMat(x@data, ts = ts, tt = x@depth, 
                           dz = x@dz, method = method)
  if(crop == TRUE){
    testCrop <- apply(abs(x@data), 1, sum)
    x <- x[!is.na(testCrop), ]
  }
  return(x)
}

#' Interpolate (vertically) trace at regular interval or given position
#' 
#' Interpolate every trace at regular interval or at given positions
#' 
#' Modified slots
#' \itemize{
#'   \item \code{data}: trace interpolated The number of rows of data may 
#'         be smaller if \code{crop = TRUE}.
#'   \item \code{dz}: new value depending on argument \code{z}.
#'   \item \code{depth}: adapted to code{z}.
#'   \item \code{proc}: updated with function name and arguments.
#' }
#'
#' @param x      [\code{GPR class}] An object of the class \code{GPR}
#' @param z      [\code{numeric}] Either an interval (e.g., time interval)
#'               to interpolate the traces at regular interval or a vector
#'               of \eqn{m} elements (\eqn{m} is equal to the number of traces)
#'               Amount of time (or depth, depending on the
#'               trace unit) to shift the traces. 
#'               \code{ts} is eiter a single value (all the traces are shifted by 
#'               the same amount \code{ts}) or a vector with \eqn{m} elements 
#'               (\eqn{m} is equal to the number of traces).
#' @param method [\code{character(1)}] Interpolation method to be applied
#'               (one of \code{pchip} \code{linear}, \code{nearest}, 
#'               \code{spline}, \code{cubic}, \code{none}, 
#'               see also \code{\link[signal]{interp1}}). 
#'                \code{"none"} means that the trace is shifted by the
#'               amount of trace samples the closest to \code{ts} without
#'               interpolation.
#' @param crop   [\code{logical(1)}] 
#'               If \code{TRUE} (default), remove the rows containing only 
#'               zero's (no data).
#'              
#' @return [\code{GPR class}] An object of the class GPR.
#' 
#' @name interpTrace
#' @rdname interpTrace
#' @export
setMethod("interpTrace", 
          "GPR", 
          function(x, z, method = c("pchip", "linear", "nearest", "spline", 
                                     "cubic"), crop = TRUE, track = TRUE){
            
  method <- method[1]
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(z,     msg, "NUMERIC")
  if(length(z) == 1) msg <- checkArg(z,     msg, "NUMERIC1_SPOS", Inf)
  msg <- checkArg(method, msg, "STRING_CHOICE", 
                  c("pchip", "linear", "nearest", "spline", "cubic"))
  msg <- checkArg(crop,   msg, "LOGICAL_LEN", 1)
  checkArgStop(msg)
  #-----------------------------------  
  extrap <- TRUE
  if(crop) extrap = FALSE
  
  x <- .traceInterpReg(x = x, z = z, method = method, extrap = extrap)
  
  if(crop == TRUE){
    testCrop <- apply(abs(x@data), 1, sum)
    x <- x[!is.na(testCrop), ]
  }
  
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
})


# interpolation at regular interval if x@dz is not unique!!
.traceInterpReg <- function(x, z, method = c("pchip", "linear", "nearest", 
                                             "spline", "cubic"), extrap = TRUE){
  method <- method[1]
  if(length(z) == 1){
    x@dz <- z
    zreg <- seq(from = min(x@depth), to = tail(x@depth, 1), by = x@dz)
  }else if(length(z) > 1){
    zreg <- z
    x@dz <- mean(diff(z))
  }
  funInterp <- function(x, z, zreg){
    signal::interp1(x = z, y = x, xi = zreg, 
                    method = method, extrap = extrap)
  }
  x@data <- apply(x@data, 2, funInterp, 
                  z = x@depth, zreg = zreg)
  x@depth <- zreg
  return(x)
}


#' Time zero correction
#'
#' \code{time0Cor} shift the traces vertically such that they start at
#' time zero (time zero of the data can be modified with the function).
#' New traces are interpolated.
#'
#' This function is a wrapper for the following commands
#' \itemize{
#'   \item \code{ts <- -t0 + keep} (or if \code{t0} is \code{NULL}, 
#'         \code{ts <- -time0(x) + keep})
#'   \item \code{x <- traceShift( x,  ts, method = method, crop = crop)}
#'   \item \code{time0(x) <- time0(x) + ts}
#' }
#' 
#' Modified slots
#' \itemize{
#'   \item \code{data}: trace shifted. The number of rows of data may 
#'         be smaller if \code{crop = TRUE}.
#'   \item \code{time0}: set to 0.
#'   \item \code{proc}: updated with function name and arguments.
#' }
#'  
#' @param x      [\code{GPR class}] An object of the class \code{GPR}
#' @param t0     [\code{DEPRECATED}] DEPRECATED - NO MORE USED.
#'               Instead, set time-zero with either 
#'               \code{time0(x) <- ...} or 
#'               \code{x <- setTime0(x, ...)}.
#' @param method [\code{character(1)}] Interpolation method to be applied
#'               (one of \code{pchip} \code{linear}, \code{nearest}, 
#'               \code{spline}, \code{cubic}, \code{none}, 
#'               see also \code{\link[signal]{interp1}}). 
#'                \code{"none"} means that the trace is shifted by the
#'               amount of trace samples the closest to \code{ts} without
#'               interpolation.
#' @param keep   [\code{DEPRECATED}] DEPRECATED - NO MORE USED.
#' @param crop   [\code{logical(1)}] 
#'               If \code{TRUE} (default), remove the rows containing only 
#'               zero's (no data).
#'               
#' @return [\code{GPR class}] An object of the class \code{GPR}
#' 
#' @examples
#' data(frenkeLine00)
#' tfb <- firstBreak(frenkeLine00)
#' t0 <- firstBreakToTime0(tfb, frenkeLine00, c0 = 0.299)
#' time0(frenkeLine00) <- t0
#' frenkeLine00_2 <- time0Cor(frenkeLine00, method = "pchip")
#' 
#' @seealso \code{\link{firstBreak}} to estimate the first wave break;
#'          \code{\link{firstBreakToTime0}} to convert the first wave break
#'          into time zero.
#'          \code{\link{time0}} and \code{\link{setTime0}} to set time-zero;
#'          \code{\link{traceShift}} to shift the traces

#' @name time0Cor
#' @rdname time0Cor
#' @export
setMethod("time0Cor", "GPR", function(x, t0 = NULL, 
                                      method = c("pchip", "linear", "nearest", 
                                                 "spline", "cubic", "none"), 
                                      crop = TRUE, keep = 0, track = TRUE){
  method <- method[1]
  if(!is.null(t0)){
    warning("'t0' is no more used. Set first time-zero with\n",
            "'time0(x) <- t0' or",
            "'x <- setTime0(x, t0)'.")
  }
  if(keep != 0){
    warning("'keep' is no more used.")
  }
  
  #------------------- check arguments
  msg <- checkArgInit()
  #msg <- checkArg(t0,     msg, "NUMERIC_LEN", c(1, ncol(x)))
  msg <- checkArg(method, msg, "STRING_CHOICE", 
                 c("pchip", "linear", "nearest", "spline", "cubic", "none"))
  msg <- checkArg(crop,   msg, "LOGICAL_LEN", 1)
  checkArgStop(msg)
  #-----------------------------------
  
  # ts <- -x@time0 + keep
  
  # if(is.null(t0)){
  #   ts <- -x@time0 + keep
  # }else{
  #   if(any(is.na(t0))){
  #     stop("Woops, time zero selected have NA values. \n",
  #          "This is not acceptable, time zero must have a numeric value in ",
  #          "order to be corrected. \n",
  #          "If this is because first break could not be picked, \n",
  #          "you might want to consider removing these traces from your ",
  #          "radargram.")
  #   }else{
  #     if(length(t0) == 1){
  #       t0 <- rep(t0, length(x@time0))
  #     }
  #     ts <- -t0 + keep
  #   }
  # }
  ts <- -x@time0 
  if(any(ts != 0)){
    x <- .traceShift(x, ts = ts, method = method, crop = crop)
    x@time0 <- x@time0 + ts
    if(isTRUE(track)) proc(x) <- getArgs()
    return(x)
  }else{
    warning("Nothing shifted because all 'ts' values are equal to zero!")
    return(x)
  }
  # if(crop == TRUE){
  #   testCrop <- apply(abs(x@data), 1, sum)
  #   x <- x[!is.na(testCrop), ]
  # }
  # if( any(ts != 0) ){
  #   x <- traceShift( x,  ts, method = method, crop = TRUE)
  #   x@time0 <- x@time0 + ts
  #   proc(x) <- getArgs()
  # }else{
  #   warning("Nothing shifted because all 't0' or 'time0(x)' values ",
  #           "as well as all 'keep' values are ",
  #           "equal to zero!")
  # }
  # proc(x) <- getArgs()
  # return(x)
})



#' Constant-offset correction (time) of the GPR data
#'
#' \code{timeCorOffset} applies a time correction to every traces 
#' to compensate the offset between transmitter 
#' and receiver antennae (it converts the trace time of the data acquired with
#' a bistatic antenna system into trace time data virtually acquiered with 
#' a monostatic system under the assumption of horizontally layered structure).
#' If all the traces have the same time-zero, this function does not change the 
#' trace but only the time (time scale). If the traces have different
#' time-zero, the traces are first aligned to have the same time-zero 
#' (spline interpolation)
#' 
#' Modified slots
#' \itemize{
#'   \item \code{data}: trace shifted to time-zeor. The number of rows of data 
#'         may be smaller.
#'   \item \code{time0}: set to 0.
#'   \item \code{depth}: adapted to a virtual monostatic system. 
#'         FIXME: not regularly spaced!
#'   \item \code{antsep}: set to 0.
#'   \item \code{proc}: updated with function name and arguments.
#' }
#' 
#' @param x  [\code{GPR class}] An object of the class \code{GPR}
#' @param t0 [\code{DEPRECATED}] DEPRECATED - NO MORE USED.
#'           Instead, set time-zero with either 
#'           \code{time0(x) <- ...} or 
#'           \code{x <- setTime0(x, ...)}.
#'               
#' @return [\code{GPR class}] An object of the class \code{GPR}
#'              
#' @seealso \code{\link{time0}} to set time zero and 
#'          \code{\link{firstBreakToTime0}} to convert the first wave break
#'          into time zero.
#' @name timeCorOffset
#' @rdname timeCorOffset
#' @export
setMethod("timeCorOffset", "GPR", function(x, t0 = NULL, track = TRUE){
  
  if(!is.null(t0)){
    warning("'t0' is no more used. Set first time-zero with\n",
            "'time0(x) <- t0' or",
            "'x <- setTime0(x, t0)'.")
  }
  
  #------------------- check if slots are empty
  msg <- checkSlotInit()
  msg <- checkSlotEmpty(x, "antsep", msg)
  msg <- checkSlotEmpty(x, "vel", msg)
  checkSlotStop(msg)
  #--------------------------------------------
  
  # if( isSlotEmpty(x, "antsep") ){
  #   msg <- c(msg, "@antsep: Antenna separation must be first defined ",
  #          "with `antsep(x)<-...`!")
  # }
  # if( isSlotEmpty(x, "vel") ){
  #   msg <- c(msg, "@vel: wave velocity must be first defined ",
  #        "with `vel(x)<-...`!")
  # }
  # if(length(msg) > 0 )

  # if(!is.null(t0)){
  #   time0(x) <- t0
  # }
  
  # x <- time0Cor(x, method = "spline")
  ts <- -x@time0 
  if(any(ts != 0)){
    x <- .traceShift(x, ts = ts, method = "spline", crop = TRUE)
    x@time0 <- x@time0 + ts
  }
  
  dz <- mean(diff(x@depth))
  # !!! pb if negative values in sqrt()!!!!
  tcor2 <- x@depth^2 - (x@antsep/x@vel[[1]])^2
  test <- tcor2 >= 0
  x <- x[test]
  x@depth <- sqrt(tcor2[test])
  # x@time0 is already = 0
  x@antsep <- 0
  
  x <- .traceInterpReg(x, z = dz)
  # x@proc <- x@proc[-length(x@proc)] # remove proc from traceShift()
  # x@proc <- c(x@proc, "timeCorOffset")
  if(isTRUE(track)) proc(x) <- getArgs()
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
#' every traces.
#' 
#' The low-frequency component is computed by different methods:
#' \itemize{
#'   \item \code{runmed} running median based on \code{\link[stats]{runmed}}
#'   \item \code{runmean} running mean based on \code{\link[stats]{filter}}
#'   \item \code{MAD} DEPRECATED - Median Absolute Deviation filter
#'   \item \code{Gaussian} Gaussian smoothing applied to the trace samples
#'         after time-zero based on \code{\link[mmand]{gaussianSmooth}}
#' }
#' 
#' Modified slots
#' \itemize{
#'   \item \code{data}: trace dewowed.
#'   \item \code{proc}: updated with function name and arguments.
#' }
#' 
#' @param x    [\code{GPR class}] An object of the class GPR.
#' @param type [\code{character(1)}] Dewow method,
#'             one of \code{runmed} (running median),
#'             \code{runmean} (running mean), 
#'             \code{MAD} (DEPRECATED Median Absolute Deviation), 
#'             \code{Gaussian} (Gaussian smoothing).
#' @param w    [\code{numeric(1)}] If \code{type} = \code{runmed}, 
#'             \code{MAD} or \code{runmean}, window length of the filter in
#'             trace unit;
#'             If \code{type} = \code{Gaussian}, standard deviation in trace
#'             unit.
#'             If \code{w = NULL}, \code{w} is estimated as five times the 
#'             wavelength corresponding to the maximum frequency of x 
#'             (estimated with \code{\link{spec}})
#'             
#' @return [\code{GPR class}] An object of the class GPR whose traces are dewowed.
#' @examples
#' data(frenkeLine00)
#' A <- dewow(frenkeLine00, type = "Gaussian")
#' A
#' @name dewow
#' @rdname dewow
#' @export
setMethod("dewow", "GPR", function(x, type = c("runmed", "runmean", 
                                               "mad", "gaussian"), 
                                   w = NULL, track = TRUE){
  # type <- match.arg(type, c("MAD", "Gaussian"))
  type <- tolower(type[1])
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(type, msg, "STRING_CHOICE", c("runmed", "runmean",
                                                "mad", "gaussian"))
  msg <- checkArg(w,    msg, "NUMERIC1_SPOS_NULL", Inf)
  checkArgStop(msg)
  #-----------------------------------
  
  if(is.null(w)){
    # argument initialization
    # pulse width in ns, (x@freq is in MHz)
    a <- RGPR::spec(x, plotSpec = FALSE, unwrapPhase = FALSE)
    freq <- a$freq[which.max(rowMeans(a$pow))]
    # pw <- 1/(x@freq * 10^6)/10^-9
    pw <- 1/(freq * 10^6)/10^-9
    w <- (5 * pw)/x@dz
  }else{
    w <- round(w / x@dz)
  }
  if(type == "mad"){  
    warning("Soon deprecated. Use instead:\n",
            "dewow(x, type = 'runmed', w = 2*w)")
    x@data <- x@data - .runmmmMat(x@data, 2*w, type = "runmed")
  }else if(type == "runmed"){
    x@data <- x@data - .runmmmMat(x@data, w, type = "runmed")
  }else if(type == "runmean"){
    x@data <- x@data - .runmmmMat(x@data, w, type = "runmean")
  }else if(type == "gaussian"){
    xdata <- x@data
    xDepth <- matrix(x@depth, byrow = FALSE, nrow = nrow(x), ncol = ncol(x))
    xTime0 <- matrix(x@time0, byrow = TRUE, nrow = nrow(x), ncol = ncol(x))
    test <- xDepth <= xTime0
    # before_t0 <- x@depth <= mean(x@time0)
    xdata[test] <- 0
    x@data[!test] <- x@data[!test] - mmand::gaussianSmooth(xdata, w)[!test]
  }
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x) 
})

#----------------- 1D-SCALING (GAIN)
#' Gain compensation
#' 
#' @name gain
#' @rdname gain
#' @export
setMethod("gain", "GPR", function(x, 
                                  type = c("power", "exp", "agc"), 
                                  ..., 
                                  track = TRUE){
  type <- match.arg(type, c("power", "exp", "agc"))
  x@data[is.na(x@data)] <-0
  if(type == "power"){
    # alpha, dts, t0 = NULL, te = NULL, tcst = NULL
    x@data <- .gainPower(x@data, dts = x@dz, ...)
  }else if(type == "exp"){
    # alpha, dts, t0 = NULL, te = NULL
    x@data <- .gainExp(x@data, dts = x@dz, ...)
  }else if(type == "agc"){
    x@data <- .gainAgc(x@data, dts = x@dz, ...)
  }
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
} 
)

#' Spreading and Exponential Compensation (SEC) gain
#' 
#' \code{gainSEC} Applies a combination of a power and exponential time gain to compensate for 
#' the signal attenuation through spherical spreading losses and  
#' exponential ohmic dissipation of energy with depth. Usually, the power in
#' the power gain is set to zero.
#' 
#' Spreading and Exponential Compensation (SEC) gain can be written as 
#' \eqn{\exp(a \cdot t) \cdot t^b}, where \eqn{t^b} is the power gain
#' (set \eqn{b = 1} to get a linear gain) and \eqn{\exp(a \cdot t)} is the
#' exponential gain.
#' 
#' Modified slots
#' \itemize{
#'   \item \code{data}: trace gained.
#'   \item \code{proc}: updated with function name and arguments.
#' }
#' 
#' @param x    [\code{GPR class}] An object of the class GPR.
#' @param a    [\code{numeric(1)}] Parameter of the exponential filter
#'             (\code{a} \eqn{\geq} 0).
#' @param b    [\code{numeric(1)}] Parameter of the power filter
#'             (\code{b} \eqn{\geq} 0). Usually, \code{b = 1}.
#' @param t0   [\code{numeric}] Start time of the gain filter
#'             (if \code{t0 = NULL}, \code{t0} is set equal to \code{time0(x)}).
#' @param tend [\code{numeric(1)}] End time of the gain filter (optional)
#' @param tcst [\code{numeric(1)}] Constant time: the gain before 
#'             \code{tcst} is set equal to the gain value at \code{tcst}.
#'             
#' @return [\code{GPR class}] An object of the class GPR.
#' 
#' @seealso \code{\link{gainAGC}}
#'                            
#' @name gainSEC
#' @rdname gainSEC
#' @export
setMethod("gainSEC", "GPR", function(x, a = 0.01, b = 1, 
                    t0   = NULL, 
                    tend = NULL, 
                    tcst = NULL,
                    track = TRUE){
  
  if(is.null(t0)) t0 <- x@time0
  if(length(t0) == 1) t0 <- rep(t0, ncol(x))
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(a,    msg, "NUMERIC1_POS", Inf)
  msg <- checkArg(b,    msg, "NUMERIC1_POS", Inf)
  msg <- checkArg(t0,   msg, "NUMERIC_LEN", c(1, ncol(x)))
  msg <- checkArg(tend, msg, "NUMERIC1_NULL", Inf)
  msg <- checkArg(tcst, msg, "NUMERIC1_NULL", Inf)

  checkArgStop(msg)
  #-----------------------------------
  
  G <- getGainSEC(x, 
                  a  = a, 
                  b  = b,
                  t0 = t0,
                  tend = tend,
                  tcst = tcst)
  x <- x*G
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
})

#' Spreading and Exponential Compensation (SEC) gain
#' 
#' \code{getGainSEC} returns the SEC gain as an object of the class \code{GPR}.
#'                   
#' @name getGainSEC
#' @rdname gainSEC
#' @export
setMethod("getGainSEC", "GPR", function(x, a = 0.01, b = 1, 
                                        t0   = NULL, 
                                        tend = NULL, 
                                        tcst = NULL,
                                        track = TRUE){
  
  if(is.null(t0)) t0 <- x@time0
  if(length(t0) == 1) t0 <- rep(t0, ncol(x))
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(a,    msg, "NUMERIC1_POS", Inf)
  msg <- checkArg(b,    msg, "NUMERIC1_POS", Inf)
  msg <- checkArg(t0,   msg, "NUMERIC_LEN", c(1, ncol(x)))
  msg <- checkArg(tend, msg, "NUMERIC1_NULL", Inf)
  msg <- checkArg(tcst, msg, "NUMERIC1_NULL", Inf)
  checkArgStop(msg)
  #-----------------------------------
  
  spls <- sapply(t0, function(y, d){floor(sum(d < y))}, 
                 x@depth)
  n <- nrow(x)
  g <- (0 + x@depth^b) * exp(a * x@depth)
  G <- sapply(spls, function(x, g, n){c(rep(1, x), g[1:(n-x)])}, g, n )
  
  if(!is.null(tend)){
    test_tend <- x@depth >= tend
    if(any(test_tend)) G[test_tend, ] <- G[which(test_tend)[1],]
  }
  if(!is.null(tcst)){
    test_tcst <- x@depth <= tcst
    if(any(test_tcst)) G[test_tcst, ] <- G[tail(which(test_tcst),1),]
  }
  # xG <- x*G
  # scaling
  h1 <- quantile(as.vector(abs(x*G)), 0.99, na.rm = TRUE)
  h2 <- quantile(as.vector(abs(x)), 0.99, na.rm = TRUE)
  x@data <- G / h1 * h2
  # xG <- xG / h1 * h2
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
})


#' Automatic Gain Control (AGC) gain
#' 
#' \code{gainAGC} applies an AGC (Automatic Gain Control) gain.
#' The trace signal is smoothed with a Gaussian filter. The smoothed trace
#' is substracted from the original trace, raised to power \code{p}, smoothed
#' by a Gaussian filter and raised to power \code{r} to obtain the gain.
# Subtract image from local mean, raise to power 'p' then apply Gaussian
# smoothing filter to obtain a local weighted sum. 
# Finally raise the result
# to power 'r' to obtain the 'gain'.  Typically p = 2 and r = 0.5 which will
# make gain equal to the local RMS.  The abs() function is used to allow
# for arbitrary 'p' and 'r'.
# Apply inverse gain to the difference between the image and the local
# mean to obtain the final AGC image.
#' 
#' Spreading and Exponential Compensation (SEC) gain can be written as 
#' \eqn{\exp(a \cdot t) \cdot t^b}, where \eqn{t^b} is the power gain
#' (set \eqn{b = 1} to get a linear gain) and \eqn{\exp(a \cdot t)} is the
#' exponential gain.
#' 
#' Modified slots
#' \itemize{
#'   \item \code{data}: trace gained.
#'   \item \code{proc}: updated with function name and arguments.
#' }
#' 
#' @param x    [\code{GPR class}] An object of the class GPR.
#' @param w    [\code{numeric(1)}] Standard deviation of the 
#'             Gaussian smoother (in trace unit).
#' @param p    [\code{numeric(1)}] Parameter of the power filter
#'             (\code{b} \eqn{\geq} 0). Usually, \code{b = 1}.
#' @param r   [\code{numeric}] Start time of the gain filter
#'             (if \code{t0 = NULL}, \code{t0} is set equal to \code{time0(x)}).
#'             
#' @return [\code{GPR class}] An object of the class GPR.
#' 
#' @seealso \code{\link{gainSEC}}
#' 
#' @name gainAGC
#' @rdname gainAGC
#' @export
setMethod("gainAGC", "GPR", function(x, w = 10, p = 2, r = 0.5, track = TRUE){
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(w,    msg, "NUMERIC1_SPOS", Inf)
  msg <- checkArg(p,    msg, "NUMERIC1_POS", Inf)
  msg <- checkArg(r,    msg, "NUMERIC1_POS", Inf)
  checkArgStop(msg)
  #-----------------------------------
  
  xG <- .gainAgc(x@data, x@dz, w = w, p = p, r = r)
  
  h1 <- quantile(as.vector(abs(xG)), 0.99, na.rm = TRUE)
  h2 <- quantile(as.vector(abs(x)), 0.99, na.rm = TRUE)
  x@data <- xG / h1 * h2
  
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
})




#----------------- 1D-FILTER
#' One dimensional filters
#' 
#' @name filter1D
#' @rdname filter1D
#' @export
setMethod("filter1D", 
          "GPR", 
          function(x, 
                   type = c("runmed", "runmean", "mad", "gaussian", "hampel"), 
                   w = NULL, track = TRUE){
  # type <- match.arg(type, c("MAD", "Gaussian"))
  type <- tolower(type[1])
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(type, msg, "STRING_CHOICE", c("runmed", "runmean",
                                                "mad", "gaussian", "hampel"))
  msg <- checkArg(w,    msg, "NUMERIC1_SPOS_NULL", Inf)
  checkArgStop(msg)
  #-----------------------------------
  
  if(is.null(w)) w <- 10 * x@dz
  w <- round(w / x@dz)
  
  if(type == "mad"){  
    warning("Soon deprecated. Use instead:\n",
            "filter1D(x, type = 'Hampel', w = ...)")
    # A <- x@data
    # if(length(dim(A)) < 2){
    #   A <- matrix(A,ncol=1,nrow=length(A))
    # }
    # X <- rbind(matrix(0,ncol=ncol(A),nrow=w), A, matrix(0,ncol=ncol(A),nrow=w))
    # n <- nrow(X)
    # Y <- X
    # for (i in (w + 1):(n - w)) {
    #   Y[i,] <-  apply( X[(i - w):(i + w),, drop=FALSE], 2, median)
    #   # x0 = 0.1  # argument initialization
    #   # S0 <- 1.4826 * apply( abs(X[(i - w):(i + w),,drop=FALSE] - Xmed),
    #   #  2, median)
    #   # test <- abs(X[i,] - Xmed) > x0 * S0
    #   # Y[i,test] <- Xmed[test]      
    # }
    # x@data <- A - Y[(w+1):(n-w),]
    x@data <- .runmmmMat(x@data, 2*w, type = "runmed")
  }else if(type == "runmed"){
    x@data <- .runmmmMat(x@data, w, type = "runmed")
  }else if(type == "runmean"){
    x@data <- .runmmmMat(x@data, w, type = "runmean")
  }else if(type == "gaussian"){
    # t0 <- round(mean(x@time0)/x@dz)
    # xdata <- x@data
    # xDepth <- matrix(x@depth, byrow = FALSE, nrow = nrow(x), ncol = ncol(x))
    # xTime0 <- matrix(x@time0, byrow = TRUE, nrow = nrow(x), ncol = ncol(x))
    # test <- xDepth <= xTime0
    # before_t0 <- x@depth <= mean(x@time0)
    # xdata[before_t0,] <- 0
    # # if(length(dim(A))<2){
    # #   A <- matrix(A, ncol = 1, nrow = length(A))
    # # }
    # x@data[!before_t0,] <- xdata[!before_t0,] - 
    #   mmand::gaussianSmooth(xdata, w)[!before_t0,]
    xdata <- x@data
    xDepth <- matrix(x@depth, byrow = FALSE, nrow = nrow(x), ncol = ncol(x))
    xTime0 <- matrix(x@time0, byrow = TRUE, nrow = nrow(x), ncol = ncol(x))
    test <- xDepth <= xTime0
    # before_t0 <- x@depth <= mean(x@time0)
    xdata[test] <- 0
    x@data[!test] <- x@data[!test] - mmand::gaussianSmooth(xdata, w)[!test]
  }else if(type == "hampel"){
    x@data <- .runmmmMat(x@data, w, type = "hampel")
  }
  if(isTRUE(track)) proc(x) <- getArgs()
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
setMethod("filter2D", "GPR", function(x, 
                                      type = c("median3x3", "adimpro"), 
                                      ...,
                                      track = TRUE){
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
  if(isTRUE(track)) proc(x) <- getArgs()
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
setMethod("clip", "GPR", function(x, Amax = NULL, Amin = NULL,
                                  track = TRUE){
  x@data <- .clip(x@data,Amax,Amin)
  if(isTRUE(track)) proc(x) <- getArgs()
  #   x@proc <- c(x@proc, proc)
  return(x)
} 
)
#' Gamma correction of the amplitude
#' 
#' @name gammaCorrection
#' @rdname gammaCorrection
#' @export
setMethod("gammaCorrection", "GPR", function(x, a = 1, b = 1,
                                             track = TRUE){
  x@data <- .gammaCorrection(x@data,a,b)
  if(isTRUE(track)) proc(x) <- getArgs()
  #   x@proc <- c(x@proc, proc)
  return(x)
} 
)

#' Trace scaling
#'
#' @name traceScaling
#' @rdname traceScaling
#' @export
setMethod("traceScaling", 
          "GPR", 
          function(x,type = c("stat", "min-max", "95", "eq", 
                              "sum", "rms", "mad", "invNormal"),
                   track = TRUE){
  x@data <- scaleCol(x@data, type = type)
  if(isTRUE(track)) proc(x) <- getArgs()
  #   x@proc <- c(x@proc, proc)
  return(x)
}
)

#' Trace statistics
#'
#' \code{traceStat} is a generic function used to produce results defined
#' by an user function. The user function is applied accross traces (horizontal) 
#' using a moving window. Note that if the moving window length is not defined, 
#' all traces are averaged into one single trace (the results is similar to
#' \code{apply(x, 1, FUN, ...)}.
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
#' f1 <- traceStat(f0)
#' plot(f1)
#' # substract the average trace
#' plot(f0 - f1)
#' 
#' f2 <- traceStat(f0, w = 20)
#' plot(f2)
#' plot(f0 - f2)
#' 
#' f3 <- traceStat(f0, w = 20, FUN = median)
#' plot(f3)
#' plot(f0 - f3)
#' @name traceStat
#' @rdname traceStat
#' @export
setMethod("traceStat", "GPR", function(x, w = NULL, FUN = mean, ...,
                                       track = TRUE){
  FUN <- match.fun(FUN)
  if(is.null(w)){
    xdata <- x@data
    x <- x[,1]
    x@data <- as.matrix(apply(xdata, 1, FUN, ...))
    x@time0 <- mean(x@time0)
    x@time <- mean(x@time)
    x@coord <- matrix(ncol = 0, nrow = 0)
    x@rec <- matrix(ncol = 0, nrow = 0)
    x@trans <- matrix(ncol = 0, nrow = 0)
  }else{
    x@data <- wapplyMat(x@data, width = w, by = 1, FUN = FUN, MARGIN = 1, ...)
  }
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
}
)

#' Trace average (DEPRECATED, use 'traceStat' instead)
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
setMethod("traceAverage", "GPR", function(x, w = NULL, FUN = mean, ...,
                                          track = TRUE){
  warning("Deprecated!\n Use 'traceStat()' instead. Check the help.")
  FUN <- match.fun(FUN)
  if(is.null(w)){
    xdata <- x@data
    x <- x[,1]
    x@data <- as.matrix(apply(xdata, 1, FUN, ...))
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
  if(isTRUE(track)) proc(x) <- getArgs()
  #   x@proc <- c(x@proc, proc)
  return(x)
}
)

#' Background matrix substraction
#'  
#' See  Rashed and Harbi (2014) Background matrix subtraction (BMS): 
#' A novel background removal algorithm for GPR data
#' doi: 10.1016/j.jappgeo.2014.04.022
#' Takes time to compute!!
#' @param x An object of the class GPR
#' @param width A length-one integer vector equal to the window length of the 
#'          average window (an odd number).
#' @param trim  A length-one numeric vector: the fraction (0 to 0.5) of 
#'              observations to be trimmed from each end of x before 
#'              the mean is computed. Values of trim outside that range 
#'              are taken as the nearest endpoint 
#'              (argument of the function \code{mean}).
#' @param s A length-one positiv numeric vector controling the strength
#'          of the weighting scheme
#' @param eps A length-one positiv numeric vector defining the minimum
#'            desired residual value. If \code{NULL}, \code{itmax} defines
#'            the iteration number.
#' @param itmax A length-one positiv integer vector defining the maximum
#'            iteration number. If \code{NULL}, \code{itmax} defines
#'            the iteration number.
#' @return An object of the class GPR with substracted background.
#' @name backgroundSub
#' @rdname backgroundSub
#' @export
setMethod("backgroundSub", "GPR", function(x, width = 21, trim = 0.2,
                                           s = 1, eps = 1, itmax = 5,
                                           track = TRUE){
  if(is.null(width)){
    stop("Set a value to 'width'")
  }
  if(width > ncol(x)){
    stop("'width' must be smaller than the column number of x") 
  }
  if(is.null(itmax) && is.null(eps)){
    stop("You cannot set both 'eps' and 'imax' equal to 'NULL'!")
  } 
  if(is.null(itmax)) itmax <- Inf
  if(is.null(eps)) eps <- 0
  
  if( (width %% 2) == 0){
    width <- width + 1
  }
  y0 <- as.matrix(x[, c(((width - 1)/2 + 1):2, 
                        seq_along(x), 
                        ncol(x) - 1:((width - 1)/2))])
  test <- c()
  i <- 0
  
  d <- eps + 1
  while(i < itmax || d <= eps){
    i <- i + 1
    y <- wapplyMat(y0, width = width, by = 1, FUN = .BMSfx, 
                   MARGIN = 1,  s = s, trim = trim)
    message("* ", appendLF = FALSE)
    d <- sum(((y - y0)^2), na.rm =TRUE)/prod(dim(y))
    test <- c(test, d)
    y0 <- y
  }
  message("Residuals: ", paste(round(test, 3), collapse = " "))
  x <- x - y0[, - c(1:((width-1)/2), 
                    (width-1)/2  + ncol(x) + 1:((width-1)/2))]
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
}
) 

.BMSfx <- function(x, s = 1, trim = 0.2){
  x_trimMean <- mean(x, trim = trim, na.rm = TRUE)
  x_hat <- x[sign(x) == sign(x_trimMean)]
  w <- 1/(sqrt((x_hat - x_trimMean)^2))^s 
  w_norm <- length(x_hat) * w/sum(w, na.rm = TRUE)
  return(mean(w_norm * x_hat, na.rm = TRUE))
}  

#----------------- FREQUENCY FILTERS
#' Frequency filter
#' 
#' The frequency filter alters the signal amplitude with respect to frequency.
#' \describe{
#'   \item{Low-pass filter}{low frequencies are passed, high frequencies are attenuated.}
#'   \item{High-pass filter}{high frequencies are passed, low frequencies are attenuated.}
#'   \item{Band-pass filter}{only frequencies in a frequency band are passed.}
#' }
#'
#' For the low- and high-pass filter, only one cut-off frequency can be defined
#' while the argument \code{L} will define the filter length of the Hamming
#' window (necessary to reduce ringing artifacts from the Gibbs phenomenon). 
#' If two values are passed to the cut-off frequency argument \code{f}, 
#' the value of \code{L} will be ignored.
#' Example for low-pass filter: \code{f = c(150, 200)}.
#' Example for high-pass filter: \code{f = c(10, 20)}
#' 
#' For the band-pass filter, only two cut-off frequency can be defined
#' while the argument \code{L} will define the filter length of the Hamming
#' window (necessary to reduce ringing artifacts from the Gibbs phenomenon). 
#' If four values (the two first corner frequencies followed by the two last
#' corner frequencies ) are passed to the cut-off frequency argument \code{f}, 
#' the value of \code{L} will be ignored. 
#' Example: \code{f = c(10, 20, 150, 200)}
#' 
#' See this free book: The Scientist and Engineer's Guide to Digital Signal 
#' Processing By Steven W. Smith, Ph.D.
#'
#' @param x An object of the class GPR
#' @param f numeric vector: cut-off frequencies. Cutoff frequency is the 
#'          frequency beyond which the filter will not pass signals.
#'           See Details.
#' @param type length-one character vector: type of frequency vector. \code{low}
#'             for low-pass filter, \code{high} for high-pass filter and 
#'             \code{bandpass} for bandpass filter.
#' @param L length-one numeric defining the filter length. See Details.
#' @param plotSpec boolean. If \code{TRUE} plot the frequency spectrum as well.
#' @name fFilter
#' @rdname fFilter
#' @export
setMethod("fFilter", "GPR", function(x, f = 100, type = 
                                     c('low','high','bandpass'),
                                     L = 257, 
                                     plotSpec = FALSE,
                                     track = TRUE){
  x@data <- .fFilter1D(x@data, f = f,  type = type, L = L, dT = x@dz, 
                       plotSpec = plotSpec)
  if(isTRUE(track)) proc(x) <- getArgs()
  #   x@proc <- c(x@proc, proc)
  return(x)
} 
)

#' Frequency-wavenumber filter
#'
#' @name fkFilter
#' @rdname fkFilter
#' @export
setMethod("fkFilter", "GPR", function(x, fk = NULL, L = c(5 , 5), npad = 1,
                                      track = TRUE){
  if(is.null(fk)) stop("fk argument has to be specified")
  # if polygon
  if(is.list(fk) && length(fk) == 2){
    areaunclosed <- t(do.call("rbind", fk))
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
    fk <- outer(fre, knutot, inPoly,
                vertx = areaunclosed[,2],
                verty = areaunclosed[,1])
  }else if(!is.matrix(fk)){
    stop("fk should be either of type 'list' or 'matrix'\n")
  }else if(is.matrix(fk)){
    cat("# FIXME! function to transform matrix into polygon\n")
  }
  x@data <- .FKFilter(x@data, fk = fk, L = L, npad = npad)
  if(isTRUE(track)) proc(x) <- getArgs()
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
#' data(frenkeLine00)
#' x <- frenkeLine00
#' x1 <- eigenFilter(x, eigenvalue = c(1,3))
#' plot(x)
#' plot(x1)
#'
#' @name eigenFilter
#' @rdname eigenFilter
#' @export
setMethod("eigenFilter", "GPR", function(x, eigenvalue = NA, center = TRUE, 
                                         scale = FALSE, track = TRUE){
  
  ev <- unique(eigenvalue)
  X <- scale(x@data, center = center, scale = scale)
  Xsvd <- svd(X)
  lambda <- Xsvd$d^2
  
  if(is.null(ev)){
    ev <- c(seq_len(ncol(X)))
  }else if(!any(is.na(ev))){
    if(max(ev) > ncol(X)){
      stop("The number of eigenvalues selected cannot exceed the number ",
           "of GPR traces.")
    }
    if(min(ev) < 0){
      stop("The eigenvalues number must be strictly positive.")
    }
  }
  
  if(any(is.na(ev))){
    plot(seq_along(lambda), lambda, type = "b", col = "blue", pch = 16,
         xlab = "Eigenvalue Index", ylab = "Eigenvalue", 
         main = paste0(length(lambda), " eigenvalues"))
    ev <- readline(paste0("What eigenvalues do you want to use to reconstruct ",
                          "the radargram ?"))
    if(grepl(":", ev)){
      ev <- as.integer(eval(parse(text = ev)))
    }else{
      ev <- as.integer(unlist(strsplit(ev, split = ",")))
    }
    if(!is.integer(ev) || is.na(ev)){
      stop("Select the eigenvalues by typing (for example)\n",
           "either '2:12' or '1,2,3,4' or '10'.")
    }
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
  if(isTRUE(track)) proc(x) <- getArgs()
  
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
setMethod("rotatePhase", "GPR", function(x, phi, track = TRUE){
  x@data <- apply(x@data, 2, phaseRotation, phi)
  if(isTRUE(track)) proc(x) <- getArgs()
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
setMethod("conv1D", "GPR", function(x, w, track = TRUE){
  # rotatePhase <- function(x,phi){
  x@data <- convolution(x@data, w)
  #     x@proc <- c(x@proc,"conv1D")
  if(isTRUE(track)) proc(x) <- getArgs()  #proc(x) <- "conv1D"
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
setMethod("conv2D", "GPR", function(x, w, track = TRUE){
  # rotatePhase <- function(x,phi){
  x@data <- convolution2D(x@data, w)
  # x@proc <- c(x@proc, "conv2D")
  if(isTRUE(track)) proc(x) <- getArgs()
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
                                    method=c("spiking", "wavelet", 
                                             "min-phase", "mixed-phase"),
                                    ...,
                                    track = TRUE){
  method <- match.arg(method, c("spiking", "wavelet", "min-phase",
                                "mixed-phase"))
  toReturn <- list()
  dots <- list(...)
  if(method == "spiking" || method == "mixed-phase"){
    if(length(dots) == 0){
      stop(paste0("spiking deconvolution requires the following arguments:",
                  "W, wtr, nf, mu\n"))
    }
    # deconvSpiking <- function(x,W,wtr,nf,mu){
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
      Wmin[,i] <- deconvolve(c(1,rep(0,nf-1)), Fmin[,i], mu=mu)
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
    if(length(dots) == 0 || (is.null(dots[["h"]]) || is.null(dots[["mu"]]))){
      stop(paste0("wavelet deconvolution requires the following arguments:",
                  "h and mu\n"))
    }
    x <- traceScaling(x, type="rms")
    x@data <- apply(x@data, 2, deconvolve, dots[["h"]], dots[["mu"]])      
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
  if(isTRUE(track)) proc(x) <- getArgs()
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
setMethod("show", "GPR", function(object){
  print.GPR(object)
})   

#' Add a GPR trace on a plot
#'
#' @method lines GPR 
#' @name lines
#' @rdname lines
#' @export
lines.GPR <- function(x, relTime0 = FALSE, ...){
  if(length(x@vel) > 0){  
    v <- x@vel[[1]]
  }else{
    v <- 0
  }
  if(any(dim(x) == 1)){
    z <- x@depth
    t0 <- x@time0
    if(isTRUE(relTime0)){
      z <- x@depth - x@time0
      t0 <- 0
    }
    # z <- seq(-x@time0, by = x@dz, length.out = length(x@data))
    #z <- x@depth - x@time0
    dots <- list(...)
    if(!is.null(dots[["log"]]) && dots[["log"]] == "y"){
      dots[["log"]] <- NULL
      x@data <- log(x@data)
    }
    dots[["x"]] <- z
    dots[["y"]] <- x@data
    invisible( do.call(lines, dots) )
    #lines(z, x@data,...)
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
points.GPR <- function(x, ...){
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

#' Plot all the traces in one 1D plot
#' 
#' @param x Object of the class GPR
#' @param ... Arguments to be passed to \code{plot}/\code{line}
#' @name trPlot
#' @rdname trPlot
#' @export
setMethod(
  f = "trPlot",
  signature = "GPR",
  definition = function(x, ...){
    if(ncol(x) == 1){
      plot(x, ...)
    }else{
      dots <- list(...)
      if(!is.null(dots[["log"]]) && dots[["log"]] == "y"){
        dots[["log"]] <- ""
        x <- log(x)
        if(is.null(dots[["ylab"]])){
          dots[["ylab"]] <- "log(amplitude envelope mV)"
        }
      }
      if(is.null(dots[["ylab"]])) dots[["ylab"]] <- "amplitude envelope (mV)"
      
      dotsLine <- list()
      dotsLine[["X"]] <- x
      dotsLine[["MARGIN"]] <- 2
      dotsLine[["FUN"]] <- lines
      dotsLine[["x"]] <- depth(x)
      
      dotsLine[["lty"]] <- dots[["lty"]]
      dotsLine[["lwd"]] <- dots[["lwd"]]
      dotsLine[["col"]] <- dots[["col"]]
      # be carefull (dots$type redefined below)
      dotsLine[["type"]] <- dots[["type"]]  
      dotsLine[["pch"]] <- dots[["pch"]]
      
      if(is.null(dots[["ylim"]])){
        dots[["ylim"]] <- range(x, na.rm = TRUE)
        if(dots[["ylim"]][1] > 0){
          dots[["ylim"]][1] <- 0
        }else{
          dots[["ylim"]] <- max(abs(dots[["ylim"]])) * c(-1, 1)
        }
      }
      # if(is.null(dots[["ylim"]])){
      #   dots[["ylim"]] <- range(depth(x))
      # }

      dots[["x"]] <- x[,1]
      dots[["y"]] <- NULL
      dots[["type"]] <- "n"
      invisible( do.call(plot, dots) )
      #plot(x[,1], type = "n", ylim = ylim, xlim = range(depth(x)), ...)
      
      # lty, line width, lwd, color, col and for type = "b", pch.
      invisible(do.call(apply, dotsLine))
    }
  }
)



#' Plot the GPR object.
#'
#' \code{plot}: If the GPR object consists of a single trace, wiggle plot 
#' is shown. For CMP, the position of the traces on the x-axis is defined
#' by the antenna separation (\code{antsep(x)}).
#' 
#' Additional arguments
#' \itemize{
#'   \item \code{type}: Possible values for 1D plot: see argument \code{type} in
#'             \code{\link[graphics]{plot}}.
#'              For 2D plot: \code{"raster"} (default) or \code{"wiggles"}.
#'   \item \code{col}: Color. Default 1D: "black". 2D: \code{palGPR(n = 101)}
#' }
#' 
#' @param x Object of class \code{GPR}
#' @param add logical. If \code{TRUE}, add to current plot
#' @param relTime0 logical. If \code{TRUE}, adjust vertical axis to time-zero.
#'                 If time-zero varies from trace to trace, the vertical axis
#'                 is adjusted to the mean time-zero. Apply first the function
#'                 \code{time0Cor()} to shift the traces to their time-zero.
#' @param addFid logical. Add fiducial marks
#' @param addAnn logical. Add GPR annotations (line intersections)
#' @param addTime0 logical. Add time-zero line
#' @param addDepth0 logical. Add depth-zero line
#' @param addAmpl0 logical. Add an horizontal line on 1D plot
#' @param addTopo logical. For 2D plot, add topography (if the data are sampled
#'                         in time unit, the data are migrated with a static
#'                         migration)
#' @param clip numeric. If length-one numeric vector, clip the amplitudes 
#'                      larger than \code{clip} and smaller than \code{-clip}.
#'                      If length-two numeric vector, clip the amplitudes
#'                      smaller than \code{clip[1]} and larger than 
#'                      \code{clip[2]}. Per default, values below the 
#'                      0.01-quantile and above the 0.99-quantile are clipped.
#'                      If \code{clip = FALSE} the data are not clipped.
#' @param ratio logical.
#' @param barscale logical. Add a colorbar scale
#' @param wsize length-one numeric. Size of the wiggles (default = \code{1}).
#' @param wside length-one numeric. If positive the right part of the wavelet
#'              is colored. If negative, the left part of the wavelet is
#'              colored.
#' @param pdfName length-one character. Name/path of the PDF to export 
#'                without extension
#' @param ... additional arguments passed to the plotting methods 
#'            \code{\link[graphics]{plot}} for 1D plot and 
#'            \code{\link[plot3D]{Image}} for 2D plot. See also  \code{details}.
#' @method plot GPR 
#' @name plot
#' @rdname plot
#' @export
#Default 1D: "black". 2D: \code{palGPR(n = 101)}
# ##' @param y \code{NULL,} not used
# options: type=c(raster,wiggles), addTopo, clip, normalize
plot.GPR <- function(x, 
                     #y = NULL, 
                     add = FALSE, 
                     relTime0 = FALSE,
                     note = NULL, 
                     addFid = TRUE,
                     addAnn = TRUE,
                     addTime0 = TRUE,
                     addDepth0 = TRUE,
                     addAmpl0 = TRUE,
                     addTopo = FALSE,
                     clip = NULL,
                     ratio = 1,
                     barscale = TRUE, 
                     wsize = 1,   # wiggles
                     wside = 1,   # wiggles
                     pdfName = NULL,
                     ...){
  # print(list(...))
  if(length(x@vel)>0){  
    v <- x@vel[[1]]
  }else{
    v <- 0
  }
  dots <- list(...)
  #------------------------ trace plot (1D) -----------------------------------#
  if(any(dim(x) == 1)){
    if(isTRUE(add)){
      lines(x, ...)
    }else{
      par(mar = c(5, 4, 3, 2) + 0.1, oma = c(0, 0, 3, 0), mgp = c(2, 0.5, 0))
      z <- x@depth
      t0 <- x@time0
      if(isTRUE(relTime0)){
        z <- x@depth - x@time0
        t0 <- 0
      }
      if(is.null(dots$xlab)){
        if(grepl("[m]$", x@depthunit)){
          dots$xlab <- paste0("depth (",x@depthunit,")")
        }else if(grepl("[s]$", x@depthunit)){
          dots$xlab <- paste0("two-way travel time (",x@depthunit,")")
        }
      }
      if(is.null(dots$type)) dots$type <- "l"
      if(is.null(dots$col)) dots$col <- "black"
      if(is.null(dots$ylab)) dots$ylab <- "amplitude (mV)"
      dotsxaxt <- dots$xaxt 
      if(is.null(dots$xaxt)) dots$xaxt <- "n"
      if(is.null(dots$main)){
        myMain <- paste0(x@name, ": trace #", x@traces," @", round(x@pos,2), 
                         x@posunit)
      }else{
        myMain <- dots$main
        dots$main <- NULL
      } 
      if(!is.null(dots[["log"]]) && dots[["log"]] == "y"){
        dots[["log"]] <- ""
        x@data <- log(x@data)
      }
      
      
      do.call(plot, c(list(x = z, y = x@data), dots))
      
      if(is.null(dots$ann) || dots$ann != FALSE){
        if(is.null(dotsxaxt) || dotsxaxt != "n"){
          x_axis <- pretty(z, 10)
          xat <- axis(side = 1,  tck = +0.02)
          if(grepl("[m]$", x@depthunit)){
            # axis(side = 3, at = x_axis, labels = x_axis, tck = +0.02)
            axis(side = 3, tck = +0.02)
            #FIXME: use fx .depthAxis()
          }else if(grepl("[s]$", x@depthunit)){
            if(length(x@antsep) > 0){
              if( x@antsep > 0){
                depth_0 <- t0 + depth0(0, v, antsep = x@antsep)
                depth2  <- seq(0.1, by = 0.1, 0.9)
                depthat0 <- depthToTime(0, 0, v, antsep = x@antsep)
                if(max(z)*v/2 > 1.3){
                  # depth <- pretty(seq(1.1, by = 0.1, max(z)*v/2), 10)
                  depth <- pretty(xat * v / 2, 10)
                  depthat <- depthToTime(depth, 0, v, antsep = x@antsep)
                  axis(side = 3, at = t0 + depthat, labels = depth, tck = +0.02)
                  #print(t0)
                }
                depthat2 <- depthToTime(depth2, 0, v, antsep = x@antsep)
                axis(side =3, at = t0 + depthat2, labels = FALSE, tck =+0.01)
                if(isTRUE(addDepth0)) abline(v = depth_0, col = "grey", lty = 3)
                mtext(paste0("depth (m),   v=", v, "m/ns"), side = 3, line = 2)
              }else{
                depth <- pretty(xat * v / 2, 10)
                depthat <- depthToTime(depth, 0, v, antsep = x@antsep)
                axis(side = 3, at = t0 + depthat, labels = depth, tck = +0.02)
              }
            }else{
              axis(side = 3, tck = +0.02)
            }
          }
        }
      }
      
      title(myMain, outer = TRUE)
      
      if(isTRUE(addAmpl0))  abline(h = 0, lty = 3, col = "grey")
      if(isTRUE(addTime0))  abline(v = t0, col = "red")
    }
    #------------------------ radargram plot (2D) -------------------------------#
  }else{
    if(grepl("[s]$", x@depthunit) && addTopo){
      x <- migration(x)
    }
    if(!is.null(clip) && is.numeric(clip)){
      if(length(clip) > 1){
        x@data <- .clip(x@data, clip[2], clip[1])
      }else if(length(clip) == 1){
        x@data <- .clip(x@data, clip[1])
      }
    }else if(is.null(clip)){
      # clip below the 0.01-quantile and above the 0.99-quantile
      x@data <- .clip(x@data, quantile(as.vector(x@data), 0.99, na.rm = TRUE),
                      quantile(as.vector(x@data), 0.01, na.rm = TRUE))
    }
    if(addFid == FALSE){
      x@fid <- character(length(x@fid))
    }
    if(is.null(note)) note <- x@filepath
    
    time_0 <- x@time0    
    t0 <- median(x@time0)
    z <- t( as.matrix(x@data) )
    
    xvalues <- x@pos
    yvalues <- x@depth
    myxlab <- paste0("length (", x@posunit, ")")
    myclab <- "mV"
    mymain <- x@name
    if(isCMP(x)){
      if(length(x@antsep) == ncol(x)){
        xvalues <- x@antsep
        myxlab <- paste0("antenna separation (", x@posunit, ")")
      }else{
        stop("length(antsep(x)) != ncol(x). You must correctly define ",
             "the antenna separation distance with 'antsep(x) <- ...'")
      }
    }else if(toupper(x@surveymode) == "CMPANALYSIS"){
      myclab <- ""
      myxlab <- paste0("velocity (", x@posunit, "/", x@depthunit, ")")
    }else if( length(x@coord) > 0 ){
      # xvalues <- posLine(x@coord)
      xvalues <- relTrPos(x)
    }
    if(grepl("[m]$",x@depthunit)){
      myylab <- paste0("depth (", x@depthunit, ")")
    }else if( grepl("[s]$", x@depthunit) ){
      myylab <- paste0("two-way travel time (", x@depthunit, ")")
    }
    if(is.null(dots$xlab)) dots$xlab <- myxlab
    if(is.null(dots$ylab)) dots$ylab <- myylab
    if(!is.null(dots$main)) mymain <- dots$main
    dots$main <- NULL
    if(is.null(dots$type)) dots$type <- "raster"
    dots$type <- match.arg(dots$type, c("raster","wiggles", "contour"))
    
    if(dots$type == "contour"){
      if(is.null(dots[["col"]]))      dots[["col"]] <- "black"
      if(length(dots[["col"]]) == 1)  barscale <- FALSE
      if(add == TRUE){
        addDepth0 <- FALSE
        addAmpl0 <- FALSE
        addTime0 <- FALSE
        addFid <- FALSE
        addAnn <- FALSE
        note <- ""
      }
    }
    
    
    # if(is.null(xlim)){
    #   xlim <- range(xvalues)
    # }
    if(isTRUE(relTime0)){
      yvalues <- yvalues - t0
      time_0 <- x@time0 - t0
      t0 <- 0
    }
    if(is.null(dots$ylim)) dots$ylim <- rev(range(yvalues))
    if(dots$ylim[1] < dots$ylim[2]) dots$ylim <- rev(dots$ylim)
    if(is.null(dots$xlim)) dots$xlim <- range(xvalues)
    
    op <- par(no.readonly=TRUE)
    
    colkeyDist <- 0.05
    if(barscale == FALSE){
      mai <- c(1.1, 1.02, 1.02, 1.02)
    }else{
      mai <- c(1.1, 1.02, 1.02, 1.4)
      if(grepl("[s]$", x@depthunit) && !isCMP(x) && 
         toupper(x@surveymode) != "CMPANALYSIS"){
        if(length(x@antsep) > 0 ){
          mai <- c(1.1, 1.02, 1.02, 2)
          colkeyDist <- 0.05  # 0.09
        }
      }
    }
    # print(colkeyDist)
    # omi <- c(0, 0, 0.6, 0)
    mai <- mai + c(0, 0, 0, 0)
    mgp <- c(2.5, 0.75, 0)
    fac <- 0.2
    omi <- par()$omi
    
    if(!is.null(pdfName)){
      # if the depthunit are "meters"
      xlim <- dots$xlim
      ylim <- dots$ylim
      if(grepl("[m]$", x@depthunit)){
        heightPDF <- fac * abs(diff(ylim)) + sum(omi[c(1,3)] + mai[c(1,3)])
        widthPDF <- fac * abs(diff(xlim)) * ratio +  
          sum(omi[c(2,4)] + mai[c(2,4)])
      }else{
        heightPDF <- fac * abs(diff(ylim)) * v/2 + 
          sum(omi[c(1,3)] + mai[c(1,3)])
        widthPDF <- fac * abs(diff(xlim)) * ratio + 
          sum(omi[c(2,4)] + mai[c(2,4)])
      }
      Cairo::CairoPDF(file = paste0(pdfName, ".pdf"),
                      width = widthPDF,
                      height = heightPDF,
                      bg = "white",
                      pointsize=10,
                      title = pdfName)
    }
    #------------------------------ RASTER ------------------------------------#
    if(dots$type %in% c("raster", "contour")){
      if(is.null(dots$clab)) dots$clab <- myclab
      if(!is.null(dots$rasterImage) && isTRUE(rasterImage)){
        dy <- diff(yvalues)
        dx <- diff(yvalues)
        test1 <- abs(max(dx) - min(dx)) > sqrt(.Machine$double.eps)
        test2 <- abs(max(dy) - min(dy)) > sqrt(.Machine$double.eps)
        # all not equal
        if(test1 && test2){
          dots$rasterImage <- FALSE
        }
      }
      
      if(is.null(dots$zlim)){
        if( min(z, na.rm = TRUE) >= 0 ){
          # to plot amplitudes for example...
          dots$zlim <- c(0, max(z, na.rm = TRUE))
          # if I use 'dots$col', R returns 'dots$colkey'!!!!
          if(is.null(dots[["col"]])) dots[["col"]] <- palGPR("slice")
        }else if(!is.null(x@surveymode) && 
                 tolower(x@surveymode) %in% c("cmp", "reflection")){
          dots$zlim <- c(-1, 1) * max(abs(z), na.rm = TRUE)
        }else{
          dots$zlim <- range(z[is.finite(z)], na.rm = TRUE)
        }
      }
      clim <- dots$zlim
      # if I use 'dots$col', R returns 'dots$colkey'!!!!
      if(is.null(dots[["col"]])) dots[["col"]] <- palGPR(n = 101)
      
      
      if(diff(range(z, na.rm = TRUE)) == 0){
        dots$zlim <- rep(0, 2)
        clim <- rep(z[1], 2)
        z[!is.na(z)] <- 0
      }
      
      if(is.null(dots$xaxs)) dots$xaxs <- "i"
      if(is.null(dots$yaxs)) dots$yaxs <- "i"
      if(is.null(dots$yaxt)) dots$yaxt <- "n"
      if(is.null(dots[["colkey"]])) dots[["colkey"]] <- FALSE
      
      if(add == TRUE){ 
        #par(new = TRUE)
        dots$add <- TRUE
        barscale <- FALSE
        dots[["colkey"]] <- FALSE
        dots[["main"]] <- ""
        addDepth0 <- FALSE
        addAmpl0 <- FALSE
        addTime0 <- FALSE
        addFid <- FALSE
        addAnn <- FALSE
        mymain <- ""
        note <- ""
        dots[["ann"]] <- FALSE
        dots[["axes"]] <- FALSE
        
      }else{
        # par( mai = mai, omi = omi, mgp = mgp)
        # par( mai = mai, mgp = mgp)
        par( mai = mai)
      }
      
      # print(clim)  
      # print(dots)  
      
      if(dots$type == "contour"){
        # if(is.null(dots[["colkey"]]) && isTRUE(dots[["add"]])){
        #   dots[["colkey"]] <- list(plot = FALSE)
        # } 
        #if(is.null(dots[["col"]])) dots[["col"]] <- "black"
        dots$type <- NULL
        do.call(plot3D::contour2D, c(list(x = xvalues, y = yvalues, z = z), 
                                     dots))
      }else if(dots$type == "raster"){
        #if(is.null(dots$bty)) dots$bty <- "n"
        dots$type <- NULL
        do.call(plot3D::image2D, c(list(x = xvalues, y = yvalues, z = z), dots))
      }
    #------------------------------ WIGGLES -----------------------------------#
    }else if(dots$type == "wiggles"){
      dots$type <- NULL
      barscale <- FALSE
      
      op <- par(no.readonly = TRUE) 
      dx <- mean(diff(xvalues)) # estimated x-step
      z <- x@data
      z[is.na(z)] = 0
      z <- z/max(abs(z)) * dx
      nr <- nrow(z)
      nc <- ncol(z)
      y0 <- 0
      topo <- rep(0L,nc)
      
      xlim <- dots$xlim + c(-1,1)*dx
      dots$xlim <- NULL
      
      ylim <- dots$ylim
      dots$ylim <- NULL
      
      if(is.null(dots$side)) side <- 1
      yaxt <- "s"
      bty <- "o"
      
      # col and lwd have no influence on plot
      col <- dots$col
      if(is.null(dots$col)) col <- "black"
      lwd  <- dots$lwd
      if(is.null(dots$lwd)) lwd <- 0.5
      
      par(mai = mai, omi = omi, mgp = mgp)
      
      do.call(plot, c( list( x = 0, type = "n", xaxs = "i", yaxs = "i", 
                             yaxt = "n", xlim = xlim, ylim = ylim, bty = "n"),
                       dots))
      
      if(wside > 0){
        for(i in rev(seq_along(xvalues))){
          y2 <- yvalues + topo[i]
          wig = cbind(wsize*z[,i] + xvalues[i], y2)
          wig1 = rbind(c(xvalues[i], y2[1]), wig, c(xvalues[i], tail(y2,1) ))
          polygon(wig1, col = col, border = NA)
          rect(min(wig1[,1]), ylim[1], xvalues[i], ylim[2], col = "white",
               border = NA)
          # lines(x[i]+wside*z[,i],y2,lwd=lwd)
        }
      }else{
        for(i in (seq_along(xvalues))){
          y2 <- yvalues + topo[i]
          wig = cbind(wsize*z[,i] + xvalues[i], y2)
          wig1 = rbind(c(xvalues[i], y2[1]), wig, c(xvalues[i], tail(y2,1) ))
          polygon(wig1, col = col, border = NA)
          rect(max(wig1[,1]), ylim[1], xvalues[i], ylim[2], col = "white", 
               border = NA)
        }
      }
      for(i in (seq_along(xvalues))){
        y2 <- yvalues + topo[i]
        lines(xvalues[i]+wsize*z[,i],y2,lwd=lwd)  
      }
      box(bty = bty)
    }
    #--------------------------------------------------------------------------#
    if(is.null(dots$ann) || dots$ann != FALSE){
      yat <- axis(side = 2)
      #xat <- axis(side = 1,  tck = +0.02)
      if(grepl("[m]$", x@depthunit) || grepl("CMP", toupper(x@surveymode))){
        axis(side = 4)
        #FIXME: use fx .depthAxis()
      }else if(grepl("[s]$", x@depthunit)){
        if(length(x@antsep) > 0){
          if(x@antsep > 0){
            depth_0 <- t0 + depth0(0, v, antsep = x@antsep)
            depth2  <- seq(0.1, by = 0.1, 0.9)
            depthat0 <- depthToTime(0, 0, v, antsep = x@antsep)
            if(max(yvalues) * v / 2 > 1.3){
              # depth <- pretty(seq(1.1, by = 0.1, max(z)*v/2), 10)
              depth <- pretty(yat * v / 2, 10)
              depthat <- depthToTime(depth, 0, v, antsep = x@antsep)
              axis(side = 4, at = t0 + depthat, labels = depth, tck = -0.02)
            }
            depthat2 <- depthToTime(depth2, 0, v, antsep = x@antsep)
            axis(side = 4, at = t0 + depthat2, labels = FALSE, tck = -0.01)
            axis(side = 4, at = depth_0, labels = "0", tick = FALSE)
            if(isTRUE(addDepth0)) abline(h = depth_0, col = "grey", lty = 3)
            # mtext(paste0("depth (m),   v=", v, "m/ns"), side = 4, line = 2)
            mtext(paste0("depth (", x@posunit, "),   v = ", round(v, 3), " ", x@posunit, 
                         "/",  x@depthunit), side = 4, line = 2.5)
          }else{
            depth_0 <- t0 + depth0(0, v, antsep = x@antsep)
            depth <- pretty(yat * v / 2, 10)
            depthat <- depthToTime(depth, 0, v, antsep = x@antsep)
            axis(side = 4, at = t0 + depthat, labels = depth, tck = -0.02)
            if(isTRUE(addDepth0)) abline(h = depth_0, col = "grey", lty = 3)
            mtext(paste0("depth (", x@posunit, "),   v = ", round(v, 3), " ", x@posunit, 
                         "/",  x@depthunit), side = 4, line = 2.5)
          }
        }else{
          axis(side = 4)
        }
      }
    }
    
    if(isTRUE(addTime0) && !grepl("CMPANALYSIS", toupper(x@surveymode))){
      dx <- diff(xvalues)/2
      xt0 <- c(xvalues[1] - dx[1],  xvalues + c(dx,  tail(dx, 1)))
      lines(xt0, c(time_0, tail(time_0, 1)), type = "s", col = "chartreuse",
            lwd = 2)
    }
    
    # plot note
    if(!is.null(note) && length(note) > 0){
      mtext(note, side = 1, line = 4, cex=0.6)
    }
    
    
    test <- ( xvalues >= dots$xlim[1] & xvalues <= dots$xlim[2] )
    # plot fiducial markers
    if(isTRUE(addFid) && !is.null(x@fid) && length(x@fid) > 0){
      .plotFid(x@fid[test], xvalues[test])
    }
    
    # plot annotations
    testAnn <- FALSE
    if(isTRUE(addAnn) && !is.null(x@ann) && length(x@ann) > 0){
      testAnn <- .plotAnn(x@ann[test], xvalues[test])
    }
    
    # plot title
    if(isTRUE(addAnn) && isTRUE(testAnn)){
      title(mymain, outer = TRUE, line = 1)
    }else{
      title(mymain)  
    }
    
    if(barscale){
      # op2 <- par(no.readonly=TRUE)
      # plot3D::colkey (col = dots$col, clim = clim, clab = dots$clab, clog = FALSE, 
      #                 add = TRUE, cex.clab = 0.75, dist = colkeyDist)
      fields::image.plot(zlim = clim, 
                         legend.only = TRUE, 
                         col = dots$col, 
                         legend.shrink = 1)
      # .barScale(clim = clim, y = yvalues, col = dots$col, 
      # clab = dots$clab, clabcex = 0.8)
      # par(op2)
    }
    # par(op)
    
    if(!is.null(pdfName)){
      dev.off()
    }
  }
}

#' \code{contour} extends \code{plot3D::contour2D} and creates a contour plot.
#' @method contour GPR 
#' @name contour
#' @rdname plot
#' @export
# options: type=c(raster,wiggles), addTopo, clip, normalize
contour.GPR <- function(x, 
                        relTime0 = FALSE,
                        add = FALSE, 
                        note = NULL, 
                        addFid = TRUE,
                        addAnn = TRUE,
                        addTime0 = TRUE,
                        addDepth0 = TRUE,
                        addAmpl0 = TRUE,
                        addTopo = FALSE,
                        clip = NULL,
                        ratio = 1,
                        barscale = TRUE, 
                        pdfName = NULL,
                        ...){
  
  plot.GPR(x, 
           add = add, 
           relTime0 = relTime0,
           note = note, 
           addFid = addFid,
           addAnn = addAnn,
           addTime0 = addTime0,
           addDepth0 = addDepth0,
           addAmpl0 = addAmpl0,
           addTopo = addTopo,
           clip = clip,
           ratio = ratio,
           barscale = barscale, 
           pdfName = pdfName,
           type = "contour",
           ...)
}



#' Three-dimensional plot of the GPR data with Open-GL
#'
#' @name plot3DRGL
#' @rdname plot3DRGL
#' @export
setMethod("plot3DRGL", "GPR", 
          function(x, addTopo = FALSE, clip = NULL, normalize = NULL, 
                   nupspl = NULL, add = TRUE, xlim = NULL, ylim = NULL, 
                   zlim = NULL, ...){
            if(length(x@vel) > 0){  
              velo <- x@vel[[1]]
            }else{
              velo <- 0
            }
            xsel <- rep(TRUE,length(x))
            if(!is.null(xlim)){
              xlim <- sort(xlim)
              xsel <- coord(x, 1) >= xlim[1] & coord(x, 1) <= xlim[2]
            }
            ysel <- rep(TRUE,length(x))
            if(!is.null(ylim)){
              ylim <- sort(ylim)
              ysel <- coord(x, 2) >= ylim[1] & coord(x, 2) <= ylim[2]
              cat(ylim,"  range = ", range(coord(x, 2)),"\n")
            }
            xysel <- xsel & ysel
            if(sum(xysel) <= 2){
              return(NULL)
            }
            x <- x[,xysel]
            if(!is.null(nupspl)){
              cat("upsample...")
              x <- upsample(x, n = nupspl)
            }
            if(!is.null(normalize)){
              x@data <- normalize(x@data, type = normalize)
            }
            # warning("First upsample then addTopo. 
            # Problem: interpolate also coord!!!")
            if(!is.null(clip) && is.numeric(clip)){
              if(length(clip)>1){
                x@data <- .clip(x@data, clip[2], clip[1])
              }else if(length(clip) == 1){
                x@data <- .clip(x@data, clip[1])
              }
            }else if(is.null(clip)){
              # clip below the 0.01-quantile and above the 0.99-quantile
              x@data <- .clip(x@data, quantile(as.vector(x@data), 0.99, na.rm = TRUE),
                              quantile(as.vector(x@data), 0.01, na.rm = TRUE))
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
              rgl::open3d()
            }
            .plot3DRGL(A, xpos, ypos, zpos, z0, ...)
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
#' @param verbose Logical. \code{TRUE}: a message will be thrown, 
#'                \code{FALSE}: no message will be thrown.
#' @name trRmDuplicates 
#' @rdname trRmDuplicates            
#' @export
setMethod("trRmDuplicates", "GPR", function(x, tol = NULL, verbose = TRUE){
  if(length(x@coord) == 0 ){
    warning("No trace coordinates!")
    return(x)
  }
  #dist2D <- posLine(x@coord[, 1:2], last = FALSE)
  dist2D <- relTrPos(x)
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
    # dist2D <- posLine(x@coord[, 1:2], last = FALSE)
    dist2D <- relTrPos(x)
    tdbl <- which(abs(diff(dist2D)) < tol)
  }
  if(verbose){
    message(check, " duplicated trace(s) removed from 'x'!")
  }
  #FIXME: use getArgs()!!!!
  #x@proc <- c(x@proc, "trRmDuplicates")
  proc(x) <- getArgs()
  return(x)
})

#' Interpolate trace positions from measurement (e.g., GPS).
#'
#' @param x      An object of the class GPR.
#' @param topo   A \eqn{m \times 4} numeric matrix, with \code{m} the number of 
#'               recorded trace positions.
#'               The first 3 columns are the 
#'               coordinates (x, y, z) and the last column the trace number 
#'               (the column names can now be freely chosen).
#' @param plot   A length-one boolean vector. If \code{TRUE} some 
#'               control/diagnostic plots are displayed.
#' @param r      A 'RasterLayer' object from the package 'raster' from which
#'               trace elevation \code{z} will be extracted based on the 
#'               trace position \code{(x, y)} on the raster. The extracted 
#'               trace elevation will overwrite the values from the third
#'               column of \code{topo}.
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
### handle crs -> if geogrphic -> projection
setMethod("interpPos", "GPR", 
          function(x, topo, plot = FALSE, r = NULL, tol = NULL,
                   method = c("linear", "linear", "linear"), crs = NULL,
                   ...){
            if(all(is.na(topo[,4]))){
              stop(x@name, ": no link between the measured points",
                          " and the GPR traces!")
            }
            if(ncol(topo) < 4){
              stop("'topo' has less than 4 columns!")
            }
            # if we have measured some points before the line start or 
            # after the end of the GPR Line, we delete them
            test <- which(!is.na(topo[, 4]))
            #trueEnd <- max(which(test))
            #trueBeg <- min(which(test))
            topo <- topo[min(test):max(test), ]
            #--- 3D topo Distance ---#
            if(!is.null(r)){
              topo[,3] <- raster::extract(r, topo[, 1:2], method = "bilinear")
              if(sum(is.na(topo[, 3])) > 0){
                stop("\n'extract(r, topo[, c(\"E\",\"N\")], method = \"bilinear\")' ",
                     "returns 'NA' values!\n", 
                     "Not all GPR positions fall within raster extent or\n",
                     "there are 'NA' values in raster 'r'!")
              }
            }
            # check for duplicates in TRACE ID and remove them!
            topo <- rmRowDuplicates(topo, topo[, 4])
            # order topo by increasing traceNb
            topo <- topo[order(topo[, 4]), ]
            #--- REMOVE DUPLICATED TRACES (TRACES WITH (ALMOST) SAME POSITIONS) ---#
            dist2D <- posLine(topo[, 1:2], last = FALSE)
            # in 'x' and 'topo'
            if(is.null(tol))  tol <- sqrt(.Machine$double.eps)
            tdbl <- which(abs(diff(dist2D)) < tol)
            #if(length(tdbl) > 0){
            while(length(tdbl) > 0){    # add
              check <- 0
              for(i in seq_along(tdbl)){
                dtr <- topo[tdbl[i] + 1, 4] - topo[tdbl[i], 4]
                # traces to remove
                w <- topo[tdbl[i], 4] + seq_len(dtr)
                x <- x[, -w]  # remove trace in x
                # remove traces in topo and actualise the trace numbers
                topo <- topo[-(tdbl[i] + 1), ] 
                v <- (tdbl[i] + 1):nrow(topo)
                topo[v, 4] <- topo[v, 4] -  dtr
                #dist3D <- dist3D[-tdbl[i]]
                tdbl <- tdbl - 1
                check <- check + dtr
              }
              message(length(tdbl), " duplicate trace position(s) removed from 'topo'!")
              message("Accordingly, ", check, " trace(s) removed from 'x'!")
              tdbl <- which(abs(diff(dist2D)) < tol)
            }
            dist3D <- posLine(topo[, 1:3], last = FALSE)
            # if there are points measured with the total station
            # that do not have an fiducial (FID) > interpolate them!
            myWarning <- ""
            if(anyNA(topo[,4])){
              # dist3D[topo$PNAME %in% FID$PNAME] > distance for the points 
              # also recorded in FID
              myWarning <- "\npoints total station without fiducials"
              test <- !is.na(topo[,4])
              intMeth <- ifelse(sum(test) > 2, method[1], "linear")
              traceNb <- signal::interp1(x      = dist3D[test], 
                                         y      = topo[test, 4], 
                                         xi     = dist3D,
                                         method = intMeth,
                                         extrap = TRUE)
              topo[, 4] <- as.integer(round(traceNb))
              # check for duplicates in TRACE ID and remove them!
              topo <- rmRowDuplicates(topo, topo[, 4])
            }
            if(all(seq_along(x@pos) %in% topo[, 4])){
              A <- topo[topo[, 4] %in% seq_along(x@pos), 1:3]
              message("No interpolation required because the trace positions\n",
                      "of all GPR traces is already available (in object 'topo')!")
            }else{
              #--- INTERPOLATION ---#
              A <- interp3DPath(x = topo[, 1:3], pos = topo[, 4], 
                                posi = seq_along(x@pos), r =r,
                                method = method)
              colnames(A) <- c("x", "y", "z")
            }
            # diagnostic plots
            dist3Dint <- posLine(A, last = FALSE)
            message(x@name, ": mean dx = ", round( mean(diff(dist3Dint)), 3 ), 
                    ", range dx = [", round( min(diff(dist3Dint)), 3 ),", ", 
                    round( max(diff(dist3Dint)), 3 ),"]", myWarning)
            if(plot == TRUE){
              op <- par(no.readonly=TRUE)
              par(mfrow=c(1, 3))
              plot(topo[, 4], dist3D, pch = 20, col = "red", cex = 2, asp = 1,
                   xlab = "trace number", ylab = "trace spacing (3D)",
                   xlim = range(seq_along(x@pos)), ylim = range(dist3Dint), 
                   main = paste( x@name))
              points(seq_along(x@pos), dist3Dint, pch = 20, col = "blue", cex = 0.6)
              plot(dist3Dint, A[, 3], type = "l", asp = 10, 
                   xlab = "interpolated trace spacing", 
                   ylab = "interpolated elevation",
                   main = paste0(x@name, " min dx = ", round(min(diff(dist3Dint)), 2), 
                                 "  max dx = ", round(max(diff(dist3Dint)), 2)))
              points(dist3D, topo[,3], pch = 20, col = "red")
              plot(topo[, c(1,2)], col = 1, type = "l", lwd = 2, asp = 1,
                   ylim = range(A[, 2]), xlim = range(A[, 1]), 
                   main = paste0(x@name, " mean dx=", round(mean(diff(dist3Dint)),2)))
              points(topo[, c(1,2)], col = 1, pch = 20, cex = 2)
              lines(A[, 1], A[, 2], col = 2, lwd = 1)
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

#' Interpolate GPR coordinates from geoJSON data
#'
#' @param x Object of the class GPR
#' @param geojson Either a geojson string or a filepath pointing to a 
#'                geojson file
#' @export
interpPosFromGeoJSON <- function(x, geojson, tol = NULL, backproject = TRUE){
  #---- 1. Read geoJSON
  xyz <- geojsonsf::geojson_sf(geojson)
  # sf::st_crs(xyz)
  
  #---- 2. convert to UTM
  XY <- sf::st_coordinates(xyz)
  XY <- XY[XY[, "L1"] == XY[1, "L1"], ]       # take the first structure
  u <- llToUTM(lat = XY[,2], lon = XY[,1])
  #plot(u$xy, type = "l", asp = 1)
  
  #---- 3. create "topo" file
  topo <- cbind(u$xy, 0, NA)
  colnames(topo) <- c("x", "y", "z", "tn")
  
  #---- 4. remove duplicates
  dist2D <- posLine(topo[, c("x", "y")], last = FALSE)
  # in 'x' and 'topo'
  if(is.null(tol))  tol <- sqrt(.Machine$double.eps)
  tdbl <- which(abs(diff(dist2D)) < tol)
  while(length(tdbl) > 0){
    topo <- topo[ -(tdbl + 1), ]
    dist2D <- posLine(topo[, c("x", "y", "z")], last = FALSE) # mod
    tdbl <- which(abs(diff(dist2D)) < tol)
  }
  
  #---- 5. trace interpolation
  # a) interpolate shape points to traces
  trFIDPos <- approx(x = c(0, tail(dist2D, 1)),
                     y = c(1, length(x)),
                     xout = dist2D)
  
  # b) interpolate coordinates
  tr_xyz <- matrix(0, nrow = ncol(x), ncol = 3)
  tx_x <- approx(x = trFIDPos$y,
                 y = topo[,"x"],
                 xout = seq_along(x))
  tx_y <- approx(x = trFIDPos$y,
                 y = topo[,"y"],
                 xout = seq_along(x))
  tx_z <- approx(x = trFIDPos$y,
                 y = topo[,"z"],
                 xout = seq_along(x))
  tr_xyz[,1] <- tx_x$y
  tr_xyz[,2] <- tx_y$y
  tr_xyz[,3] <- tx_z$y
  # plot(u$xy, type = "l", asp = 1)
  # lines(tx_x$y, tx_y$y, type = "p")
  
  if(backproject == TRUE){
    tr_xyz[,1:2] <- UTMToll(xy = tr_xyz[,1:2], xy_crs = u$crs)
  }
  
  # utr <- sf::st_as_sf(as.data.frame(uu), coords = c(1,2))
  # plot(sf::st_coordinates(xyz), asp = 1, type = "l")
  # points(sf::st_coordinates(utr), pch = 20)
  # plot(utr, add = TRUE, pch = 20)
  
  return(tr_xyz)
}

#' Relative trace position on the GPR profile.
#'
#' Trace position computed from (x, y)
#' @name relTrPos
#' @rdname relTrPos
#' @export
setMethod("relTrPos", "GPR", function(x, last = FALSE){
  return(posLine(x@coord[ ,1:2], last = last))
} 
)

#' Relative trace position acounting for elevation.
#'
#' Trace position computed from (x, y, z)
#' @name relTrPos3D
#' @rdname relTrPos
#' @export
setMethod("relTrPos3D", "GPR", function(x, last = FALSE){
  return(posLine(x@coord, last = last))
} 
)

#' @export
relPos <- function(x){
  stop("Use 'relPos()' instead!")
}

#' Reverse the trace position.
#'
#' @param x Object of the class \code{GPR} or \code{GPRsurvey}
#' @param id Only if \code{x} is an object of the class \code{GPRsurvey}. If 
#'           \code{id = NULL} and \code{x} has coordinates, \code{reverse()} 
#'           will cluster the GPR data according to their names (e.g., 
#'           cluster 1 = XLINE01, XLINE02, XLINE03; cluster 2 = YLINE01, 
#'           YLINE02; cluster 3 = XYLINE1, XYLINE2, XYLINE3) and reverse the
#'           data such that all GPR lines within the same cluster have the
#'           same orientation (up to a tolerance value \code{tol}).
#' @param to Length-one numeric vector. Tolerance angle in radian to determine
#'           if the data have the same orientation. The first data of the 
#'           cluster is set as reference angle \eqn{\alpha_0}, then for data 
#'           \eqn{i} in the same cluster, if \eqn{\alpha_i} is not between
#'           \eqn{\alpha_0 - \frac{tol}{2}} and 
#'           \eqn{\alpha_0 + \frac{tol}{2}}, then the data is reversed.
#' @name reverse
#' @rdname reverse
#' @examples 
#' \dontrun{
#' # SU class GPRsurvey
#' SU <- reverse(SU, id = "zigzag")
#' # identical to above
#' SU <- reverse(SU, id = seq(from = 2, to = length(SU), by = 2))
#' }
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

#' @name tpOBB2D
#' @rdname tpOBB2D
#' @export
setMethod("tpOBB2D", "GPR", function(x){
  if(length(x@coord) > 0){
    return(OBB(x@coord[,1:2]))
  }else{
    stop("x has no coordinates.")
  }
})

#' @name svAngle
#' @rdname svAngle
#' @export
setMethod("svAngle", "GPR", function(x){
  if(length(x@coord) > 0){
    dEN <- x@coord[1,1:2] - tail(x@coord[,1:2],1)
    angl_EN <- atan2(dEN[2], dEN[1])
    # angl_EN/pi * 180
    orb <- tpOBB2D(x)
    dEN <- orb[1,] - orb[2,]
    i <- which.max(diff(posLine(orb)))[1]
    dOBB <- orb[i + 1,] - orb[i,]
    angl_OBB <- atan2(dOBB[2], dOBB[1])
    # angl_OBB/pi * 180
    # abs(angl_EN - angl_OBB) / pi * 180
    if(pi * 6/5 > abs(angl_EN - angl_OBB) && abs(angl_EN - angl_OBB)  > pi* 4 /5){
      angl_OBB <- angl_OBB + pi
      if(angl_OBB > pi) angl_OBB <- angl_OBB - 2*pi
    }
    return(angl_OBB)
  }else{
    stop("x has no coordinates.")
  }
})

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
          function(x,name = NULL, type = c("raster", "wiggles"), 
                   addTopo = FALSE, nupspl = NULL, n = 10000, ...){
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
              # xvalues <- posLine(x@coord)
              xvalues <- relTrPos(x)
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
  #xvalues <- posLine(x@coord)      
  xvalues <- relTrPos(x)      
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
    # x_dist <- posLine(x@coord)
    x_dist <- relTrPos(x)
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
  #x_dist <- posLine(x@coord)
  x_dist <- relTrPos(x)
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
          function(x, sel = NULL, col = NULL, add = TRUE, ...){
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
                rgl::open3d()
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
                    rgl::lines3d(ypos, zpos, xpos, col = col[i], ...)
                  }
                }else{
                  tracePos <- sapply( deli[[i]][,1], .which, x@traces)
                  xpos <- x@coord[tracePos,1] - x@coordref[1]
                  ypos <- x@coord[tracePos,2] - x@coordref[2]
                  z0 <- max(coord(x, 3))   -   x@coordref[3]
                  zpos <- z0 + deli[[i]][,5] 
                  rgl::lines3d(ypos, zpos, xpos, col = col[i], ...)
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
    #x_dist <- posLine(x@coord)
    x_dist <- relTrPos(x)
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
    #x_dist <- posLine(x@coord)
    x_dist <- relTrPos(x)
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
    stop("survey mode of 'x' is not multi-offset. ",
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
  print(v)
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

winsemblance <- function(x){
  S <- sum(rowSums(x, na.rm = TRUE)^2) / 
    sum(rowSums(x^2, na.rm = TRUE)) * nrow(x)
  #S <- sum((apply(x, 1, sum, na.rm = TRUE))^2) /
  #  sum(apply((x)^2, 1, sum, na.rm = TRUE)) * nrow(x)
  return(S)
}

semblance <- function(x){
  S <- rowSums(x, na.rm = TRUE)^2 / rowSums(x^2, na.rm = TRUE)
  #x_velAna@data[,i] <- (apply(y@data, 1, sum, na.rm = TRUE))^2 / 
  #                    apply((y@data)^2, 1, sum, na.rm = TRUE)
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
#'   \item{winsemblance}{windowed semblance}       
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
  if(any(x@time0 != 0)){
    x <- time0Cor(x, method = "pchip")
  }
  #FIXME: create a new object > check slot consistency
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
    if(wi > nrow(x) || wi < 0 ) stop("w too large or too small")
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
                                        FUN = winsemblance)
        # x_velAna@data[floor(w/2) + seq_along(test),i] <- test
      }
    }
    x_velAna@data[vabove,] <- 0
  }else if(method == "semblance"){
    for(i in seq_along(v)){
      y <- .NMOCor(x, v = v[i], asep = x@antsep)
      x_velAna@data[,i] <- semblance(y@data)
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
#' Fresnel zone defined according to 
#' Perez-Gracia et al. (2008) Horizontal resolution in a non-destructive
#' shallow GPR survey: An experimental evaluation. NDT & E International,
#' 41(8): 611-620.
#' doi:10.1016/j.ndteint.2008.06.002
#'
#' @name migration
#' @rdname migration
#' @export
setMethod("migration", "GPR", function(x, type = c("static", "kirchhoff"), ...){
  if(length(x@antsep) == 0 || (!is.numeric(x@antsep))){
    stop("You must first define the antenna separation ",
         "with 'antsep(x) <- 1' for example!")
  }
  if(is.null(x@vel) || length(x@vel)==0){
    stop("You must first define the EM wave velocity ",
         "with 'vel(x) <- 0.1' for example!")
  }
  if(length(x@coord) != 0 && ncol(x@coord) == 3){
    topo <- x@coord[1:ncol(x@data), 3]
    # stop("You must first set coordinates to the traces ",
    #      "with 'coord(x) <- ...' or ",
    #      "'x <- interpPos(x, ...)' !")
  }else{
    topo <- rep.int(0L, ncol(x@data))
    message("Trace vertical position set to zero!")
  }
  type <- match.arg(type, c("static", "kirchhoff"))
  if(type == "static"){  
    if(any(x@time0 != 0)){
      x <- time0Cor(x, method = c("pchip"))
    }
    if(x@depthunit == "ns"){
      message("time to depth conversion with constant velocity (", x@vel[[1]],
              " ", x@posunit, "/", x@depthunit, ")")
      z <- timeToDepth(x@depth, time_0 = 0, v = vel(x), 
                       antsep = antsep(x))
      x <- x[!is.na(z),]
      x@dz <-  x@dz * x@vel[[1]]/ 2
      x@depth <- seq(from = 0, to = tail(z, 1), by = x@dz)
      funInterp <- function(x, z, zreg){
        signal::interp1(x = z, y = x, xi = zreg, 
                        method = "pchip", extrap = TRUE)
      }
      x@data <- apply(x@data, 2, funInterp, 
                      z = z[!is.na(z)], zreg = x@depth)
      x@depthunit <- "m"
    }else{
      # interpolation at regular interval if x@dz is not unique!!
      if(!length(unique(diff(x@depth))) ){
        x@dz <- min(abs(diff(x@depth)))
        zreg <- seq(from = min(x@depth), to = tail(x@depth, 1), by = x@dz)
        funInterp <- function(x, z, zreg){
          signal::interp1(x = z, y = x, xi = zreg, 
                          method = "pchip", extrap = TRUE)
        }
        x@data <- apply(x@data, 2, funInterp, 
                        z = x@depth, zreg = zreg)
      }
    }
    zShift <- (max(topo) - topo)
    #all(zShift != 0)
    x <- traceShift(x,  ts = zShift, method = c("pchip"), crop = FALSE)
    x@vel <- list() 
    if(length(x@coord) > 0 && ncol(x@coord) == 3 ){
      x@coord[, 3] <- max(x@coord[,3])
    }
  }else if(type == "kirchhoff"){
    A <- x@data
    #topo <- x@coord[,3]
    dx <- x@dx
    dts <- x@dz
    v <- x@vel[[1]]
    # initialisation
    #max_depth <- nrow(x)*x@dx
    max_depth <- max(x@depth) * v / 2 * 0.9
    dz <- 0.25 * x@dz
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
    x@data      <- .kirMig(x@data, topoGPR = topo, xpos = x@pos,
                           dts = x@dz, v = v, max_depth = max_depth, 
                           dz = dz, fdo = fdo, FUN = FUN)
    x@depth     <- seq(0,by=dz, length.out = nrow(x))
    x@time0     <- rep(0, ncol(x))
    x@dz        <- dz
    x@depthunit <- x@posunit          # check!!!
    if(length(x@coord) > 0 && ncol(x@coord) == 3 ){
      x@coord[,3] <- max(x@coord[,3])
    }
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
    n <- rep(n, 2)
  }
  x@data <- .upsample(x@data, n = n, type = c("DFT"))
  x@data <- x@data[, 1:(ncol(x@data))]
  yvalues <- (seq(0, by=x@dz,length.out=nrow(x@data)))
  
  xvalues  <- .doubleVector(x@pos, n = n[2])
  yvalues  <- .doubleVector(x@depth, n = n[1])
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
  x@depth <- .doubleVector(x@depth, n = n[1])
  
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
    #xpos <- posLine(x@coord)
    xpos <- relTrPos(x)
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
#' @param x Object of the class \code{GPR} or \code{GPRsurvey}
#' @param fPaht Filepath (Length-one character vector). If \code{fPath = NULL},
#'              the file will be save in the current working directory with
#'              the name of x (\code{name(x)}) with the extension depending 
#'              of \code{type}.
#' @param type Format type. See Details.
#' @param overwrite Boolean. If \code{TRUE} existing files will be overwritten,
#'                  if \code{FALSE} an error will be thrown if the file(s) 
#'                  already exist(s).
#' @name writeGPR
#' @rdname writeGPR
#' @seealso \code{\link{readGPR}}
#' @export
setMethod("writeGPR", "GPR", function(x, fPath = NULL, 
                                      type = c("DT1", "rds", "ASCII", "xta", "xyza"),
                                      overwrite = FALSE, ...){
  type <- match.arg(tolower(type), c("dt1", "rds", "ascii", "xta", "xyza"))
  fPath <- ifelse(is.null(fPath), x@name, 
                  file.path(dirname(fPath), .fNameWExt(fPath)))
  ext <- switch(type,
                "dt1" = ".dt1",
                "rds" = ".rds",
                "ascii" = ".txt",
                "xta" = ".txt",
                "xyza" = ".txt")
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
                                quote = FALSE, col.names = x@pos, 
                                row.names = x@depth,
                                ...)},
         "xyza" = {if(length(x@coord) == 0){
                     stop("This data has no coordinates!")
                   }
                   xyzv <- matrix(nrow=prod(dim(x)), ncol = 4)
                   colnames(xyzv) <- c("x", "y", "z", "a")
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
setMethod("exportFid", "GPR", function(x, fPath = NULL, sep = " "){
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
      write.table(x = FID, file = fPath, sep = sep, row.names = FALSE, 
                  col.names = TRUE, quote = FALSE)
    }
  }else{
    if(length(x@name)>0){
      message("No fiducials for ", x@name, "\n")
    }else{
      message("No fiducials\n")
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
              colnames(xCoord) <- c("x", "y", "z")
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

