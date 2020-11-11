# requires following functions:
# - '.getHD()'




#------------------------------------------------------------------------------#
# private function - constructor
# x = list containing all the data
# return object of class GPR

# Private function read DT1
# @export
.gprDT1 <- function(x, fName = character(0), desc = character(0),
                 fPath = character(0), Vmax = NULL){
  
  if(is.null(Vmax)) Vmax <- 50
  
  coord_rec   <- cbind(x$dt1$recx,   x$dt1$recy,   x$dt1$recz)
  coord_trans <- cbind(x$dt1$transx, x$dt1$transy, x$dt1$transz)
  if(sum(is.na(coord_rec)) > 0){
    warning(paste(fName,": ", sum(is.na(coord_rec)), 
                  "NA's in the receiver coordinates\n"))
  }
  if(sum(is.na(coord_trans)) > 0){
    warning(paste(fName,": ", sum(is.na(coord_trans)), 
                  "NA's in the transmitter coordinates\n"))
  }
  if(sum(is.na(x$dt1$topo)) > 0){
    warning(paste(fName,": ", sum(is.na(x$dt1$topo)), 
                  "NA's in the topo coordinates\n"))
  }
  if(sum(abs(coord_rec), na.rm = TRUE) == 0 ){
    coord_rec <- matrix(nrow = 0, ncol = 0) 
  }
  if(sum(abs(coord_trans), na.rm = TRUE) == 0){
    coord_trans <- matrix(nrow = 0, ncol = 0) 
  }
  if(sum(abs(x$dt1$topo), na.rm = TRUE) == 0){
    coord <- matrix(nrow = 0, ncol = 0) 
  }else{
    coord <- matrix(0, nrow = ncol(x$data), ncol = 3)
    coord[,3] <- x$dt1$topo
    # coord[,1] <- x$dt1$pos  #FIXME
  }
  #------------ HEADER DATA (FILE *.HD)
  ttw  <- .getHD(x$hd, "TOTAL TIME WINDOW")
  if(!is.null(ttw)){
    dz <- as.numeric(ttw) / nrow(x$data)
  }else{
    warning("time/depth resolution unknown! I take dz = 0.4 ns!\n")
    dz <- 0.4
    ttw  <- nrow(x$data) * dz
  }
  
  tzap <- .getHD(x$hd, "TIMEZERO AT POINT")
  if( sum(abs(x$dt1$time0)) == 0 ){
    if(!is.null(tzap)){
      time_0 <- rep(as.numeric(tzap)*dz - dz, ncol(x$data))
    }
  }else{
    time_0 <- x$dt1$time0
  }
  
  posunit <- .getHD(x$hd, "POSITION UNITS")
  if(is.null(posunit)){
    posunit <- "m"
    #FIXME -> show how to set the unit
    message("X unit (trace position) set to 'm' (meter).")
  }
  
  antfreq <- .getHD(x$hd, "NOMINAL FREQUENCY")
  if(!is.null(antfreq)){
    antfreq <- freqFromString(antfreq)
  }else{
    antfreq <- NA_real_
    # message("Antenna frequency set to 0 MHz. Set it with 'antfreq(x) <- ... '")
  }
  
  antsep <- .getHD(x$hd, "ANTENNA SEPARATION")
  if(is.null(antsep)){
    antsep <- NA_real_
    # message("Antenna separation set to 0 ", posunit, 
    #         ". Set it with 'antsep(x) <- ... '")
  }else{
    antsep <- as.numeric(antsep)
  }
  
  surveymode <- .getHD(x$hd, "SURVEY MODE")
  if(is.null(surveymode)){
    surveymode <- "CO"
  }else{
    if(grepl("reflection", surveymode, ignore.case = TRUE)){
      surveymode <- "CO"
    }else if(grepl("CMP", surveymode, ignore.case = TRUE)){
      surveymode <- "CMP"
    }
  }
  
  # nop  <- .getHD(x$hd, "NUMBER OF PTS/TRC")
  # if(is.null(nop)){
  #   pos_used[nop[2]] <- 1L
  # }
  # not <- .getHD(x$hd, "NUMBER OF TRACES", position = TRUE)
  # if(!is.null(not)){
  #   pos_used[not[2]] <- 1L
  # }
  sup_hd <- list()
  # startpos <- .getHD(x$hd, "STARTING POSITION", position = TRUE)
  # if(!is.null(startpos)){
  #   pos_used[startpos[2]] <- 1L
  #   sup_hd[["startpos"]] <- as.numeric(startpos[1])
  # }else{
  #   startpos <- 0
  # }
  # endpos <- .getHD(x$hd, "FINAL POSITION", position = TRUE)
  # if(!is.null(endpos)){
  #   pos_used[endpos[2]] <- 1L
  #   sup_hd[["startpos"]] <- as.numeric(endpos[1])
  # }else{
  #   endpos <- dx[1]*ncol(x$data)
  # }
  #--- survey date
  allhd <- gsub(pattern ="[^0-9]", replacement = "-", x$hd$val)
  # 2014-04-10 (new PulseEkko format)
  yyyymmdd <- "^([0-9]{4})([^0-9])([0-9]{2})([^0-9])([0-9]{2})"
  # 10/06/2011 (old PulseEkko format)
  ddmmyyyy <- "^([0-9]{2})([^0-9])([0-9]{2})([^0-9])([0-9]{4})"
  idx <- grep(yyyymmdd, allhd)
  d <- warnings("Could not understand the survey date\n")
  d <- Sys.Date()
  if(length(idx) > 0){
    d <- as.Date(allhd[idx[1]], "%Y-%m-%d")
  }else{
    idx <- grep(ddmmyyyy, allhd)
    if(length(idx) > 0){
      d <- as.Date(allhd[idx[1]], "%d-%m-%Y")
    }
  }
  # #--- device type
  # freepos <- which(pos_used[1:5] == 0)
  # #   GPR_device <-  paste(x$hd[2,1],x$hd[2,2],sep="")
  # GPR_device <-  x$hd[freepos, 2]
  # testDevice <- grepl("^(Data.)", GPR_device)
  # if(any(testDevice)){
  #   sup_hd[["gprdevice"]] <- GPR_device[testDevice][1]
  #   pos_used[which(testDevice)[1]] <- 1L
  # }  
  # x$hd2 <- x$hd[!pos_used,]
  # if(nrow(x$hd2)>0){
  #   key <-  trimStr(x$hd2[,1])
  #   test <- key!=""
  #   key <- key[test]
  #   key2 <- gsub("[[:punct:]]", replacement = "", key)
  #   key2 <- gsub(" ", replacement = "_", key2)
  #   nameL <- trimStr(x$hd2[test,2])
  #   names(nameL) <- as.character(key2)
  #   sup_hd2 <- as.list(nameL)
  #   sup_hd <- c(sup_hd, sup_hd2)
  # }
  # dorigin <- d
  # if(length(dorigin) == 0){
  #   dorigin <- "1970-01-01"
  # }
  traceTime <- as.double(as.POSIXct(x$dt1$time, origin = d))
  sup_hd[["clip"]] <- clippedBits(x$data, nbits = 16)
  sup_hd[["hd"]] <- x$hd
  xdata <- bits2volt(Vmax = Vmax)*x$data
  colnames(xdata) <- seq_len(ncol(x$data))
  if(is.null(Vmax)){
    dunit <- "bits"
  }else{
    dunit <- "mV"
  }
  new("GPR",   
      #--- class GPRvirtual
      version      = "0.3",  
      name         = fName,
      path         = fPath,
      desc         = desc,
      mode         = surveymode,
      date         = d,
      freq         = antfreq, 
      
      data         = xdata,     
      dunit        = dunit,  
      dlab         = "amplitude", 
      
      spunit       = "",  
      crs          = NA_character_,  
      #crs          = "",  
       
      xunit        = posunit,  
      xlab         = "position",
      
      zunit        = "ns",  
      zlab         = "two-way travel time",
      
      vel          = list(v = 0.1),   
      
      # proc         = "list",
      # delineations = "list",
      md           = sup_hd,  
      
      #--- class GPR
      z0           = time_0,    
      time         = traceTime,    
      antsep       = antsep,    
      markers      = trimStr(x$dt1$com), 
      # ann          = "character", 
      
      coord        = coord,     
      rec          = coord_rec,     
      trans        = coord_trans,     
      
      x            = round(x$dt1$pos, 5),     
      z            = seq(0, by = dz, length.out = nrow(x$data))  
      
      # angles      = "matrix" 
  )
}



#------------------------------------------------------------------------------#
#' Read Sensors and Software GPR data
#' 
#' @param dsn [\code{character(1)|connection object}] data source name: 
#'            either the filepath to the GPR data (character),
#'            or an open file connection.
#' @param ntr [\code{integer(1)}] Number of traces (given by .hd file)
#' @param npt [\code{integer(1)}] Number of samples per traces (given by .hd file)
#' @return [\code{list(2)}] Two-elements list: \code{dt1hd} with trace header and
#'         \code{data} with the traces.
#' @seealso \code{\link{readHD}}, \code{\link{readGPS}}
#' @name readDT1
#' @rdname readDT1
#' @export
readDT1 <- function(dsn, ntr, npt){
  dsn <- .openFileIfNot(dsn)
  tags <- c("traces", "position", "samples", "topo", "NA1", "bytes",
            "window", "stacks", 
            "GPSx", "GPSy", "GPSz", 
            "recx", "recy", "recz",
            "transx", "transy", "transz",
            "time0", "zeroflag", "NA7", "time", "flag", "com")
  binSize <- rep(4, 23)
  binSize[9:11] <- 8
  binSize[23] <- 28
  binMod <- rep("numeric", 23)
  binMod[23] <- "character"
  
  
  hDT1 <- list()
  dataDT1 <- matrix(NA, nrow = npt, ncol = ntr)
  
  for(i in 1:ntr){
    for(j in 1:23){
      hDT1[[tags[j]]][i] <- readBinary(dsn, what = binMod[j], n = 1L, 
                                       size =  binSize[j])
    }
    # read the npt * 2 bytes trace data
    dataDT1[,i] <- readBinary(dsn, what = "integer", n = npt, size = 2)
  }
  .closeFileIfNot(dsn)
  return( list(dt1hd = hDT1, data = dataDT1) )
}
#------------------------------------------------------------------------------#

#' Read Sensors and Software .HD file
#' 
#' @param dsn [\code{character(1)|connection object}] data source name: 
#'            either the filepath to the GPR data (character),
#'            or an open file connection.
#' @return [\code{list(3)}]  Three-elements list: \code{HD} containing the 
#'         header info, \code{ntr} the number of trace and \code{npt} the
#'         number of points per trace.
#' @seealso \code{\link{readDT1}}, \code{\link{readGPS}}
#' @name readHD
#' @rdname readHD
#' @export
readHD <- function(dsn){
  # scan header file
  headHD <- scan(dsn, what = character(), strip.white = TRUE,
                 quiet = TRUE, fill = TRUE, blank.lines.skip = TRUE, 
                 flush = TRUE, sep = "\n")
  hHD <- data.frame( tag = character(), 
                     val = character(), 
                     stringsAsFactors = FALSE)
  for(i in seq_along(headHD)){
    hdline <- strsplit(headHD[i], "=")[[1]]
    if(length(hdline) < 2){
      hHD[i,1] <- ""
      hHD[i,2] <- trimStr(hdline[1])
    }else{
      hHD[i,1:2] <-  as.character(sapply(hdline[1:2], trimStr))
    }
  }
  ntr <- as.integer(.getHD(hHD, "NUMBER OF TRACES"))
  npt <- as.integer(.getHD(hHD, "NUMBER OF PTS/TRC"))
  .closeFileIfNot(dsn)
  return(list(HD = hHD, ntr = ntr, npt = npt))
}

#------------------------------------------------------------------------------#

#' Read Sensors and Software .GPS file
#' 
#' @param dsn [\code{character(1)|connection object}] data source name: 
#'             either the filepath to the GPR data (character),
#'            or an open file connection.
#' @param returnSf [\code{logical(1)}] If \code{TRUE} returns an object of class
#'              \code{sf} (see \code{\link{sf}{sf}}). If \code{FALSE} returns
#'              a \code{data.frame}.
#' @return [\code{sf|data.frame|NULL}] Either an object of class
#'         \code{sf} or a \code{data.frame} or \code{NULL} if no
#'         coordinates could be extracted.
#' @seealso \code{\link{readDT1}}, \code{\link{readHD}}
#' @name readGPS
#' @rdname readGPS
#' @export
readGPS <- function(dsn, returnSf = TRUE){
  X <- .readGPS(dsn)
  if(length(X$tr_id) == 0) return(NULL)
  xyzt <- .getLonLatFromGPGGA(X$gpgga)
  mrk <- cbind(xyzt[ ,1:3], X$tr_id, X$tr_pos, xyzt[ ,4])
  names(mrk) <- c("x", "y", "z", "id", "pos", "time")
  if(isTRUE(returnSf)){
    mrk <- sf::st_as_sf(x      = mrk,
                        coords = c("x", "y", "z"),
                        crs    = 4326)
  }
  .closeFileIfNot(dsn)
  return(mrk)
}

.readGPS <- function(dsn){
  # read file
  x <- scan(dsn, what = character(), sep = "\n", quiet = TRUE)
  # find positions corresponding to trace markers
  xtr <- which(grepl("Trace \\#[0-9]+", x,            # index of the marker rows
                     ignore.case = TRUE, useBytes = TRUE ))
  xgpgga <- integer(length(xtr) - 1)
  xtr <- c(xtr, nrow(x))
  todelete <- c()
  for(i in seq_along(xtr[-1])){
    for(j in (xtr[i] + 1):(xtr[i+1] - 1)){
      test <- grepl("(\\$GPGGA)", x[j], ignore.case = TRUE, useBytes = TRUE )
      if(isTRUE(test)){
        xgpgga[i] <- j
        break
      }
    }
    if(!isTRUE(test)){
      todelete <- c(todelete, i)
    }
  }
  if(length(todelete) > 0){
    xtr <- xtr[-todelete]
    xgpgga <- xgpgga[-todelete]
  }
  # trace number
  tr_id <- as.integer(extractPattern(x[xtr], pattern = "(\\#[0-9]+)", 
                                     start = 1, stop = -1))
  # trace position
  tr_pos <- as.numeric(extractPattern(x[xtr], 
                                      pattern = "(position [0-9]*\\.?[0-9]*)", 
                                      start = 9, stop = 0))
  pat_gpgga <- paste0("\\$(?<ID>GPGGA),(?<UTC>[0-9.]+),(?<lat>[0-9.]+),",
                      "(?<NS>[NS]),(?<lon>[0-9.]+),(?<EW>[EW]),(?<fix>[0-9]),",
                      "(?<NbSat>[0-9.]+),(?<HDOP>[0-9.]+),(?<H>[0-9.]+),",
                      "(?<mf>[MmFf]+)") 
  #,(?<HGeoid>[0-9.]+),(?<mf2>[mMfF+),",
  # "(?<TDGPS>[0-9.]+),(?<DGPSID> [A-z0-9.]+)"  )
  
  gpgga <- extractPattern(x[xgpgga], pattern = pat_gpgga, 
                          start = 0, stop = -1)
  dim(gpgga) <- c(length(xgpgga), 11)
  gpgga <- as.data.frame(gpgga, stringsAsFactors = FALSE)
  colnames(gpgga) <- c("ID", "UTC", "lat", "NS", "lon", "EW", 
                       "fix", "NbSat", "HDOP", "H", "mf")
  sel <- which(gpgga[,1] != "")
  tr_id <- tr_id[sel]
  tr_pos <- tr_pos[sel]
  gpgga <- gpgga[sel,]
  if(any(c(nrow(gpgga), length(tr_id)) != length(tr_pos))){
    stop("Problem - code 'qoiwelk'. Please contact me\n",
         "emanuel.huber@pm.me")
  }
  # .closeFileIfNot(dsn)
  return(list(tr_id = tr_id, tr_pos = tr_pos, gpgga = gpgga))
}


