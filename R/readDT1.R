# requires following functions:
# - '.getHD()'




#------------------------------------------------------------------------------#

# x = classical GPR list
.gprDT1 <- function(x, fName = character(0), desc = character(0),
                 fPath = character(0), Vmax = NULL){
  if(is.null(Vmax)) Vmax <- 50
  
  ntr <- ncol(x$data)
  
  if(sum(abs(c(x$dt1$recx, x$dt1$recy, x$dt1$recz))) > 0){
    rec_coord <- matrix(nrow = ntr, ncol = 3)
    rec_coord[, 1] <- x$dt1$recx
    rec_coord[, 2] <- x$dt1$recy
    rec_coord[, 3] <- x$dt1$recz
  }else{
    rec_coord <- matrix(nrow = 0, ncol = 3)
  }
  
  if(sum(abs(c(x$dt1$transx, x$dt1$transy, x$dt1$transz))) > 0){
    trans_coord <- matrix(nrow = ntr, ncol = 3)
    trans_coord[, 1] <- x$dt1$transx
    trans_coord[, 2] <- x$dt1$transy
    trans_coord[, 3] <- x$dt1$transz
  }else{
    trans_coord <- matrix(nrow = 0, ncol = 3)
  }  
  
  
  # }else if(sum(abs(x$dt1$topo), na.rm = TRUE) > 0){
  #   topo <- x$dt1$topo
  # }else{
  # }
  
  coord <- matrix(nrow = 0, ncol = 3)
  
  
  
  # rec_coord[, 1] <- cbind(x$dt1$recx, x$dt1$recy, x$dt1$recz)
  # trans_coord <- cbind(x$dt1$transx, x$dt1$transy, x$dt1$transz)
  # if(sum(is.na(rec_coord)) > 0){
  #   warning(paste(fName,": ",sum(is.na(rec_coord)), 
  #                 "NA's in the receiver coordinates\n"))
  # }
  # if(sum(is.na(trans_coord)) > 0){
  #   warning(paste(fName,": ",sum(is.na(trans_coord)), 
  #                 "NA's in the transmitter coordinates\n"))
  # }
  # if(sum(is.na(x$dt1$topo)) > 0){
  #   warning(paste(fName,": ",sum(is.na(x$dt1$topo)), 
  #                 "NA's in the topo coordinates\n"))
  # }
  # if(sum(abs(rec_coord),na.rm = TRUE) == 0 ){
  #   rec_coord <- matrix(nrow = 0, ncol = 0) 
  # }
  # if(sum(abs(trans_coord), na.rm = TRUE) == 0){
  #   trans_coord <- matrix(nrow = 0, ncol = 0) 
  # }
  # if(sum(abs(x$dt1$topo), na.rm = TRUE) == 0){
  #   coord <- matrix(nrow = 0, ncol = 0) 
  # }else{
  #   coord <- matrix(0, nrow = ncol(x$data), ncol = 3)
  #   coord[,3] <- x$dt1$topo
  # }
  
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
  velocity <- 0.1 
  posunit <- .getHD(x$hd, "POSITION UNITS",number=FALSE, position=TRUE)
  if(!is.null(posunit)){
    pos_used[as.numeric(posunit[2])] <- 1L
  }else{
    # posunit <- "m"
    if( tolower(posunit) %in% c("metre", "metres", "meter", "meters")){
      posunit <- "m"
      # velocity <- 0.1             # m/ns
    }else if(tolower(posunit) %in% c("feet", "feets")){
      posunit <- "ft" 
      velocity <- velocity * 3.28084   # ft/ns
    }
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
    sup_hd[["endpos"]] <- as.numeric(endpos[1])
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
  if(any(!is.finite(x$dt1$time))) x$dt1$time <- seq_len(ntr)
  traceTime <- as.double(as.POSIXct(x$dt1$time, origin = as.Date(dorigin)))
  sup_hd[["clip"]] <- getClippedBits(x$data, nbits = 16)
  
  if(sum(abs(x$dt1$topo), na.rm = TRUE) > 0){
    sup_hd[["topo"]] <- x$dt1$topo
  }
  
  if(sum(abs(c(x$dt1$GPSx, x$dt1$GPSy, x$dt1$GPSz))) > 0){
    GPS <- matrix(nrow = ntr, ncol = 3)
    GPS[, 1] <- x$dt1$GPSx
    GPS[, 2] <- x$dt1$GPSy
    GPS[, 3] <- x$dt1$GPSz
    sup_hd[["GPS"]] <- GPS
  }
  
  new("GPR",   
      version     = "0.2",
      data        = bits2volt(Vmax = Vmax)*x$data,
      traces      = x$dt1$traces,
      fid         = trimStr(x$dt1$com),
      coord       = coord,
      pos         = x$dt1$pos,
      depth       = seq(0, by = dz, length.out = nrow(x$data)),
      rec         = rec_coord,
      trans       = trans_coord,
      time0       = time_0,
      time        = traceTime,
      proc        = character(0),
      vel         = list(v = velocity),                  # m/ns
      name        = fName,
      description = desc,
      filepath    = fPath,
      dz          = dz,  
      dx          = dx[1],
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




#------------------------------------------------------------------------------#
#' @export
readDT1 <- function(dsn){
  if(!inherits(dsn, "connection")){
    dsn <- file(dsn, 'rb')
  }
  
  #----- 1. Define the tag names, byte size and type
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
  
  #----- 2.  try to estimate the number of traces from the DT1 file only
  # file length
  fileLength <- .flen(dsn)
  # number of points 
  invisible(seek(dsn, where = 2*4, origin = "start"))
  npt <- readBinary(dsn, what = "numeric", n = 1L, size = 4)
  invisible(seek(dsn, where = 0, origin = "start"))
  # number of traces
  trLen <- sum(binSize) + 2 * npt
  ntr <- fileLength/trLen
  # check the values
  if(ntr %% 1 != 0){
    # something went wrong
    tst <- fileLength / (sum(binSize) + 2 * (1:50000))
    i <- which(tst %% 1 == 0)
    # tst[i]
    # plot(i, tst[i])
    for(k in seq_along(tst[i])){
      nb <- (sum(binSize) + 2 * i[k])
      invisible(seek(dsn, where = nb + 8, origin = "start"))
      nptk <- readBinary(dsn, what = "numeric", n = 1L, size = 4)
      if(!is.na(nptk) && nptk == npt){
        ntr <- tst[i][k]
        npt <- i[k]
        break
      }
    }
    if(k == length(i)){
      warning("Maybe something went wrong with you file...\n",
              "Do not hestiate to contact me, if you cannot read you file!")
    }
  }
 
  
  hDT1 <- list()
  dataDT1 <- matrix(NA, nrow = npt, ncol = ntr)
  
  invisible(seek(dsn, where = 0, origin = "start"))
  for(i in 1:ntr){
    for(j in 1:23){
      hDT1[[tags[j]]][i] <- verboseF(readBinary(dsn, what = binMod[j], n = 1L, 
                                       size =  binSize[j]), verbose = FALSE)
    }
    # read the 28 characters long comment
    # hDT1[[tags[26]]][i] <- readChar(dsn, 28)
    # read the npt * 2 bytes trace data
    dataDT1[,i] <- readBinary(dsn, what = "integer", n = npt, size = hDT1[["bytes"]][i])
  }
  .closeFileIfNot(dsn)
  return( list(dt1hd = hDT1, data = dataDT1) )
}
#------------------------------------------------------------------------------#

#' @export
readHD <- function(dsn){
  # scan header file
  dsn <- .openFileIfNot(dsn)  # in case there is some binary stuff
  headHD <- scan(dsn, what = character(), strip.white = TRUE,
                 quiet = TRUE, fill = TRUE, blank.lines.skip = TRUE, 
                 flush = TRUE, sep = "\n")
  hHD <- data.frame( tag = character(), val = character(), 
                     stringsAsFactors = FALSE)
  for(i in seq_along(headHD)){
    hdline <- strsplit(headHD[i], "=")[[1]]
    if(length(hdline) < 2){
      hHD[i,1] <- ""
      hHD[i,2] <- trimStr(hdline[1])
    }else{
      hHD[i,1:2] <-  as.character(sapply(hdline[1:2],trimStr))
    }
  }
  ntr <- .getHD(hHD, "NUMBER OF TRACES")
  npt <- .getHD(hHD, "NUMBER OF PTS/TRC")
  .closeFileIfNot(dsn)
  return(list(HD = hHD, ntr = ntr, npt = npt))
}

#------------------------------------------------------------------------------#

#' Read .GPS files (Sensors and Software)
#' 
#' @param dsn [\code{character(1)|connection object}]data source name: 
#'            either the filepath to the GPR data (character),
#'            or an open file connection.
#' 
#' @export
readGPS <- function(dsn, toUTM = FALSE){
  X <- .readGPS(dsn)
  if(X$type == "GNGGA"){
    xyzt <- .getLatLonFromGNGGA(X$gpgga)
  }else if(X$type == "GPGGA"){
    xyzt <- .getLatLonFromGPGGA(X$gpgga)
  }
  xyzt_crs <- "EPSG:4326"
  if(toUTM == TRUE){
    topoUTM <-  llToUTM(lat = xyzt[,2], 
                        lon = xyzt[,1], 
                        zone = NULL, 
                        south = (X$gpgga$NS[1] == "S"),
                        west  = (X$gpgga$EW[1] == "W"))
    xyzt[, 1:2] <- topoUTM$xy
    xyzt_crs <- topoUTM$crs
  }
  
  mrk <- cbind(xyzt[ ,1:3], X$tr_id, X$tr_pos, xyzt[ ,4])
  # mrk <- as.matrix(mrk)
  names(mrk) <- c("x", "y", "z", "id", "pos", "time")
  .closeFileIfNot(dsn)
  return(list(mrk = mrk, crs = xyzt_crs))
}

.readGPS <- function(dsn){
  dsn <- .openFileIfNot(dsn)  # in case there is some binary stuff
  x <- scan(dsn, what = character(), sep = "\n", quiet = TRUE, skipNul = TRUE)
  
  # fac <- 1
  test1 <- grepl("(\\$GPGGA)", x)
  if(any(test1)){
    type <- "GPGGA"
  }else{
    test2 <- grepl("(\\$GNGGA)", x)
    if(any(test2)){
      type <- "GNGGA"
    }else{
      stop("Problem - no GPGGA or GNGGA string. Please contact me\n",
           "emanuel.huber@pm.me")
    }
  }
  
  xtr <- which(grepl("Trace \\#[0-9]+", x, 
                     ignore.case = TRUE, useBytes = TRUE ))
  #if(length(xtr) != length(xgpgga)){}
  xgpgga <- integer(length(xtr) - 1)
  xtr <- c(xtr, nrow(x))
  todelete <- c()
  for(i in seq_along(xtr[-1])){
    for(j in (xtr[i] + 1):(xtr[i+1] - 1)){
      
      test <- grepl(paste0("(\\$", type, ")"), x[j], ignore.case = TRUE, useBytes = TRUE )
      if(isTRUE(test)){
        xgpgga[i] <- j
        # message(xtr[i], " - ", j)
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
  # pat_tr <- "(\\#[0-9]+)"
  # matches <- regexpr(pat_tr, x[xtr], perl=TRUE)
  # first <- attr(matches, "capture.start") + 1
  # last <- first + attr(matches, "capture.length") - 1
  # tr_id <- as.integer(mapply(substring, x[xtr], first, last, 
  #                            USE.NAMES = FALSE))
  tr_id <- as.integer(extractPattern(x[xtr], pat = "(\\#[0-9]+)", 
                                     shift1 = 1, shift2 = -1))
  
  # trace position
  # pat_tr <- "(position [0-9]*\\.?[0-9]*)"
  # matches <- regexpr(pat_tr, x[xtr], perl=TRUE)
  # first <- attr(matches, "capture.start") + 9
  # last <- first + attr(matches, "capture.length")
  # tr_pos <- as.numeric(mapply(substring, x[xtr], first, last, 
  #                             USE.NAMES = FALSE))
  tr_pos <- as.numeric(extractPattern(x[xtr], 
                                      pat = "(position [0-9]*\\.?[0-9]*)", 
                                      shift1 = 9, shift2 = 0))
  
  pat_gpgga <- paste0("\\$(?<ID>[A-Z]+GGA),(?<UTC>[0-9.]+),(?<lat>[0-9.]+),",
                      "(?<NS>[NS]),(?<lon>[0-9.]+),(?<EW>[EW]),(?<fix>[0-9]),",
                      "(?<NbSat>[0-9.]+),(?<HDOP>[0-9.]+),(?<H>[0-9.]+),",
                      "(?<mf>[MmFf]+)") 
  #,(?<HGeoid>[0-9.]+),(?<mf2>[mMfF+),",
  # "(?<TDGPS>[0-9.]+),(?<DGPSID> [A-z0-9.]+)"
  # )
  
  # matches <- regexpr(pat_gpgga, x[xgpgga], perl=TRUE)
  # first <- attr(matches, "capture.start")
  # last <- first + attr(matches, "capture.length") -1
  # gpgga <- mapply(substring, x[xgpgga], first, last, USE.NAMES = FALSE)
  gpgga <- extractPattern(x[xgpgga], pat = pat_gpgga, 
                          shift1 = 0, shift2 = -1)
  
  dim(gpgga) <- c(length(xgpgga), 11)
  gpgga <- as.data.frame(gpgga, stringsAsFactors = FALSE)
  colnames(gpgga) <- c("ID", "UTC", "lat", "NS", "lon", "EW", 
                       "fix", "NbSat", "HDOP", "H", "mf")
  # gpgga$lat <- as.numeric(gpgga$lat) * fac
  # gpgga$lon <- as.numeric(gpgga$lon) * fac
  
  sel <- which(gpgga[,1] != "")
  
  tr_id <- tr_id[sel]
  tr_pos <- tr_pos[sel]
  gpgga <- gpgga[sel,]
  
  if(any(c(nrow(gpgga), length(tr_id)) != length(tr_pos))){
    stop("Problem - code 'qoiwelk'. Please contact me\n",
         "emanuel.huber@pm.me")
  }
  .closeFileIfNot(dsn)
  return(list(tr_id = tr_id, tr_pos = tr_pos, gpgga = gpgga, type = type))
}




# # -------------------------------------------
# # ------------readDT1--------------------------
# # -------------------------------------------
# # @name  readDT1 (plot Ground Penetrating Radar image)
# # @description This function read *.HD and associated *.DT1
# # files from Sensors & Software.
# 
# # @date 30.04.2014 08:33
# # @auteur Emanuel Huber
# # @param [text]    fPath       (file path of *.hd or *.dt1 file)
# # @require source("trimStr.R")
# # source('trimStr.R')
# # cat('> Function(s) loaded: "trimStr.R" \n')
# # @return list((hd = headerHD, dt1hd = headerDT1, data=myData))
# # -------------------------------------------
# 
# readDT1 <- function(dsn, dsn2 = NULL){
#   
#   if( inherits(dsn, "connection") ){
#     if(!inherits(dsn2, "connection")){
#       stop("Please add an additional connection to 'readGPR()' for ",
#            "the header file '*.hd'")
#     }
#   }else if(is.character(dsn) && is.null(dsn2)){
#     fName <- getFName(dsn, ext = c(".HD", ".DT1"))
#     # open dt1 file
#     dsn <- file(fName$dt1 , "rb")
#     dsn2 <- fName$hd
#   }else{
#     if(!file.exists(dsn)){
#       stop("File ", dsn, " does not exist!")
#     }
#     if(!file.exists(dsn2)){
#       stop("File ", dsn2, " does not exist!")
#     }
#     dsn_save <- c(dsn, dsn2)
#     dsn  <- file(dsn_save[grepl("(\\.rd7)$", dsn_save)], "rb")
#     dsn2 <- dsn_save[grepl("(\\.rad)$", dsn_save)]
#   }
#   
#   # scan header file
#   headHD <- scan(dsn2, what = character(), strip.white = TRUE,
#                  quiet = TRUE, fill = TRUE, blank.lines.skip = TRUE, 
#                  flush = TRUE, sep = "\n")
#   nHD <- length(headHD)
#   hHD <- data.frame( tag = character(), val = character(), 
#                      stringsAsFactors = FALSE)
#   for(i in seq_along(headHD)){
#     hdline <- strsplit(headHD[i], "=")[[1]]
#     if(length(hdline) < 2){
#       hHD[i,1] <- ""
#       hHD[i,2] <- trimStr(hdline[1])
#     }else{
#       hHD[i,1:2] <-  as.character(sapply(hdline[1:2],trimStr))
#     }
#   }
#   nTr <- .getHD(hHD, "NUMBER OF TRACES")
#   nPt <- .getHD(hHD, "NUMBER OF PTS/TRC")
#   
#   #--- READ DT1
#   tags <- c("traces", "position", "samples","topo", "NA1", "bytes",
#             "tracenb", "stack","window","NA2", "NA3", "NA4",
#             "NA5", "NA6", "recx","recy","recz","transx","transy",
#             "transz","time0","zeroflag", "NA7", "time","x8","com")  
#   hDT1 <- list()
#   dataDT1 <- matrix(NA, nrow = nPt, ncol = nTr)
#   
#   for(i in 1:nTr){
#     for(j in 1:25){
#       hDT1[[tags[j]]][i] <- readBin(dsn, what = numeric(), n = 1L, size = 4)
#     }
#     # read the 28 characters long comment
#     hDT1[[tags[26]]][i] <- readChar(dsn, 28)
#     # read the nPt * 2 bytes trace data
#     dataDT1[,i] <- readBin(dsn, what = integer(), n = nPt, size = 2)
#   }
#   
#   close(dsn)
#   if(inherits(dsn2, "connection") ) close(dsn2)
#   
#   return( list(hd = hHD, dt1hd = hDT1, data = dataDT1) )
# }



