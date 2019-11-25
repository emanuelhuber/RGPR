



.gprImpulseRadar <- function(x, fName = character(0), desc = character(0),
                             fPath = character(0), Vmax = 50){ 
  rec_coord <- matrix(nrow = 0, ncol = 0)
  trans_coord <- matrix(nrow = 0, ncol = 0)
  coord <- matrix(nrow = 0, ncol = 0)
  pos_used <- integer(nrow(x$hd))
  nbits <- .getHD(x$hd, "DATA VERSION", position = TRUE)
  if(!is.null(nbits)){
    pos_used[nbits[2]] <- 1L
  }else{
    nbits <- 16
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
      data = bits2volt(Vmax = Vmax, nbits = nbits[1])*x$data,
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








readIPRB <- function(dsn, npt, ntr, nbytes){
  if(!inherits(dsn, "connection")){
    dsn <- file(dsn, 'rb')
  }
  
  #--- READ .IPRB
  IPRB <- matrix(NA, nrow = npt, ncol = ntr)
  for(i in 1:ntr){
    IPRB[,i] <- readBin(dsn, what = integer(), n = npt, size = nbytes)
  }
  .closeFileIfNot(dsn)
  return(IPRB)
}

readIPRH <- function(dsn){
  if(!inherits(dsn, "connection")){
    dsn <- file(dsn, 'rb')
  }
  #--- read header file
  headHD <- scan( dsn, what = character(), strip.white = TRUE,
                  quiet = TRUE, fill = TRUE, blank.lines.skip = TRUE, 
                  flush = TRUE, sep = "\n")
  nHD <- length(headHD)
  hHD <- data.frame( tag = character(), val = character(), 
                     stringsAsFactors = FALSE)
  for(i in seq_along(headHD)){
    hdline <- strsplit(headHD[i], ":")[[1]]
    if(length(hdline) < 2){
      hHD[i,1] <- ""
      hHD[i,2] <- trimStr(hdline[1])
    }else{
      hHD[i,1:2] <-  as.character(sapply(hdline[1:2],trimStr))
    }
  }
  ntr    <- .getHD(hHD, "LAST TRACE")
  npt    <- .getHD(hHD, "SAMPLES")
  nbits  <- .getHD(hHD, "DATA VERSION")
  if(nbits == 16){
    nbytes <- 2
  }else if(nbits == 32){
    nbytes <- 4
  }
  .closeFileIfNot(dsn)
  return(list(HD = hHD, ntr = ntr, npt = npt, nbytes = nbytes))
}

# TIME: trace_numer date(yyyy-mm-dd) time(hh:mm:sss)
readTIME <- function(dsn){
  if(!inherits(dsn, "connection")){
    dsn <- file(dsn, 'rb')
  }
  content <- verboseF(readLines(dsn), verbose = FALSE)
  if(length(content) == 0){
    .closeFileIfNot(dsn)
    return(NULL)
  }
  # hTIME <- read.table(textConnection(gsub(",", ";", content)), 
  #                    dec = ".", header = FALSE, stringsAsFactors = FALSE,
  #                    sep = ";")
  hTIME <- read.table(textConnection(content),
                      colClasses = "character",
                      stringsAsFactors = FALSE, 
                      sep = "")
  .closeFileIfNot(dsn)
  return(hTIME)
}

# marker
readMRK <- function(dsn){
  if(!inherits(dsn, "connection")){
    dsn <- file(dsn, 'rb')
  }
  content <- verboseF(readLines(dsn), verbose = FALSE)
  if(length(content) == 0){
    .closeFileIfNot(dsn)
    return(NULL)
  }
  # hMRK <- read.table(textConnection(gsub(",", ";", content)), 
  #                    dec = ".", header = FALSE, stringsAsFactors = FALSE,
  #                    sep = ";")
  hMRK <- read.table(textConnection(content),
                      colClasses = "character",
                      stringsAsFactors = FALSE, 
                      sep = "")
  .closeFileIfNot(dsn)
  return(hMRK)
}

# GPS POSITION: trace date time north N East E Elevation M Quality
#' @export
readIPRCOR <- function(dsn){
  if(!inherits(dsn, "connection")){
    dsn <- file(dsn, 'rb')
  }
  content <- verboseF(readLines(dsn), verbose = FALSE)
  if(length(content) == 0){
    .closeFileIfNot(dsn)
    return(NULL)
  }
  # hCOR <- read.table(textConnection(gsub(",", ";", content)), 
  #                    dec = ".", header = FALSE, stringsAsFactors = FALSE,
  #                    sep = ";")
  hCOR <- read.table(textConnection(content),
                     stringsAsFactors = FALSE, 
                     sep = "")
  # hCOR <- read.table(textConnection(gsub(",", "\t", readLines(dsn))), 
  #                    dec = ".", header = FALSE, stringsAsFactors = FALSE)
  colnames(hCOR) <- c("id", "date", "time", "y", 
                      "lat", "x",
                      "long", "z", "unit", "accuracy")
  
  if(any(grepl("S", hCOR[["lat"]]))) hCOR[["y"]] <- -hCOR[["y"]]
  if(any(grepl("W", hCOR[["long"]]))) hCOR[["x"]] <- -hCOR[["x"]]
  
  .closeFileIfNot(dsn)
  # return(hCOR)
  return(hCOR[c("x", "y", "z", "id", "date", "time")])
}

# 
# 
# 
# readImpulseRadar <- function( dsn, dsn2 = NULL){
#   
#   fNameOpt <- NULL
#   hTime <- NULL
#   hCor <- NULL
#   hMrk <- NULL
#   
#   if( inherits(dsn, "connection") ){
#     if(!inherits(dsn2, "connection")){
#       stop("Please add an additional connection to 'readGPR()' for ",
#            "the header file '*.hd'")
#     }
#   }else if(is.character(dsn) && is.null(dsn2)){
#     fName <- getFName(dsn, ext = c(".iprh", ".iprb"))
#     #--- READ OPTIONAL FILES
#     fNameOpt <- getFName(dsn, ext = c(".cor", ".time", ".mrk"), 
#                          throwError = FALSE)
#     dsn <- file(fName$iprb , "rb")
#     dsn2 <- fName$iprh
#   }else{
#     if(!file.exists(dsn)){
#       stop("File ", dsn, " does not exist!")
#     }
#     if(!file.exists(dsn2)){
#       stop("File ", dsn2, " does not exist!")
#     }
#     dsn_save <- c(dsn, dsn2)
#     dsn  <- file(dsn_save[grepl("(\\.iprb)$", dsn_save)], "rb")
#     dsn2 <- dsn_save[grepl("(\\.iprh)$", dsn_save)]
#   }
#   
#   
#   #--- read header file
#   headHD <- scan( dsn2, what = character(), strip.white = TRUE,
#                   quiet = TRUE, fill = TRUE, blank.lines.skip = TRUE, 
#                   flush = TRUE, sep = "\n")
#   nHD <- length(headHD)
#   hHD <- data.frame( tag = character(), val = character(), 
#                      stringsAsFactors = FALSE)
#   for(i in seq_along(headHD)){
#     hdline <- strsplit(headHD[i], ":")[[1]]
#     if(length(hdline) < 2){
#       hHD[i,1] <- ""
#       hHD[i,2] <- trimStr(hdline[1])
#     }else{
#       hHD[i,1:2] <-  as.character(sapply(hdline[1:2],trimStr))
#     }
#   }
#   nTr    <- .getHD(hHD, "LAST TRACE")
#   nPt    <- .getHD(hHD, "SAMPLES")
#   nBits <- .getHD(hHD, "DATA VERSION")
#   if(nBits == 16){
#     nBytes <- 2
#   }else if(nBits == 32){
#     nBytes <- 4
#   }
#   
#   #--- READ .IPRB
#   bind <- matrix(NA, nrow = nPt, ncol = nTr)
#   for(i in 1:nTr){
#     bind[,i] <- readBin(dsn, what=integer(), n = nPt, size = nBytes)
#   }
#   
#   # TIME: trace_numer date(yyyy-mm-dd) time(hh:mm:sss)
#   if(!is.null(fNameOpt$time))  hTime <- read.table(fNameOpt$time, 
#                                                    stringsAsFactors = FALSE)
#   # GPS POSITION: trace date time north N East E Elevation M Quality
#   if(!is.null(fNameOpt$cor)) hCor <- read.table(fNameOpt$cor,
#                                                 stringsAsFactors = FALSE)
#   # marker
#   if(!is.null(fNameOpt$mrk)) hMrk <- read.table(fNameOpt$mrk, 
#                                                 stringsAsFactors = FALSE)
#   
#   close(dsn)
#   if(inherits(dsn2, "connection")){  close(dsn2)}
#   
#   return( list(hd = hHD, data = bind, time = hTime, cor = hCor, mkr = hMrk) )
# }