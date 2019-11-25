

#--------------- read MALA files -------------------#

.gprRD3 <- function(x, fName = character(0), desc = character(0),
                    fPath = character(0), nbits = 16, Vmax = 50){  
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
    dx <- 1
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
      data        = bits2volt(Vmax = Vmax, nbits = nbits)*x$data,
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



readRD37 <- function(dsn, ntr, npt, nbytes = 2){
  if(!inherits(dsn, "connection")){
    dsn <- file(dsn, 'rb')
  }
  
  dataRD7 <- matrix(NA, nrow = npt, ncol = ntr)
  for(i in seq_len(ntr)){
    dataRD7[, i] <- readBin(dsn, what = integer(), n = npt, size = nbytes)
  }
  
  .closeFileIfNot(dsn)
  return(dataRD7)
}

readRAD <- function(dsn){
  ##---- RAD file
  headRAD <- scan(dsn, what = character(), strip.white = TRUE,
                  quiet = TRUE, fill = TRUE, blank.lines.skip = TRUE, 
                  flush = TRUE, sep = "\n")
  
  nRAD <- length(headRAD)
  hRAD <- data.frame( tag = character(), val = character(), 
                      stringsAsFactors = FALSE)
  for(i in seq_along(headRAD)){
    hdline <- strsplit(headRAD[i], ":")[[1]]
    hRAD[i,1:2] <-  as.character(sapply(hdline[1:2],trimStr))
  }
  ntr <- .getHD(hRAD, "LAST TRACE")
  npt <- .getHD(hRAD, "SAMPLES")
  .closeFileIfNot(dsn)
  return(list(HD = hRAD, ntr = ntr, npt = npt))
}

#' @export
readCOR <- function(dsn){
  if(!inherits(dsn, "connection")){
    dsn <- file(dsn, 'rb')
  }
  # hCOR <- read.table(dsn, sep = "\t", dec = ".", header = FALSE,
  #                    stringsAsFactors = FALSE)
  # colnames(hCOR) <- c("traces", "date", "time", "latitude", "longitude",
  #                 "height", "accuracy")
  content <- verboseF(readLines(dsn), verbose = FALSE)
  if(length(content) == 0){
    .closeFileIfNot(dsn)
    return(NULL)
  }
  # hCOR <- read.table(textConnection(gsub(",", ";", content)), 
  #                    dec = ".", header = FALSE, stringsAsFactors = FALSE,
  #                    sep = ";")
  hCOR <- read.table(textConnection(gsub(",", "\t", content)),
                     #colClasses = "character",
                     stringsAsFactors = FALSE, 
                     sep = "")
  if(ncol(hCOR) == 9){
    unts <- gsub("[0-9.]", "", hCOR[,8])[1]
    hCOR[,8] <- as.numeric(gsub(unts, "", hCOR[,8]))
    colnames(hCOR) <- c("id", "date", "time", "y", 
                        "lat", "x",
                        "long", "z", "accuracy")
  }else{
    colnames(hCOR) <- c("id", "date", "time", "y", 
                       "lat", "x",
                       "long", "z", "unit", "accuracy")
  }
  if(grepl(":", hCOR["x"])){  # longitude
    z <- sapply((strsplit(hCOR[["x"]], ":")), as.numeric)
    hCOR[["x"]] <- z[1, ] + z[2, ]/60 + z[3, ]/3600
  }
  if(grepl(":", hCOR["y"])){   # latitude
    z <- sapply((strsplit(hCOR[["y"]], ":")), as.numeric)
    hCOR[["y"]] <- z[1, ] + z[2, ]/60 + z[3, ]/3600
  }
  if(any(grepl("S", hCOR[["lat"]]))) hCOR[["y"]] <- -hCOR[["y"]]
  if(any(grepl("W", hCOR[["long"]]))) hCOR[["x"]] <- -hCOR[["x"]]
  
  .closeFileIfNot(dsn)
  return(hCOR[c("x", "y", "z", "id", "date", "time")])
}
