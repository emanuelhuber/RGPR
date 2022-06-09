.gprUtsi <- function(x, fName = "", fPath = "", 
                     desc = "", Vmax = NULL){
  
  if(is.null(Vmax)) Vmax <- 50
  
  y <- new("GPR", 
           version     = "0.2",
           data        = x[["data"]] * bits2volt(Vmax = Vmax, 
                                                 nbits = x$hd$bits),
           traces      = 1:ncol(x[["data"]]),       # trace numbering
           pos         = 1:ncol(x[["data"]]),                # trace position
           depth       = 1:nrow(x[["data"]]),
           time0       = rep(0, ncol(x[["data"]])),  
           time        = rep(0, ncol(x[["data"]])), # time of trace records
           proc        =  character(0),       
           vel         = list(v = 0.1),                 # m/ns
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


# Data is stored in 16 bits as raw data. The only parameters that affect the 
# recorded data directly are Tsweep and Read.  The other parameters affect the 
# display and may be varied during or after completion of the survey 
# (see sections 5.3 and 6.3).  
# The data is stored under the run name as RUNNAME.dat. The run details are 
# stored in the file RUNNAME.hdr.  The stored data format is 2 bytes per point 
# with LSB byte followed by MSB byte. There are 256 points (512 bytes) 
# followed by 1 byte of marker (ASCII).
# In addition to the data and header files, GPS files (.gps) and gps number 
# files (.gpt) are generated, irrespective of whether or not a GPS is used.  
# If a GPS is not used, the .gps and .gpt files will be 0kB in size.
# The HDR file is an ASCII file (can be read using notepad) that contains the 
# radar parameters and notes about the run.

# readUtsi <- function(dsn, dsn2 = NULL){
#   
#   if( inherits(dsn, "connection") ){
#     if(!inherits(dsn2, "connection")){
#       stop("Please add an additional connection to 'readGPR()' for ",
#            "the header file '*.hdr'")
#     }
#   }else if(is.character(dsn) && is.null(dsn2)){
#     fName <- getFName(dsn, ext = c(".hdr", ".dat"))
#     # open dt1 file
#     dsn  <- file(fName$dat , "rb")
#     dsn2 <- file(fName$hdr , "rb")
#   }else{
#     if(!file.exists(dsn)){
#       stop("File ", dsn, " does not exist!")
#     }
#     if(!file.exists(dsn2)){
#       stop("File ", dsn2, " does not exist!")
#     }
#     dsn_save <- c(dsn, dsn2)
#     dsn  <- file(dsn_save[grepl("(\\.dat)$", dsn_save)], "rb")
#     dsn2 <- dsn_save[grepl("(\\.hdr)$", dsn_save)]
#   }
#   
#   hd <- readUtsiHDR(dsn2) 
#   z <- readUtsiDat(dsn, splPerScan = hd$splPerScan, bits = hd$bits)
#   z[["hd"]] <- hd
#   close(dsn)
#   close(dsn2)
#   return(z)
#   
# }

readUtsiHDR <- function(dsn){
  if(!inherits(dsn, "connection")){
    dsn <- file(dsn, 'rb')
  }
  
  hd <- c()
  
  seek(dsn, where = 0, origin = "start")
  #------------------ Utsi header *.hdr -----------------------------------------#
  u <- readBin(dsn, what = "raw", n = 2, size = 1)
  hd$magic_number <- sf::rawToHex(u)
  if(hd$magic_number != "0f20"){
    message("Magic number in '", 
            summary.connection(dsn)$description, 
            "' is '",
            hd$magic_number,
            "' instead of '0f20'")
  }
  u <- readLines(dsn, n = 1)
  u <- strsplit(u, split = ", ")[[1]]
  hd$software_version <- u[1]
  hd$software_date <- u[2]
  
  # scan(dsn, what = "character", n = 1, skipNul = TRUE)
  
  # u <- readLines(dsn, n = 1, skipNul = TRUE, warn = FALSE)
  u <- readBin(dsn, what = "character", n = 1)
  hd$date <- as.Date(u[1], "%d\\%m\\%y")
  
  invisible(readBin(dsn, what = "character", n = 1))
  u <- readBin(dsn, what = "character", n = 1)
  u <- strsplit(gsub("\005", "", u), " ")[[1]]
  hd$time <- u[1]
  hd$site_text <- u[2]
  
  
  invisible(readBin(dsn, what = "character", n = 5))
  u <- readBin(dsn, what = "character", n = 1)
  hd$time_sweep <- as.numeric(gsub("\002|\005|\n|\004", "", u))
  
  u <- readBin(dsn, what = "character", n = 1)
  hd$depth_scaling <- as.numeric(gsub("\0016|\002|\005|\n|\004", "", u))
  
  u <- readBin(dsn, what = "character", n = 1)
  hd$encoder_div_selection <- as.numeric(gsub("\004|\005|\n|\002", "", u))
  
  u <- readBin(dsn, what = "character", n = 1)
  hd$antsep <- as.numeric(trimStr(gsub("\002|\005|\n|\004", "", u)))
  
  u <- readBin(dsn, what = "character", n = 1)
  hd$unused_zero <- as.numeric(gsub("\002|\005|\n|\004", "", u))
  
  u <- readBin(dsn, what = "character", n = 1)
  hd$splPerScan <- as.numeric(gsub("\002|\005|\n|\004", "", u))
  
  invisible(readBin(dsn, what = "character", n = 1))
  
  u <- readBin(dsn, what = "character", n = 1)
  hd$bits <- as.numeric(gsub("\002|\005|\n|\004", "", u))
  
  .closeFileIfNot(dsn)
  
  return(hd)
}



readUtsiDat <- function(dsn, splPerScan = 512, bits = 16){
  if(!inherits(dsn, "connection")){
    dsn <- file(dsn, 'rb')
  }
  dsn_len <- .flen(dsn)
  # seek(dsn, where = 0, "start")
  # xraw <- readBin(dsn, what = "integer", n = dsn_len, size = 2, endian = "little")
  # close(dsn)
  nr <- splPerScan
  # nc <- length(xraw)/(nr+1+nr)
  nc <- dsn_len/(nr*bits/8 + 1)
  xdata <- matrix(nrow = nr, ncol = nc)
  
  mrkr <- character(nc)
  
  seek(dsn, where = 0, "start")
  for(i in seq_len(nc)){
    xdata[,i] <- readBin(dsn, what = "integer", n = nr, size = bits/8, endian = "little")
    mrkr[i] <- readBin(dsn, what = "character", n = 1, size = 1)
  }
  
  .closeFileIfNot(dsn)
  
  return(list(data = xdata, fid = mrkr))
}

#' @export
readUtsiGPT <- function(dsn){
  if(!inherits(dsn, "connection")){
    dsn <- file(dsn, 'rb')
  }
  gpt <- scan(dsn, quiet = TRUE)
  .closeFileIfNot(dsn)
  return(gpt)
}
# dsn0 <- dsn
# dsn <- dsn[["GPS"]]
# $GPGGA,140454.00,5518.98033,N,00203.66162,W,1,09,1.19,123.0,M,48.6,M,,*48
#' @export
readUtsiGPS <- function(dsn, gpt){
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
  # hCOR <- read.table(textConnection(gsub(",", "\t", readLines(dsn))), 
  #                    dec = ".", header = FALSE, colClasses = "character",
  #                    stringsAsFactors = FALSE)
  hCOR <- read.table(textConnection(content),
                      colClasses = "character",
                      stringsAsFactors = FALSE, 
                      sep = ",")
  a <- getLonLatFromGPGGA(hCOR)
  
  .closeFileIfNot(dsn)
  
  if(nrow(a) == length(gpt)){
    a2 <- cbind(a[,1:3], gpt)
    return(a2)
  }else{
    return(NULL)
  }
  
  
}
