.gprGPR2 <- function(x, fName = character(0), desc = character(0),
                     fPath = character(0), Vmax = NULL){
  if(is.null(Vmax)) Vmax <- 50
  
  ntr <- ncol(x$data)
  
  rec_coord <- matrix(nrow = 0, ncol = 3)
  trans_coord <- matrix(nrow = 0, ncol = 3)
  coordxyz <- matrix(nrow = 0, ncol = 3)
  
  dz <- x$hd$Tall/nrow(x$data)  # ns
  dx <- x$hd$Ddxmm/1000  # m
  xpos <- seq(0, by= dx, length.out = ncol(x$data))
  zdepth <- seq(0, by= dz, length.out = nrow(x$data))
  time_0 <- rep(0, ncol(x$data))
  
  velocity <- 0.1
  posunit <- "m"
  
  antname <- iconv(x$hd$AntenName, from = "windows-1251", to = "UTF-8")
  
  antfreq <- freqFromString(antname)
  if(is.null(antfreq) || is.na(antfreq)){
    antfreq <- 0
    message("Antenna frequency set to 0 MHz. Set it with 'antfreq(x) <- ... '")
  }
  
  antsep <- x$hd$La / 1000
  
  surveymode <- "reflection"
  
  stime <- Cfiletime(x$hd$CreateTime)
  
  
  
  new("GPR",   
      version     = "0.2",
      data        = bits2volt(Vmax = Vmax)*x$data,
      traces      = 1:ncol(x$data),
      fid         = rep("", ncol(x$data)),
      coord       = coordxyz,
      pos         = xpos,
      depth       = zdepth,
      rec         = rec_coord,
      trans       = trans_coord,
      time0       = time_0,
      time        = rep(0, ncol(x$data)),
      proc        = character(0),
      vel         = list(v = velocity),                  # m/ns
      name        = fName,
      description = desc,
      filepath    = fPath,
      dz          = dz,  
      dx          = dx,
      depthunit   = "ns",
      posunit     = "m",
      freq        = antfreq[1], 
      antsep      = antsep[1], 
      surveymode  = surveymode[1],
      date        = as.character(as.Date(stime)),
      crs         = character(0),
      hd          = x$hd                      # header
  )
}



readGPR2 <- function(dsn){
  # if(!inherits(dsn, "connection")){
  #   dsn <- file(dsn, 'rb')
  # }
  # on.exit(close(dsn))
  dsn <- .openFileIfNot(dsn)  # in case there is some binary stuff
  header <- gpr2_readHeader(dsn)
  arrays <- grp2_readArrays(dsn, header[["NSamples"]])
  traces <- gpr2_readTraces(dsn, nspl = header[["NSamples"]], ntr = header[["NTraces"]])
  # plot3D::image2D(traces$gpr)
  .closeFileIfNot(dsn)
  return(list(hd = header, arrays = arrays, data = traces$gpr, trhd = traces$HD))
}


# source https:#rdrr.io/cran/readABF/src/R/readABF.R
# header <- list()
# auxiliary functions for reading certain byte sequences
skip <- function (dsn, n) invisible(readBin(dsn, "raw", n=n))
bool <- function (dsn, n=1) as.logical(readBin(dsn, n=n, "integer", size=1,
                                               endian="little"))
int8 <- function (dsn, n=1) readBin(dsn, n=n, "integer", size=1, endian="little")
int16 <- function (dsn, n=1) readBin(dsn, n=n, "integer", size=2, endian="little")
uint16 <- function (dsn, n=1) readBin(dsn, n=n, "integer", size=2, signed = FALSE, endian="little")
int32 <- function (dsn, n=1) readBin(dsn, n=n, "integer", size=4, endian="little")
int64 <- function (dsn, n=1) readBin(dsn, n=n, "integer", size=8, endian="little")
uint32 <- function (dsn, n=1) {
  # you would think:
  # readBin(dsn, n=n, "integer", size=4, signed=FALSE, endian="little")
  # but R can't do that
  # instead we do what the author of the abf2 package did:
  # we combine two unsigned integers
  result <- c()
  for (i in 1:n) { # we can assume that n is never 0
    lo <- readBin(dsn, "integer", size=2, signed=FALSE, endian="little")
    hi <- readBin(dsn, "integer", size=2, signed=FALSE, endian="little")
    result[i] <- 65536*hi+lo
  }
  result
}
uint64 <- function (dsn, n=1) {
  # you would think:
  # readBin(dsn, n=n, "integer", size=4, signed=FALSE, endian="little")
  # but R can't do that
  # instead we do what the author of the abf2 package did:
  # we combine two unsigned integers
  result <- c()
  for (i in 1:n) { # we can assume that n is never 0
    lo <- uint32(dsn)
    hi <- uint32(dsn)
    result[i] <- (2^32)*hi+lo
  }
  result
}
float32 <- function (dsn, n=1) readBin(dsn, "double", n=n, size=4, endian="little")
double64 <- function (dsn, n=1) readBin(dsn, "double", n=n, size=8, endian="little")
char <- function (dsn, n=1) readChar(dsn, n, useBytes=FALSE)
chararr <- function (dsn, n=1) readChar(dsn, rep(n, 16), useBytes=TRUE)
# `%:%` <- function (a, b) header[[a]] <<- b()
# `%x%` <- function (dsn, n) function () dsn(n)
rawn <- function(dsn, n=4){
  # print(paste0("**** ", n, " ****"))
  readBin(dsn, what = "raw", n = n, size = 1, endian = "little")
}


char <- function(dsn, n=1) paste0(rawToChar(readBin(dsn, "raw", n = n), multiple = TRUE), collapse = "")

Cfiletime <- function(x){
  as.POSIXct( x / 1e7 - 11644473600, origin = "1970-01-01", tz = "UTC")
}


gpr2_readHeader <- function(dsn){
  header <- list()
  seek(dsn, where = 0, origin = "start")
  nchar <- 40
  header_config <- list(
    "label"             = "rawn",          # Mark (lab (=0xfedcba98)
    "IDVersion"         = "uint32",      # File version number
    "MainNumber"        = "uint32",     # Profile number
    "SerNumber"         = "uint32",      # Series number
    "ProfSerNumber"     = "uint32",  # Profile series number
    "State"             = "uint32",          # State
    "NTraces"           = "uint32",        # Number of traces in the file of the profile
    "NSamples"          = "uint32",       # Number of points at the trace
    "NTextLabels"       = "uint32",    # Number of text points at the file of the profile
    "Tall"              = "uint32",           # Length of the trace [nanoseconds]
    "Eps"               = "float32",           # Espilon
    "Ddxmm"             = "int32",           # Step across X axis, mm
    "StartPosition"     = "int64",   # Start coordinate [mm]
    "StartX"            = "int64",          # Start Coordinate X(absolute)[mm]
    # "dStartX"           = "double64",          # Start Coordinate X(absolute)[mm]
    "StartY"            = "int64",          # Start Coordinate X(absolute)[mm]
    # "dStartY"           = "double64",          # Start Coordinate X(absolute)[mm]
    "StartZ"            = "int64",          # Start Coordinate X(absolute)[mm]
    # "dStartZ"           = "double64",          # Start Coordinate X(absolute)[mm]
    "CreateTime"        = "uint64", #c("rawn", 8),      # Creation time POSIX epoch = Jan 1, 1970
    "ManipulationTime"  = "uint64",
    "La"                = "uint32",               # Basis between antennas, [mm]
    "Tstart"            = "int32",               # Start time of the receiver
    # (quantity of the points that are cut above)
    # (>0 after cutting from above)
    "Tspp"              = "int32",              # Signal position of the direct passing
    "SppThreshold"      = "float32",    # Threshold quantity to define ???
    "Kraz"              = "float32",            # Max ratio of racing
    "WinSize"           = "uint32",          # Window depth balancing
    "HorWinSize"        = "uint32",    # Window wideness balancing
    "WhiteProcent"      = "int32",   # Negative boundary rangeability of the color
    "BlackProcent"      = "int32",    # Positive boundary rangeability of the color
    "ScanMode"          = "uint32",         #Scanning mode (Acc. with the step, with the time and e.t.c.)
    "NSum"              = "uint32",   # Accumulation coefficient (soft*hard)
    "NTpz"              = "uint32",    # Shift (PTZ0*8+PTZ1*256)
    "AntenName"         = c("char", nchar),      # Title of the antenna unit
    "Operator"          = c("char", nchar),       # Operational user
    "Object"            = c("char", nchar),          # scanning object
    "Tips1"             = c("char", nchar),       # comments 1
    "Tips2"             = c("char", nchar),         # comments 2
    "Tips3"             = c("char", nchar),          # comments 3
    "UserNumber"        = "int64",                  # User ID number, the generated profile,
    "LastUserNumber"    = "int64",                      # User ID number of the changed profile,
    "ZeroZone"          = "int32",                            # Dead zone of changing color
    "ShiftProcent"      = "int32",                       # Shift changing in color (whilte balance)
    "FirstLabelNumber"  = "uint32",  
    "AB2Shift"          = "int32",  
    "sCursAngl"         = "int16",
    "nAntSerial"        = "uint32",  
    "dwPVersionMS"      = "uint32",
    "dwPVersionLS"      = "uint32",
    "AntenID"           = "uint16",
    "DeviceID"          = "uint16",
    "State2"            = "uint32",
    "iHeightAboveRoad"  = "uint16",
    "fTall"             = "float32")
  
  fun <- function(x, dsn){
    if(length(x) > 1){
      get(x[1])(dsn, as.integer(x[2]))
    }else{
      get(x[1])(dsn)
    }
  }
  header <- lapply(header_config, fun, dsn)
  # header <- lapply(header_config, function(x, dsn) get(x[1])(dsn, x[-1]), dsn)
  # skip(23)
  
  return(header)
}



grp2_readArrays <- function(dsn, nspl){
  seek(dsn, where = 512, origin = "start")
  list(
    "ColorArray" = rawn(dsn, n=256*4),
    "Equalisation_factor_array" = float32(dsn, nspl)
  )        
}

gpr2_readTraces <- function(dsn, nspl, ntr){
  # rTime <- rep(ntr)
  # pos <- rep(ntr)
  # x <- rep(ntr)
  # y <- rep(ntr)
  # z <- rep(ntr)
  # iAnt <- rep(ntr)
  # labelID <- rep(ntr)
  # labelpos <- rep(ntr)
  # ScShotID <- rep(ntr)
  # PictureID <- rep(ntr)
  # LabelClr <- rep(ntr)
  # fid <- rep(ntr)
  A <- matrix(nrow = nspl, ncol = ntr)
  tags <- list(
    "rTime" = "int64",
    "pos" = "int32",
    "x" = "int32",
    "y" = "int32",
    "z" = "int32",
    "iAnt" = "int32",
    "labelID" = "uint32",
    "labelpos" = "uint32")
  HDT <- list()
  for(i in seq_len(ntr)){
    for(j in seq_along(tags)){
      HDT[[names(tags)[j]]][i] <- get(tags[[j]])(dsn)
    }
    # rTime[i] <- int64()
    # pos[i] <- int32()
    # x[i] <- int32()
    # y[i] <- int32()
    # z[i] <- int32()
    # iAnt[i] <- int32()
    # labelID[i] <- uint32()
    # labelpos[i] <- uint32()
    # # ScShotID[i] <- uint32()
    # # PictureID[i] <- uint32()
    A[,i] <- float32(dsn, nspl)
  }
  return(list(HD = HDT, gpr = A))
}

