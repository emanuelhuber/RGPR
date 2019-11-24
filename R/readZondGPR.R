

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
      data = bits2volt(Vmax = Vmax)*x$data,
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

# Prism2 ” software
#--------------- read RadSys Zond GPR device files -------------------#
readSEGY_RadSys_Zond_GPR <- function(dsn){
  #dirName     <- dirname(fPath)
  #baseName    <- .fNameWExt(fPath)
  #fName    <- file.path(dirName, paste0(baseName, ".sgy"))
  # fName <- getFName(fPath, ext = c(".sgy"))
  hd <- c()
  # con <- file(fName$sgy , "rb")
  
  if(is.character(dsn)){
    fName <- getFName(dsn, ext = c(".sgy"))
    # open dt1 file
    dsn <- file(fName$sgy , "rb")
  }
  
  ##---- SEGY file
  uu <- readBin(dsn, what = character(), n = 1, size = 1)
  vv <- strsplit(uu, split ="\r\n", perl = TRUE)
  hd$EBCDIC <- sub("\\s+$", "", vv[[1]])
  invisible(seek(dsn, where = 3200, origin = "start"))
  # Job identification number
  hd$JOB_ID <- readBin(dsn, what = integer(), n = 1, size = 4)
  # Line number
  hd$LINE_NUMBER <-  readBin(dsn, what = integer(), n = 1, size = 4)
  # Reel number
  hd$REEL_NUMBER <- readBin(dsn, what = integer(), n = 1, size = 4)
  # Number of data traces per record
  hd$NB_DATA_TRACES <- readBin(dsn, what = integer(), n = 1, size = 2)
  # Number of auxiliary traces per record
  hd$NB_AUX_TRACES <- readBin(dsn, what = integer(), n = 1, size = 2)
  # tspl > Sample interval of this reel's data in PICOseconds
  # hd$TIME_SAMPLING in nanoseconds
  hd$TIME_SAMPLING <-  readBin(dsn, what = integer(), n = 1, size = 2) * 1e-3
  # Number of samples per trace for this reel's data
  invisible(readBin(dsn, what = integer(), n = 1, size = 2))
  # samples per trace for this reel's data
  # Nspl
  hd$NB_SAMPLES <- readBin(dsn, what = integer(), n = 1, size = 2)
  #unused
  invisible(readBin(dsn, what = integer(), n = 1, size = 2))
  # data sample format code
  # 1 = 32-bit IBM floating point;
  # 2 = 32-bit fixed-point (integer);
  # 3 = 16-bit fixed-point (integer);
  # 4 = 16-bit fixed-point with gain code 
  hd$DATA_FORMAT_CODE <- readBin(dsn, what = integer(), n = 1, size = 2)
  hd$DATA_FORMAT <- switch(hd$DATA_FORMAT_CODE,
                           "1" = "32-bit IBM floating point",
                           "2" = "32-bit fixed-point",
                           "3" = "16-bit fixed-point",
                           "4" = "16-bit fixed-point with gain code")
  hd$DATA_BYTES <- switch(hd$DATA_FORMAT_CODE,
                          "1"  = 4,
                          "2"  = 4,
                          "3"  = 2,
                          "4"  = 2)
  #number of traces per ensemble
  invisible(readBin(dsn, what = integer(), n = 1, size = 2))
  # not used
  invisible(readBin(dsn, what = integer(), n = 13, size = 2))
  # mesuring system
  # 1 = meters
  # 2 = feet
  pos_unit <- readBin(dsn, what = integer(), n = 1, size = 2)
  hd$POS_UNIT <- switch(pos_unit,
                        "1" = "meter",
                        "2" = "feet")
  # not used
  invisible(readBin(dsn, what = integer(), n = 172, size = 2))
  
  # 240-byte binary tracer header + trace data
  hd$NB_TRACES <- (.flen(dsn) - seek(dsn))/(240 + hd$NB_SAMPLES * hd$DATA_BYTES)
  dataSGY <- matrix(nrow = hd$NB_SAMPLES, ncol = hd$NB_TRACES)
  hdt <- matrix(nrow = 7, ncol = hd$NB_TRACES)
  xyfac <- numeric(hd$NB_TRACES)
  for(i in 1:hd$NB_TRACES){
    #--------------------------#
    #--------- header ---------#
    # trace sequence number within line
    invisible(readBin(dsn, what = integer(), n = 1, size = 4, 
                      endian = "little"))
    # not used
    invisible(readBin(dsn, what = integer(), n = 1, size = 4, 
                      endian = "little"))
    # Original field record number
    invisible(readBin(dsn, what = integer(), n = 1, size = 4, 
                      endian = "little"))
    # Trace sequence number within original field record
    invisible(readBin(dsn, what = integer(), n = 1, size = 4, 
                      endian = "little"))
    # not used
    invisible(readBin(dsn, what = integer(), n = 1, size = 4, 
                      endian = "little"))
    # CDP ensemble number || CDP = CMP
    invisible(readBin(dsn, what = integer(), n = 1, size = 4, 
                      endian = "little"))
    # Trace sequence number within CDP ensemble
    invisible(readBin(dsn, what = integer(), n = 1, size = 4, 
                      endian = "little"))
    # Trace identification code:
    # 1 = seismic data;
    # 2 = dead;
    # 3 = dummy;
    # 4 = time break;
    # 5 = uphole;
    # 6 = sweep;
    # 7 = timing;
    # 8 = water break;
    # 9 = optional use
    invisible(readBin(dsn, what = integer(), n = 1, size = 2, 
                      endian = "little"))
    # Number of vertically summed traces yielding this trace
    invisible(readBin(dsn, what = integer(), n = 1, size = 2, 
                      endian = "little"))
    # Number of horizontally summed traces yielding this trace
    invisible(readBin(dsn, what = integer(), n = 1, size = 2, 
                      endian = "little"))
    # data use:
    # 1 = production;
    # 2 = test.
    invisible(readBin(dsn, what = integer(), n = 1L, size = 2, 
                      endian = "little"))
    # not used
    invisible(readBin(dsn, what = integer(), n = 1L, size = 4, 
                      endian = "little"))
    # Altitude (mean-sea-level)
    invisible(readBin(dsn, what = numeric(), n = 1L, size = 4, 
                      endian = "little"))
    # Height of geoid above WGS84 ellipsoid
    invisible(readBin(dsn, what = numeric(), n = 1L, size = 4, 
                      endian = "little"))
    # Backward/toward direction (if negative -backward)
    invisible(readBin(dsn, what = integer(), n = 1, size = 4, 
                      endian = "little"))
    # Datum elevation at source in m (topography offset)
    invisible(readBin(dsn, what = numeric(), n = 1L, size = 4, 
                      endian = "little"))
    # not used
    invisible(readBin(dsn, what = integer(), n = 7L, size = 2, 
                      endian = "little"))
    # Scalar for coordinates:
    # + = multiplier; 
    # –= divisor.
    xyfac[i] <- readBin(dsn, what = integer(), n = 1L, size = 2, 
                        endian = "little")
    # X source coordinate (Longitude in 32-bit float accuracy for arc seconds)
    invisible(readBin(dsn, what = integer(), n = 1L, size = 4, 
                      endian = "little"))
    # Y source coordinate (Longitude in 32-bit float accuracy for arc seconds)
    invisible(readBin(dsn, what = integer(), n = 1L, size = 4, 
                      endian = "little"))
    # X receiver group coordinate
    hdt[4,i] <- readBin(dsn, what = integer(), n = 1L, size = 4, 
                        endian = "little")
    # Y receiver group coordinate
    hdt[5,i] <- readBin(dsn, what = integer(), n = 1L, size = 4, 
                        endian = "little")
    # Coordinate units:
    # 1 = length in meters or feets; 
    # 2 = arc seconds (DDMM.SSSS).
    invisible(readBin(dsn, what = integer(), n = 1L, size = 2, 
                      endian = "little"))
    # GPS signal quality
    invisible(readBin(dsn, what = numeric(), n = 1L, size = 4, 
                      endian = "little"))
    # not used
    invisible(readBin(dsn, what = integer(), n = 7L, size = 2, 
                      endian = "little"))
    # Lag time between shot and recording start in PICOseconds
    invisible(readBin(dsn, what = integer(), n = 1L, size = 2, 
                      endian = "little"))
    # not used
    invisible(readBin(dsn, what = numeric(), n = 1L, size = 4, 
                      endian = "little"))
    # Number of samples in this trace = hd$NB_SAMPLES
    invisible(readBin(dsn, what = integer(), n = 1L, size = 2, 
                      endian = "little"))
    # Sample interval of this reel's data in PICOseconds 
    # = hd$TIME_SAMPLING *1e3
    invisible(readBin(dsn, what = integer(), n = 1L, size = 2, 
                      endian = "little"))
    # not used
    invisible(readBin(dsn, what = integer(), n = 21L, size = 2, 
                      endian = "little"))
    # Hour of day (24 hour clock)
    hdt[1,i] <- readBin(dsn, what = integer(), n = 1L, size = 2, 
                        endian = "little")
    # Minute of hour
    hdt[2,i] <- readBin(dsn, what = integer(), n = 1L, size = 2, 
                        endian = "little")
    # Second of minute
    hdt[3,i] <- readBin(dsn, what = integer(), n = 1L, size = 2, 
                        endian = "little")
    # Time basis code (1 –Local, 2 -GMT)
    invisible(readBin(dsn, what = integer(), n = 1L, size = 2, 
                      endian = "little"))
    # not used
    invisible(readBin(dsn, what = integer(), n = 7L, size = 2, 
                      endian = "little"))
    # Longitude in 64-bit double accuracy
    invisible(readBin(dsn, what = numeric(), n = 1L, size = 8, 
                      endian = "little"))
    # Latitude in 64-bit double accuracy
    invisible(readBin(dsn, what = numeric(), n = 1L, size = 8, 
                      endian = "little"))
    # not used
    invisible(readBin(dsn, what = integer(), n = 8L, size = 2, 
                      endian = "little"))
    # Time scalar. If positive, scalar is used as a 
    # multiplier. If negative –divisor.
    invisible(readBin(dsn, what = integer(), n = 1L, size = 2, 
                      endian = "little"))
    # not used
    invisible(readBin(dsn, what = integer(), n = 10L, size = 2, 
                      endian = "little"))
    # Marks indicator. If equal to 0x5555, trace is marked.
    hdt[6,i] <- readBin(dsn, what = integer(), n = 1L, size = 2, 
                        endian = "little")
    # Mark number.
    hdt[7,i] <- readBin(dsn, what = integer(), n = 1L, size = 2, 
                        endian = "little") 
    #---------- trace ---------------#
    if(hd$DATA_FORMAT_CODE == 1){
      # "32-bit IBM floating point"
      dataSGY[,i] <- readBin(dsn, what = numeric(), n = hd$NB_SAMPLES, 
                             size = hd$DATA_BYTES, endian = "little")
    }else{
      dataSGY[,i] <- readBin(dsn, what = integer(), n = hd$NB_SAMPLES,
                             size = hd$DATA_BYTES, endian = "little")
    }
  }
  hdt[4,] <- hdt[4,] * abs(xyfac)^sign(xyfac)
  
  
  close(dsn)
  
  # byte to volt conversion assuming
  # recording range: [-50mV, 50mV]
  # 16 bytes
  #V <- c(-50,50)
  #nBytes <- 16
  #A2 <- A*abs(diff(V))/(2^nBytes)
  
  # time sample in nanosecond
  #tt <- seq(0, by = hd$TIME_SAMPLING, length.out = hd$NB_SAMPLES)
  
  # hdt matrix
  # row 1 = hour
  # row 2 = minute
  # row 3 = seconde
  # row 4 = x-pos
  # row 5 = y-pos
  # row 6 = mark indicator
  # row 7 = mark number
  return(list(hd = hd, data = dataSGY, hdt = hdt))
}
