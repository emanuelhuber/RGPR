
.gprSGY <- function(x, fName = character(0), desc = character(0),
                    fPath = character(0), Vmax = NULL){
  
  if(is.null(Vmax)) Vmax <- 50
  
  
  n <- ncol( x$DTR$data)
  data_xyz <- matrix( c(x$DTR$trHD[16+2,], x$DTR$trHD[17+2,], x$DTR$trHD[12,]),
                      nrow = n, ncol = 3, byrow = FALSE)
  data_pos <- posLine(data_xyz)
  
  if(!all(diff(data_pos) > 0)){
    data_pos <- seq_len(n)
    x$BHD$xyz <- data_xyz
    data_xyz <- matrix(nrow = 0, ncol = 0)
    message("No increasing inter-trace distances,\n",
            "I ignore the trace coordinates.\n",
            "Check the coordinates with 'gethd(x)$xyz'")
  }else if(sum(diff(data_pos) == 0)/n > 0.50){
    data_pos <- seq_len(n)
    x$BHD$xyz <- data_xyz
    data_xyz <- matrix(nrow = 0, ncol = 0)
    message("More than 50% of the traces with identical coordinates... strange.\n",
            "I ignore the trace coordinates.\n",
            "Check the coordinates with 'gethd(x)$xyz'")
  }
  
  # check date
  # 22 = year, 23 = day of the year
  if(all(c(x$DTR$trHD[22+2, 1], x$DTR$trHD[23+2, 1]) == 0)){
    data_date <- format(Sys.time(), "%Y-%m-%d")
  }else{
    data_date <- as.character(format(as.Date(x$DTR$trHD[23+2, 1] - 1, 
                                             origin = paste0(x$DTR$trHD[22+2, 1], "-01-01")),
                                     "%Y-%m-%d"))
  }
  
  # check time
  data_time <- seq_len(n)
  
  data_dt <- x$DTR$trHD[21+2, 1]*10^3
  
  antfreq <- 0
  message("Antenna frequency set to 0 MHz. Set it with 'antfreq(x) <- ... '")
  antsep <- 0
  message("Antenna separation set to 0 ", "m", 
          ". Set it with 'antsep(x) <- ... '")
  
  # extract information from textual header data (only if valid encoding)

  data_crs <- character(0)
  # if(all(validEnc(x$THD))){
  k <- verboseF(grepl("epsg:", x$THD, ignore.case = TRUE), verbose = FALSE)
  if(any(k)){
    epsg_code <- regmatches(x$THD[k], regexpr("epsg:[0-9]+" , x$THD[k], ignore.case = TRUE))
    if(length(epsg_code) >= 1){
      epsg_code <- gsub("[^0-9.]", "", epsg_code[1], ignore.case = TRUE)
      data_crs <- paste0("+init=epsg:", epsg_code)
    }
  }
  
  k <- verboseF(grepl("scale factor ", x$THD, ignore.case = TRUE), verbose = FALSE)
  if(any(k)){
    scale_fact <- regexpr("scale factor (?<scale>[0-9.]+)", x$THD[k], 
                          perl = TRUE, ignore.case = TRUE)
    if(length(k) > 1 && scale_fact[1] != -1){
      i1 <- attr(scale_fact, "capture.start")
      i2 <- i1 + attr(scale_fact, "capture.length") -1
      scl <- as.numeric(substr(x$THD[k], i1, i2))
      data_xyz <- data_xyz * scl
    }
  }else if(length(data_xyz) > 0){
    if(all(x$DTR$trHD[17, ] > 0)){
      data_xyz[, 1] <- data_xyz[, 1] / x$DTR$trHD[17, ]
      data_xyz[, 2] <- data_xyz[, 2] / x$DTR$trHD[17, ]
    }    
    if(all(x$DTR$trHD[16, ] > 0))  data_xyz[, 3] <- data_xyz[, 3] / x$DTR$trHD[16, ]
  }
  
  y <- new("GPR",   
           version      = "0.2",
           data        = bits2volt(Vmax = Vmax, nbits = x$BHD$DATA_BYTES) * x$DTR$data,
           traces      = x$DTR$trHD[1,],
           fid         = rep("", n),
           coord       = data_xyz,
           pos         = data_pos,
           depth       = seq(0, by = data_dt, length.out = nrow(x$DTR$data)),
           rec         = matrix(nrow = 0, ncol = 0),
           trans       = matrix(nrow = 0, ncol = 0),
           time0       = rep(0, n),
           time        = data_time,
           proc        = character(0),
           vel         = list(v = 0.1),
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
  
  if( identical(x$DTR$trHD[1, ], x$DTR$trHD[2, ]) ){
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
      message("I return an object of the class 'GPRsurvey' (dsntains many ",
              "GPR lines). ",
              "'plot(x)' will display the position of the traces. ",
              "To plot a single line, use 'plot(x[[1]])'")
      return(Ys)
    }else{
      return(y)
    }
  }
}

#------------------------------------------------------------------------------#

#' Read SEG-Y file
#' 
#' @param dsn data source name: either the filepath to the GPR data (character),
#'            or an open file dsnnection.
#' @param ENDIAN The endian-ness ("big" or "little") of the target system for 
#'               the file. Using "swap" will force swapping endian-ness.
#' 
#' @export
readSGY <- function(dsn, ENDIAN = "big"){
  if( !inherits(dsn, "connection") ){
    dsn <- file(dsn, "rb")
  }
  THD <- readSGY_textual_file_header(dsn, ENDIAN)
  BHD <- readSGY_binary_file_header(dsn, ENDIAN)
  DTR <- readSGY_data_trace(dsn, ENDIAN, 
                            nbytes = BHD$DATA_BYTES, 
                            NB_3200_BYTES = BHD$NB_3200_BYTES, 
                            NB_DATA_TRAILER = BHD$NB_DATA_TRAILER,
                            DATA_FORMAT = BHD$DATA_FORMAT)
  close(dsn)
  return(list(THD = THD, BHD = BHD, DTR = DTR))
}

#------------------------------ SEG-Y FORMAT ----------------------------------#
#---------------------------- TEXTUAL FILE HEADER -----------------------------#
# A reel identification header dsnsisting of 3600 bytes in 2 parts:
# 1. - 3200 (0xC80) bytes of EBCDIC characters representing 40 80-byte
# "card images"
readSGY_textual_file_header <- function(dsn, ENDIAN){
  invisible(seek(dsn, where = 0, origin = "start"))
  # readChar(dsn, nchars = 3200, useBytes = TRUE)
  # readBin(dsn, what = "raw", n = 3200, size = 1, endian = ENDIAN)
  uu <- readBin(dsn, what = character(), n = 1, size = 1, endian = ENDIAN)
  uu <- trimStr(uu)
  uu1 <- verboseF(strsplit(uu, "(C\\s*[0-9]+)", useBytes = TRUE)[[1]], verbose = FALSE)
  if(length(uu1) > 1 && !is.na(uu1)){
    uu1 <- sapply(uu1, trimStr, USE.NAMES = FALSE)
    uu1 <- uu1[uu1!=""]
  }else{
    if(grepl("\\r\\n", uu1)){
      uu1 <- verboseF(strsplit(uu, "\r\n", useBytes = TRUE)[[1]], verbose = FALSE)
    }else if(grepl("\\n", uu1)){
      uu1 <- verboseF(strsplit(uu, "\n", useBytes = TRUE)[[1]], verbose = FALSE)
    }
    if(length(uu1) > 0 && !is.na(uu1)){
      uu1 <- sapply(uu1, trimStr, USE.NAMES = FALSE)
      uu1 <- uu1[uu1!=""]
    }else{
      uu1 <- uu
    }
  }
  return(uu1)
}


#------------------------------ BINARY FILE HEADER ----------------------------#
# 400 (0x190) bytes of binary fixed-point integers, the first 60 bytes
# are assigned, the remaining 340 bytes are unassigned for optional use;
readSGY_binary_file_header <- function(dsn, ENDIAN){
  hd <- c()
  invisible(seek(dsn, where = 3200, origin = "start"))
  # 3201-3204 - Job identification number
  hd$JOB_ID <- readBin(dsn, what = integer(), n = 1, size = 4, endian = ENDIAN)
  # 3205-3208 - Line number
  hd$LINE_NUMBER <-  readBin(dsn, what = integer(), n = 1, size = 4, endian = ENDIAN)
  # 3209-3212 - Reel number
  hd$REEL_NUMBER <-  readBin(dsn, what = integer(), n = 1, size = 4, endian = ENDIAN)
  # 3213-3214 - Number of data traces per ensemble
  hd$NB_DATA_TRACES <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
  # 3215-3216 - Number of auxiliary traces per ensemble
  hd$NB_AUX_TRACES <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
  # 3217-3218 - Sample interval. Microsedsnds (micro-s) for time data, Hertz (Hz) 
  # for frequency data, meters (m) or feet (ft) for depth data.
  hd$TIME_SAMPLING <-  readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
  # 3219-3220 Sample interval of original field recording. Microsedsnds (micro-s) for 
  # time data, Hertz (Hz) for frequency data, meters (m) or 
  #  feet (ft) for depth data.
  hd$TIME_SAMPLING_FIELD <-  readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
  # 3221-3222 - Number of samples per data trace
  hd$NB_SAMPLES <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
  # 3223-3224 - Number of samples per data trace for original field recording.
  hd$NB_SAMPLES_FIELD <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
  
  # 3225-3226 - data sample format code
  # 1 = 4-byte IBM floating-point
  # 2 = 4-byte, two's complement integer
  # 3 = 2-byte, two's complement integer
  # 4 = 4-byte fixed-point with gain (obsolete)
  # 5 = 4-byte IEEE floating-point
  # 6 = 8-byte IEEE floating-point
  # 7 = 3-byte two's complement integer
  # 8 = 1-byte, two's complement integer
  # 9 = 8-byte, two's complement integer
  # 10 = 4-byte, unsigned integer
  # 11 = 2-byte, unsigned integer
  # 12 = 8-byte, unsigned integer
  # 15 = 3-byte, unsigned integer
  # 16 = 1-byte, unsigned integer
  invisible(seek(dsn, where = 3224, origin = "start"))
  hd$DATA_FORMAT_CODE <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
  hd$DATA_FORMAT <- switch(hd$DATA_FORMAT_CODE,
                           "1" = "32-bit IBM floating point",
                           "2" = "32-bit fixed-point",
                           "3" = "16-bit fixed-point",
                           "4" = "16-bit fixed-point with gain code",
                           "5" = "4-byte IEEE floating-point",
                           "6" = "8-byte IEEE floating-point",
                           "7" = "3-byte two's complement integer",
                           "8" = "1-byte, two's complement integer",
                           "9" = "8-byte, two's complement integer",
                           "10" = "4-byte, unsigned integer",
                           "11" = "2-byte, unsigned integer",
                           "12" = "8-byte, unsigned integer",
                           "15" = "3-byte, unsigned integer")
  hd$DATA_BYTES <- switch(hd$DATA_FORMAT_CODE,
                          "1"  = 4,
                          "2"  = 4,
                          "3"  = 2,
                          "4"  = 2,
                          "5"  = 4,
                          "6"  = 8,
                          "7"  = 3,
                          "8"  = 1,
                          "9"  = 8,
                          "10" = 4,
                          "11" = 2,
                          "12" = 8,
                          "15" = 3,
                          "16" = 1)
  
  # 3227-3228 - CDP fold expected per CDP ensemble
  hd$ENSEMBLE_FOLD <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
  # 3229-3230 - Trace sorting code (i.e. type of ensemble) :
  # -1 = Other (should be explained in a user Extended Textual File Header
  #               stanza)
  # 0 = Unknown
  # 1 = As recorded (no sorting)
  # 2 = CDP ensemble
  # 3 = Single fold dsntinuous profile
  # 4 = Horizontally stacked
  # 5 = Common source point
  # 6 = Common receiver point
  # 7 = Common offset point
  # 8 = Common mid-point
  # 9 = Common dsnversion point
  hd$TRACE_SORTING_CODE <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
  # 3231-3232 - Vertical sum code:
  # 1 = no sum,
  # 2 = two sum,
  # ...,
  # N = M-1 sum (M = 2 to 32,767)
  hd$VERTICAL_SUM_CODE <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
  hd$VERTICAL_SUM <- switch(hd$VERTICAL_SUM_CODE ,
                            "1" = "meter",
                            "2" = "feet")
  # 3233-3234 - Sweep frequency at start (Hz).
  hd$SWEEP_FREQ_START <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
  # 3235-3236 - Sweep frequency at end (Hz).
  hd$SWEEP_FREQ_END <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
  # 3237-3238 Sweep length (ms).
  hd$SWEEP_LENGTH <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
  
  
  seek(dsn, where = 3500, origin = "start")
  # 3501 Major SEG-Y Format Revision Number. This is an 8-bit unsigned value. Thus
  # for SEG-Y Revision 2.0, as defined in this document, this will be recorded as
  # 02 16 . This field is mandatory for all versions of SEG-Y, although a value of
  # zero indicates "traditional" SEG-Y dsnforming to the 1975 standard.
  hd$FORMAT_REV_NB <- readBin(dsn, what = integer(), n = 1, size = 1, endian = ENDIAN, signed = FALSE)
  # 3502 Minor SEG-Y Format Revision Number. This is an 8-bit unsigned value with a
  # radix point between the first and sedsnd bytes. Thus for SEG-Y Revision 2.0,
  # as defined in this document, this will be recorded as 00 16 . This field is
  # mandatory for all versions of SEG-Y.
  hd$FORMAT_REV_NB_MINOR <- readBin(dsn, what = integer(), n = 1, size = 1, endian = ENDIAN)
  # 3503-3504 - Fixed length trace flag. A value of one indicates that all traces in this SEG-Y
  # file are guaranteed to have the same sample interval, number of trace header
  # blocks and trace samples, as specified in Binary File Header bytes 3217-3218
  # or 3281-3288, 3517-3518, and 3221-3222 or 3289-3292. A value of zero
  # indicates that the length of the traces in the file may vary and the number of
  # samples in bytes 115-116 of the Standard SEG-Y Trace Header and, if
  # present, bytes 137-140 of SEG-Y Trace Header Extension 1 must be
  # examined to determine the actual length of each trace. This field is mandatory
  # for all versions of SEG-Y, although a value of zero indicates "traditional" SEGY
  # dsnforming to the 1975 standard. Irrespective of this flag, it is strongly
  # recommended that corect values for the number of samples per trace and
  # sample interval appear in the appropriate trace Trace Header locations.
  hd$FIXED_LENGTH_FLAG <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
  # 3505-3506 - Number of 3200-byte, Extended Textual File Header 
  # records following the Binary Header.
  hd$NB_3200_BYTES <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
  # 3507-3510 - Maximum number of additional 240 byte trace headers.
  hd$MAX_NB_240_HEADER <- readBin(dsn, what = integer(), n = 1, size = 4, endian = ENDIAN)
  # 3511-3512 - Time basis code:
  # 1 = Local
  # 2 = GMT (Greenwich Mean Time)
  # 3 = Other, should be explained in a user defined stanza in the 
  # Extended Textual File Header
  # 4 = UTC (Coordinated Universal Time)
  # 5 = GPS (Global Positioning System Time)
  hd$TIME_BASIS_CODE <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
  # 3513-3520Number of traces in this file or stream
  # If zero, all bytes in the file or stream are part of this SEG-Y dataset.
  hd$TRACE_NUMBER <- readBin(dsn, what = integer(), n = 1, size = 8, endian = ENDIAN)
  # Byte offset of first trace relative to start of file or stream if known
  hd$BYTE_OFFSET <- readBin(dsn, what = integer(), n = 1, size = 8, endian = ENDIAN)
  # Number of 3200-byte data trailer stanza records
  hd$NB_DATA_TRAILER <- readBin(dsn, what = integer(), n = 1, size = 4, endian = ENDIAN)
  return(hd)
}

readSGY_data_trace <- function(dsn, ENDIAN, nbytes, NB_3200_BYTES = 0, NB_DATA_TRAILER = 0, DATA_FORMAT){
  start_data_trace <- 3600 + NB_3200_BYTES * 3200
  length_data_trailer <- NB_DATA_TRAILER * 3200
  seek(dsn, where = start_data_trace + 114, origin = "start")
  # number of samples 
  nspls <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
  # sample interval
  # trdt <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)  / (1000 * 1000)
  
  trclen = 240 + nspls * nbytes
  ntrc = (.flen(dsn) - start_data_trace) / trclen
  
  dataSGY <- matrix(nrow = nspls, ncol = ntrc)
  trhd <- matrix(0L, nrow = 29, ncol = ntrc)
  seek(dsn, where = start_data_trace, origin = "start")
  for(i in seq_len(ntrc)){
    # trace sequence number within line
    trhd[1, i] <- readBin(dsn, what = integer(), n = 1, size = 4, endian = ENDIAN)
    # trace sequence number within SEG-Y file 
    trhd[2, i] <- readBin(dsn, what = integer(), n = 1, size = 4, endian = ENDIAN)
    # Original field record number
    trhd[3, i] <- readBin(dsn, what = integer(), n = 1, size = 4, endian = ENDIAN)
    # Trace number within original field record
    trhd[4, i] <- readBin(dsn, what = integer(), n = 1, size = 4,
                          endian = ENDIAN)
    # Energy source point number
    trhd[5, i] <- readBin(dsn, what = integer(), n = 1, size = 4,
                          endian = ENDIAN)
    # CDP ensemble number || CDP = CMP
    trhd[6, i] <- readBin(dsn, what = integer(), n = 1, size = 4,
                          endian = ENDIAN)
    # Trace  number within the ensemble
    trhd[7, i] <- readBin(dsn, what = integer(), n = 1, size = 4,
                          endian = ENDIAN)
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
    trhd[8, i] <- readBin(dsn, what = integer(), n = 1, size = 2,
                          endian = ENDIAN)
    # Stacking: Number of vertically summed traces yielding this trace
    trhd[9, i] <- readBin(dsn, what = integer(), n = 1, size = 2,
                          endian = ENDIAN)
    # Number of horizontally summed traces yielding this trace
    invisible(readBin(dsn, what = integer(), n = 1, size = 2,
                      endian = ENDIAN))
    # data use:
    # 1 = production;
    # 2 = test.
    invisible(readBin(dsn, what = integer(), n = 1L, size = 2,
                      endian = ENDIAN))
    # Distance from center of the source point to the center of the receiver group
    # (negative if opposite to direction in which line is shot).
    trhd[10, i] <-  readBin(dsn, what = integer(), n = 1L, size = 4,
                            endian = ENDIAN)
    # Elevation of receiver group
    trhd[11, i] <- readBin(dsn, what = integer(), n = 1L, size = 4,
                           endian = ENDIAN)
    # Surface elevation at source location.
    trhd[12, i] <- readBin(dsn, what = integer(), n = 1L, size = 4,
                           endian = ENDIAN)
    # Source depth below surface
    trhd[13, i] <- readBin(dsn, what = integer(), n = 1L, size = 4,
                           endian = ENDIAN)
    # Seismic Datum elevation at receiver group
    trhd[14, i] <- readBin(dsn, what = integer(), n = 1L, size = 4,
                           endian = ENDIAN)
    # Seismic Datum elevation at source.
    trhd[15, i] <- readBin(dsn, what = integer(), n = 1L, size = 4,
                           endian = ENDIAN)
    # Water column height at source location
    invisible(readBin(dsn, what = integer(), n = 1L, size = 4,
                      endian = ENDIAN))
    # Water column height at receiver group location
    invisible(readBin(dsn, what = integer(), n = 1L, size = 4,
                      endian = ENDIAN))
    # Scalar to be applied to all elevations and depths
    trhd[16, i] <- readBin(dsn, what = integer(), n = 1L, size = 2,
                      endian = ENDIAN)
    # Scalar to be applied to all coordinates
    trhd[17, i] <- readBin(dsn, what = integer(), n = 1L, size = 2,
                      endian = ENDIAN)
    # Source coordinate - X.
    trhd[18, i] <- readBin(dsn, what = integer(), n = 1L, size = 4,
                           endian = ENDIAN)
    # Source coordinate - Y.
    trhd[19, i] <- readBin(dsn, what = integer(), n = 1L, size = 4,
                           endian = ENDIAN)
    # Group coordinate - X.
    trhd[20, i] <- readBin(dsn, what = integer(), n = 1L, size = 4,
                           endian = ENDIAN)
    # Group coordinate - Y.
    trhd[21, i] <- readBin(dsn, what = integer(), n = 1L, size = 4,
                           endian = ENDIAN)
    invisible(seek(dsn, where = 114 - 88, origin = "current"))
    
    # Number of samples in this trace.
    trhd[22, i] <-readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
    # Sample interval for this trace.
    trhd[23, i] <-readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)  / (1000 * 1000)
    
    invisible(seek(dsn, where = 156 - 118, origin = "current"))
    
    # Year data recorded
    trhd[24, i] <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
    # Day of year
    trhd[25, i] <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
    # Hour of day
    trhd[26, i] <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
    # Minute of hour.
    trhd[27, i] <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
    # Sedsnd of minute.
    trhd[28, i] <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
    
    # Time basis code. If nonzero, overrides Binary File Header bytes 3511-3512.
    # 1 = Local
    # 2 = GMT (Greenwich Mean Time)
    # 3 = Other, should be explained in a user defined stanza in the Extended
    # Textual File Header
    # 4 = UTC (Coordinated Universal Time)
    # 5 = GPS (Global Positioning System Time)
    trhd[29, i] <- readBin(dsn, what = integer(), n = 1, size = 2, endian = ENDIAN)
    
    invisible(seek(dsn, where = 3600 + 240 * i + nspls * nbytes * (i-1), origin = "start"))
    
    if(grepl("float", DATA_FORMAT)){  #  == "4-byte IEEE floating-point"){
      dataSGY[,i] <- readBin(dsn, what = double(), n = nspls, 
                              size = nbytes, endian = ENDIAN, signed = TRUE)
    }else{
      dataSGY[,i] <- readBin(dsn, what = integer(), n = nspls, 
                             size = nbytes, endian = ENDIAN)
    }
  }
  return(list(data = dataSGY, trHD = trhd))
}


