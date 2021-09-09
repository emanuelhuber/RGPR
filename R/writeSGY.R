


# 
# flen <- function(con){
#   pos0 <- seek(con)
#   seek(con,0,"end")
#   pos <- seek(con)
#   seek(con,where=pos0,"start")
#   return(pos)
# }
# 
# library(RGPR)
# x <- frenkeLine00
# x@pos * 1000
# 
# THD <- "A reel identification header consisting of 3600 bytes in 2 parts:"
# 
# dsn <- "test.sgy"
# dsn <- file(dsn, "wb")
# writeSGY_textual_file_header(dsn, THD)
# writeSGY_binary_file_header(dsn, x)
# writeSGY_binary_trace_header(dsn, x)
# 
# flen(dsn)
# close(dsn)
# 
# dsn <- "test.sgy"
# x <- readSGY(dsn, ENDIAN = "little")
# y <- readGPR(dsn)
# plot(y)
# plot(y@coord)
# plot(y@pos)
# 
# dsn <- file(dsn, "rb")
# 
# 
# u <- readSGY(dsn, ENDIAN = "little")
# names(u)
# 
# u$DTR$trHD[16,]
# u$DTR$trHD[17,]
# u$DTR$trHD[12,]


.writeSGY <- function(x, fPath, endian = "little"){
  dsn <- file(fPath, "wb")
  x@data[!is.finite(x@data)] <- 0
  x@data <-  round( (x@data - min(x@data))/(diff(range(x@data))) * 
                      (32767 + 32768) - 32768 )
  storage.mode(x@data) <- "integer"
  THD <- "Export by RGPR"
  writeSGY_textual_file_header(dsn, THD, endian = endian)
  writeSGY_binary_file_header(dsn, x, endian = endian)
  writeSGY_binary_trace_header(dsn, x, endian = endian)
  close(dsn)
  
}

# A reel identification header consisting of 3600 bytes in 2 parts:
# 1. - 3200 (0xC80) bytes of EBCDIC characters representing 40 80-byte
# "card images"
# .Platform$endian
writeSGY_textual_file_header <- function(dsn, THD , endian = "little"){
  u <- charToRaw(THD)
  n <- length(u)
  if(n > 3200){
    u <- u[1:3200]
  }else{
    u <- c(u , raw(3200 - n))
  }
  writeBin(u, dsn, size =1, endian = endian, useBytes = FALSE)
  # .flen(dsn)
}



#------------------------------ BINARY FILE HEADER ----------------------------#
# 400 (0x190) bytes of binary fixed-point integers, the first 60 bytes
# are assigned, the remaining 340 bytes are unassigned for optional use;
writeSGY_binary_file_header <- function(dsn, x,  endian = "little"){
  
  bindata <- data.frame(tag = rep("", 10),
                        # what = "integer",
                        size = rep(4, 10),
                        val = 0L)
  # str(bindata)
  
  # # 3201-3204 - Job identification number
  bindata[1, ] <- list("JOB_ID", 4, 0L)
  # 3205-3208 - Line number
  bindata[2, ] <- list("LINE_NUMBER", 4, 0L)
  # 3209-3212 - Reel number
  bindata[3, ] <- list("REEL_NUMBER", 4, 0L)
  # 3213-3214 - Number of data traces per ensemble
  bindata[4, ] <- list("NB_DATA_TRACES", 2, 0L)
  # 3215-3216 - Number of auxiliary traces per ensemble
  bindata[5, ] <- list("NB_AUX_TRACES", 2, 0L)
  # 3217-3218 - Sample interval. Microseconds (micro-s) for time data, Hertz (Hz) 
  # for frequency data, meters (m) or feet (ft) for depth data.
  # 
  splint <- as.integer(round(diff(range(x@depth))))   # keep in nanosecond
  bindata[6, ] <- list("TIME_SAMPLING", 2, splint)
  # 3219-3220 Sample interval of original field recording. Microseconds (micro-s) for 
  # time data, Hertz (Hz) for frequency data, meters (m) or 
  #  feet (ft) for depth data.
  bindata[7, ] <- list("TIME_SAMPLING_FIELD", 2, 0L)
  # 3221-3222 - Number of samples per data trace
  bindata[8, ] <- list("NB_SAMPLES", 2, nrow(x))
  # 3223-3224 - Number of samples per data trace for original field recording.
  bindata[9, ] <- list("NB_SAMPLES_FIELD", 2, 0L)
  
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
  # invisible(seek(con, where = 3224, origin = "start"))
  # hd$DATA_FORMAT_CODE <- readBin(con, what = integer(), n = 1, size = 2, endian = ENDIAN)
  bindata[10, ] <- list("DATA_FORMAT_CODE", 2, 3L)
  # hd$DATA_FORMAT <- switch(hd$DATA_FORMAT_CODE,
  #                          "1" = "32-bit IBM floating point",
  #                          "2" = "32-bit fixed-point",
  #                          "3" = "16-bit fixed-point",
  #                          "4" = "16-bit fixed-point with gain code",
  #                          "5" = "4-byte IEEE floating-point",
  #                          "6" = "8-byte IEEE floating-point",
  #                          "7" = "3-byte two's complement integer",
  #                          "8" = "1-byte, two's complement integer",
  #                          "9" = "8-byte, two's complement integer",
  #                          "10" = "4-byte, unsigned integer",
  #                          "11" = "2-byte, unsigned integer",
  #                          "12" = "8-byte, unsigned integer",
  #                          "15" = "3-byte, unsigned integer")
  # hd$DATA_BYTES <- switch(hd$DATA_FORMAT_CODE,
  #                         "1"  = 4,
  #                         "2"  = 4,
  #                         "3"  = 2,
  #                         "4"  = 2,
  #                         "5"  = 4,
  #                         "6"  = 8,
  #                         "7"  = 3,
  #                         "8"  = 1,
  #                         "9"  = 8,
  #                         "10" = 4,
  #                         "11" = 2,
  #                         "12" = 8,
  #                         "15" = 3,
  #                         "16" = 1)
  # 
  # 3227-3228 - CDP fold expected per CDP ensemble
  # hd$ENSEMBLE_FOLD <- readBin(con, what = integer(), n = 1, size = 2, endian = ENDIAN)
  bindata[11, ] <- list("ENSEMBLE_FOLD", 2, 1L)
  # 3229-3230 - Trace sorting code (i.e. type of ensemble) :
  # -1 = Other (should be explained in a user Extended Textual File Header
  #               stanza)
  # 0 = Unknown
  # 1 = As recorded (no sorting)
  # 2 = CDP ensemble
  # 3 = Single fold continuous profile
  # 4 = Horizontally stacked
  # 5 = Common source point
  # 6 = Common receiver point
  # 7 = Common offset point
  # 8 = Common mid-point
  # 9 = Common conversion point
  bindata[12, ] <- list("TRACE_SORTING_CODE", 2, 0L)
  # hd$TRACE_SORTING_CODE <- readBin(con, what = integer(), n = 1, size = 2, endian = ENDIAN)
  # 3231-3232 - Vertical sum code:
  # 1 = no sum,
  # 2 = two sum,
  # ...,
  # N = M-1 sum (M = 2 to 32,767)
  bindata[13, ] <- list("VERTICAL_SUM_CODE", 2, 0L)
  # hd$VERTICAL_SUM_CODE <- readBin(con, what = integer(), n = 1, size = 2, endian = ENDIAN)
  # hd$VERTICAL_SUM <- switch(hd$VERTICAL_SUM_CODE ,
  #                           "1" = "meter",
  #                           "2" = "feet")
  # 3233-3234 - Sweep frequency at start (Hz).
  bindata[14, ] <- list("SWEEP_FREQ_START", 2, 0L)
  # hd$SWEEP_FREQ_START <- readBin(con, what = integer(), n = 1, size = 2, endian = ENDIAN)
  # 3235-3236 - Sweep frequency at end (Hz).
  bindata[15, ] <- list("SWEEP_FREQ_END", 2, 0L)
  # hd$SWEEP_FREQ_END <- readBin(con, what = integer(), n = 1, size = 2, endian = ENDIAN)
  # 3237-3238 Sweep length (ms).
  bindata[16, ] <- list("SWEEP_LENGTH", 2, 0L)
  # hd$SWEEP_LENGTH <- readBin(con, what = integer(), n = 1, size = 2, endian = ENDIAN)
  # 3239–3240
  # Sweep type code:
  #     1 = linear
  #     2 = parabolic
  #     3 = exponential
  #     4 = other
  bindata[17, ] <- list("SWEEP_TYPE_CODE", 2, 0L)
  # 3241-3242 > Trace number of sweep channel.
  bindata[18, ] <- list("TRACE_NB_SWEEP_CHANNEL", 2, 0L)
  # 3243–3244 > Sweep trace taper length in milliseconds at start
  bindata[19, ] <- list("SWEEP_TRACE_TAPER_START", 2, 0L)
  # 3245–3246 > Sweep trace taper length in milliseconds at end
  bindata[20, ] <- list("SWEEP_TRACE_TAPER_END", 2, 0L)
  # 3247–3248 > Taper type:
  #                 1 = linear
  #                 2 = cosine squared
  #                 3 = other
  bindata[21, ] <- list("TAPER_TYPE", 2, 0L)
  # 3249–3250 Correlated data traces:
  #               1 = no
  #               2 = yes
  bindata[22, ] <- list("CORRELATED_DATA_TRACES", 2, 0L)
  # 3251–3252 Binary gain recovered:
  #                 1 = yes
  #                 2 = no
  bindata[23, ] <- list("BINARY_GAIN_RECOVERED", 2, 0L)
  # 3253–3254 Amplitude recovery method:
  #               1 = none
  #               2 = spherical divergence
  #               3 = AGC
  #               4 = other
  bindata[24, ] <- list("AMPLITUDE_RECOVERY_METHOD", 2, 0L)
  # 3255–3256 Measurement system:
  #             1 = Meters
  #             2 = Feet
  bindata[25, ] <- list("MEASUREMENT_SYSTEM", 2, 0L)
  # 3257–3258 Impulse signal polarity
  # 1 = Increase in pressure or upward geophone case movement gives negative
  # number on trace.
  # 2 = Increase in pressure or upward geophone case movement gives positive
  # number on trace.
  bindata[26, ] <- list("IMPULSE_SIGNAL_POLARITY", 2, 0L)
  
  # 3259–3260 Vibratory polarity code:
  bindata[27, ] <- list("VIBRATORY_POLARITY_CODE", 2, 0L)
  
  # 3261–3264 Extended number of data traces per ensemble. If nonzero, this overrides the
  # number of data traces per ensemble in bytes 3213–3214.
  bindata[28, ] <- list("EXTENDED_NB_TRACES_PER_ENSEMBLE", 4, 0L)
  # 3265–3268 Extended number of auxiliary traces per ensemble. If nonzero, this overrides
  # the number of auxiliary traces per ensemble in bytes 3215–3216.
  bindata[29, ] <- list("EXTENDED_NB_AUX_TRACES_PER_ENSEMBLE", 4, 0L)
  # 3269–3272 Extended number of samples per data trace. If nonzero, this overrides the
  # number of samples per data trace in bytes 3221–3222.
  bindata[30, ] <- list("EXTENDED_NB_SAMPLES_PER_TRACE", 4, 0L)
  # 3273–3280 Extended sample interval, IEEE double precision (64-bit). If nonzero, this
  # overrides the sample interval in bytes 3217–3218 with the same units.
  bindata[31, ] <- list("EXTENDED_SAMPLES_INTERVAL", 8, 0L)
  # bindata[32, ] <- list("EXTENDED_SAMPLES_INTERVAL2", 4, 0L)
  
  # 3281–3288 Extended sample interval of original field recording, IEEE double precision 
  # (64-bit) . If nonzero, this overrides the sample interval of original field recording in
  # bytes 3219–3220 with the same units.
  bindata[32, ] <- list("EXTENDED_SAMPLES_INTERVAL_ORIG", 8, 0L)
  # bindata[34, ] <- list("EXTENDED_SAMPLES_INTERVAL_ORIG2", 4, 0L)
  # 3289–3292 Extended number of samples per data trace in original recording. If nonzero,
  # this overrides the number of samples per data trace in original recording in
  # bytes 3223–3224.
  bindata[33, ] <- list("EXTENDED_NB_SAMPLES_PER_TRACE", 4, 0L)
  # 3293–3296 Extended ensemble fold. If nonzero, this overrides ensemble fold in bytes
  # 3227–3228.
  bindata[34, ] <- list("EXTENDED_ENSEMBLE_FOLD", 4, 0L)
  # 3297–3300
  # The integer constant 16909060 10 (01020304 16 ). This is used to allow
  # unambiguous detection of the byte ordering to expect for this SEG-Y file. For
  # example, if this field reads as 67305985 10 (04030201 16 ) then the bytes in every
  # Binary File Header, Trace Header and Trace Data field must be reversed as
  # they are read, i.e. converting the endian-ness of the fields. If it reads
  # 33620995 10 (02010403 16 ) then consecutive pairs of bytes need to be swapped
  # in every Binary File Header, Trace Header and Trace Data field.
  # The byte ordering of all other portions (the Extended Textual Header and Data
  #                                          Trailer) of the SEG-Y file is not affected by this field.
  bindata[35, ] <- list("INTEGER_CST", 4, 0L)
  # 3301–3500
  # Unassigned
  bindata[36, ] <- list("UNASSIGNED",  200, 0L)
  # 3501 Major SEG-Y Format Revision Number. This is an 8-bit unsigned value. Thus
  # for SEG-Y Revision 2.0, as defined in this document, this will be recorded as
  # 02 16 . This field is mandatory for all versions of SEG-Y, although a value of
  # zero indicates "traditional" SEG-Y conforming to the 1975 standard.
  bindata[37, ] <- list("FORMAT_REV_NB", 1, 0L)
  # hd$FORMAT_REV_NB <- readBin(con, what = integer(), n = 1, size = 1, endian = ENDIAN, signed = FALSE)
  
  # 3502 Minor SEG-Y Format Revision Number. This is an 8-bit unsigned value with a
  # radix point between the first and second bytes. Thus for SEG-Y Revision 2.0,
  # as defined in this document, this will be recorded as 00 16 . This field is
  # mandatory for all versions of SEG-Y.
  bindata[38, ] <- list("FORMAT_REV_NB_MINOR", 1, 0L)
  # hd$FORMAT_REV_NB_MINOR <- readBin(con, what = integer(), n = 1, size = 1, endian = ENDIAN)
  
  # 3503-3504 - Fixed length trace flag. A value of one indicates that all traces in this SEG-Y
  # file are guaranteed to have the same sample interval, number of trace header
  # blocks and trace samples, as specified in Binary File Header bytes 3217-3218
  # or 3281-3288, 3517-3518, and 3221-3222 or 3289-3292. A value of zero
  # indicates that the length of the traces in the file may vary and the number of
  # samples in bytes 115-116 of the Standard SEG-Y Trace Header and, if
  # present, bytes 137-140 of SEG-Y Trace Header Extension 1 must be
  # examined to determine the actual length of each trace. This field is mandatory
  # for all versions of SEG-Y, although a value of zero indicates "traditional" SEGY
  # conforming to the 1975 standard. Irrespective of this flag, it is strongly
  # recommended that corect values for the number of samples per trace and
  # sample interval appear in the appropriate trace Trace Header locations.
  bindata[39, ] <- list("FIXED_LENGTH_FLAG", 2, 0L)
  # hd$FIXED_LENGTH_FLAG <- readBin(con, what = integer(), n = 1, size = 2, endian = ENDIAN)
  
  # 3505-3506 - Number of 3200-byte, Extended Textual File Header 
  # records following the Binary Header.
  bindata[40, ] <- list("NB_3200_BYTES", 2, 0L)
  # hd$NB_3200_BYTES <- readBin(con, what = integer(), n = 1, size = 2, endian = ENDIAN)
  
  # 3507-3510 - Maximum number of additional 240 byte trace headers.
  bindata[41, ] <- list("MAX_NB_240_HEADER", 4, 0L)
  # hd$MAX_NB_240_HEADER <- readBin(con, what = integer(), n = 1, size = 4, endian = ENDIAN)
  
  # 3511-3512 - Time basis code:
  # 1 = Local
  # 2 = GMT (Greenwich Mean Time)
  # 3 = Other, should be explained in a user defined stanza in the 
  # Extended Textual File Header
  # 4 = UTC (Coordinated Universal Time)
  # 5 = GPS (Global Positioning System Time)
  bindata[42, ] <- list("TIME_BASIS_CODE", 2, 0L)
  # hd$TIME_BASIS_CODE <- readBin(con, what = integer(), n = 1, size = 2, endian = ENDIAN)
 
  # 3513-3520 Number of traces in this file or stream
  # If zero, all bytes in the file or stream are part of this SEG-Y dataset.
  bindata[43, ] <- list("TRACE_NUMBER", 8, 0L)
  # hd$TRACE_NUMBER <- readBin(con, what = integer(), n = 1, size = 8, endian = ENDIAN)
  
  # 3521–3528 
  # Byte offset of first trace relative to start of file or stream if known
  bindata[44, ] <- list("BYTE_OFFSET", 8, 0L)
  # hd$BYTE_OFFSET <- readBin(con, what = integer(), n = 1, size = 8, endian = ENDIAN)
  
  # 3529–3532
  # Number of 3200-byte data trailer stanza records
  bindata[45, ] <- list("NB_DATA_TRAILER", 4, 0L)
  # hd$NB_DATA_TRAILER <- readBin(con, what = integer(), n = 1, size = 4, endian = ENDIAN)
  
  # 3533–3600 Unassigned
  bindata[46, ] <- list("UNASSIGNED2", 68, 0L)
  
  
  for(i in 1:nrow(bindata)){
    bdi <-  bindata[i,]
    if(bdi$size > 8){
      for(j in 1:bdi$size)
      writeBin(bdi$val, con = dsn, size = 1L, endian = endian)
    }else{
      writeBin(bdi$val, con = dsn, size = bdi$size, endian = endian)
    }
  }
  
}

#------------------------------------------------------------------------------#
# 240-byte Standard Trace Header
writeSGY_binary_trace_header <- function(dsn, x,  endian = "little"){
  if(length(x@coord) > 0 && sum(is.na(x@coord)) == 0){
    XYZ <- x@coord
  }else{
    XYZ <- matrix(0, nrow = ncol(x), ncol = 3)
    XYZ[, 1] <- x@pos
  }
  XYZ <-round(XYZ * 1000)
  storage.mode(XYZ) <- "integer"
  nspls <- nrow(x)
  storage.mode(nspls) <- "integer"
  dz <- as.integer(round(mean(diff(x@depth)) * 1e3))
  storage.mode(dz) <- "integer"
  for(i in seq_along(x)){
    # di_1 <- flen(dsn)
    # trace sequence number within line
    writeBin(i, con = dsn, size = 4, endian = endian)
    # trace sequence number within SEG-Y file 
    writeBin(i, con = dsn, size = 4, endian = endian)
    # Original field record number
    writeBin(0L, con = dsn, size = 4, endian = endian)
    # Trace number within original field record
    writeBin(0L, con = dsn, size = 4, endian = endian)
    # Energy source point number
    writeBin(0L, con = dsn, size = 4, endian = endian)
    # CDP ensemble number || CDP = CMP
    writeBin(0L, con = dsn, size = 4, endian = endian)
    # Trace  number within the ensemble
    writeBin(i, con = dsn, size = 4, endian = endian)
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
    writeBin(1L, con = dsn, size = 2, endian = endian)
    # Stacking: Number of vertically summed traces yielding this trace
    writeBin(0L, con = dsn, size = 2, endian = endian)
    # Number of horizontally summed traces yielding this trace
    writeBin(0L, con = dsn, size = 2, endian = endian)
    # data use:
    # 1 = production;
    # 2 = test.
    writeBin(1L, con = dsn, size = 2, endian = endian)
    # Distance from center of the source point to the center of the receiver group
    # (negative if opposite to direction in which line is shot).
    writeBin(0L, con = dsn, size = 4, endian = endian)
    # Elevation of receiver group
    writeBin(XYZ[i, 3], con = dsn, size = 4, endian = endian)
    # *** Surface elevation at source location.
    writeBin(XYZ[i, 3], con = dsn, size = 4, endian = endian)
    # Source depth below surface
    writeBin(0L, con = dsn, size = 4, endian = endian)
    # Seismic Datum elevation at receiver group
    writeBin(0L, con = dsn, size = 4, endian = endian)
    # Seismic Datum elevation at source.
    writeBin(0L, con = dsn, size = 4, endian = endian)
    # Water column height at source location
    writeBin(0L, con = dsn, size = 4, endian = endian)
    # Water column height at receiver group location
    writeBin(0L, con = dsn, size = 4, endian = endian)
    # Scalar to be applied to all elevations and depths
    writeBin(1000L, con = dsn, size = 2, endian = endian)
    # Scalar to be applied to all coordinates
    writeBin(1000L, con = dsn, size = 2, endian = endian)
    # Source coordinate - X.
    writeBin(XYZ[i, 1], con = dsn, size = 4, endian = endian)
    # Source coordinate - Y.
    writeBin(XYZ[i, 2], con = dsn, size = 4, endian = endian)
    # Group coordinate - X.
    writeBin(XYZ[i, 1], con = dsn, size = 4, endian = endian)
    # Group coordinate - Y.
    writeBin(XYZ[i, 2], con = dsn, size = 4, endian = endian)
    # Coordinates units
    writeBin(1L, con = dsn, size = 2, endian = endian)
    # Weathering velocity (ft/s or m/s as specified in Binary File Header 
    # bytes 3255– 3256)
    writeBin(0L, con = dsn, size = 2, endian = endian)
    # Subweathering velocity. (ft/s or m/s as specified in Binary File Header 
    # bytes 3255–3256)
    writeBin(0L, con = dsn, size = 2, endian = endian)
    # Uphole time at source in milliseconds.
    writeBin(0L, con = dsn, size = 2, endian = endian)
    # Uphole time at group in milliseconds.
    writeBin(0L, con = dsn, size = 2, endian = endian)
    # Source static correction in milliseconds.
    writeBin(0L, con = dsn, size = 2, endian = endian)
    # Group static correction in milliseconds.
    writeBin(0L, con = dsn, size = 2, endian = endian)
    # Total static applied in milliseconds. (Zero if no static has
    # been applied,)
    writeBin(0L, con = dsn, size = 2, endian = endian)
    # Lag time A
    writeBin(0L, con = dsn, size = 2, endian = endian)
    # Lag Time B
    writeBin(0L, con = dsn, size = 2, endian = endian)
    # Delay recording time
    writeBin(0L, con = dsn, size = 2, endian = endian)
    # Mute time — Start time in milliseconds.
    writeBin(0L, con = dsn, size = 2, endian = endian)
    # Mute time — End time in milliseconds.
    writeBin(0L, con = dsn, size = 2, endian = endian)
    # *** Number of samples in this trace.
    writeBin(nspls, con = dsn, size = 2, endian = endian)
    # *** Sample interval for this trace.
    writeBin(dz,    con = dsn, size = 2, endian = endian)
    # bytes 119 - 156 
    for(j in 1:19){
      writeBin(0L, con = dsn, size = 2, endian = endian)
    }
    # bytes 157 - 1240 
    for(j in 1:42){
      writeBin(0L, con = dsn, size = 2, endian = endian)
    }
    
    writeBin(as.integer(x@data[, i]), con = dsn, size = 2, endian = endian)
    # invisible(seek(con, where = 156 - 118, origin = "current"))
    # 
    # # Year data recorded
    # trhd[22, i] <- readBin(con, what = integer(), n = 1, size = 2, endian = ENDIAN)
    # # Day of year
    # trhd[23, i] <- readBin(con, what = integer(), n = 1, size = 2, endian = ENDIAN)
    # # Hour of day
    # trhd[24, i] <- readBin(con, what = integer(), n = 1, size = 2, endian = ENDIAN)
    # # Minute of hour.
    # trhd[25, i] <- readBin(con, what = integer(), n = 1, size = 2, endian = ENDIAN)
    # # Second of minute.
    # trhd[26, i] <- readBin(con, what = integer(), n = 1, size = 2, endian = ENDIAN)
    # 
    # # Time basis code. If nonzero, overrides Binary File Header bytes 3511-3512.
    # # 1 = Local
    # # 2 = GMT (Greenwich Mean Time)
    # # 3 = Other, should be explained in a user defined stanza in the Extended
    # # Textual File Header
    # # 4 = UTC (Coordinated Universal Time)
    # # 5 = GPS (Global Positioning System Time)
    # trhd[27, i] <- readBin(con, what = integer(), n = 1, size = 2, endian = ENDIAN)
    # 
    # invisible(seek(con, where = 3600 + 240 * i + nspls * nbytes * (i-1), origin = "start"))
    
    # if(grepl("float", DATA_FORMAT)){  #  == "4-byte IEEE floating-point"){
    #   dataSGY[,i] <- readBin(con, what = double(), n = nspls, 
    #                          size = nbytes, endian = ENDIAN, signed = TRUE)
    # }else{
    #   dataSGY[,i] <- readBin(con, what = integer(), n = nspls, 
    #                          size = nbytes, endian = ENDIAN)
    # }
  }
  
  # bindata <- data.frame(tag = rep("", 10),
  #                       # what = "integer",
  #                       size = rep(4, 10),
  #                       val = 0L)
}

