
# -------------------------------------------
# ------------writeDT1--------------------------
# -------------------------------------------
# @name  writeDT1 
# @description This function writes *.HD and associated *.DT1
# files  (Sensors & Software)

# @date 07.11.2012 08:33
# @auteur Emanuel Huber
# @param [text]    fileNameHD       (file path of *.hd file)
# @param [text]    fileNameDT1       (file path of *.dt1 file)


# @return list((hd = headerHD, dt1hd = headerDT1, data=myData))
# -------------------------------------------
.writeDT1 <- function(x, fPath, endian = "little"){
  #-------------------- DT1 FILE: traces
  # should ranges between -32768 and 32767
  x@data[!is.finite(x@data)] <- 0
  x@data <-  round( (x@data - min(x@data))/(diff(range(x@data))) * 
                      (32767 + 32768) - 32768 )
  
  #   cat(ra  nge(traceData),"\n")
  if(min(x@data) < -32768 || max(x@data) > 32768){
    stop("problem real > integer conversion")
  }
  #   A
  #   traceData <- A$data
  #  storage.mode(x@data) <- "integer" -> see function writeBinary
  
  binSize <- rep(4, 23)
  binSize[9:11] <- 8
  binSize[23] <- 28
  binMod <- rep("numeric", 23)
  binMod[23] <- "raw"
  
  # DT1 FILE: header
  # indexDT1Header=c("traces", "position", "samples","topo", "NA0", "bytes","win", 
  #                  "stacks","NA1","NA2", "NA3", "NA4", "NA5", "NA6", "recx","recy",
  #                  "recz","transx","transy","transz","time0","zeroflag", "NA7", "time",
  #                  "x8", "com","com1","com2","com3","com4","com5","com6")
  traces_hd <- list()
  traces_hd$traces <- x@traces              # 1. Trace number
  traces_hd$position <- x@pos               # 2. Trace position
  # 3. Number sample per trace
  traces_hd$samples <- rep(nrow(x@data), ncol(x@data))
  # 4. Topo
  if(length(x@coord) > 0 && sum(is.na(x@coord)) == 0){
    traces_hd$topo <- x@coord[,3]
  }else{
    traces_hd$topo <- rep.int(0L, ncol(x@data))
  }
  # 5. Not used
  traces_hd$NA0 <- rep.int(0L, ncol(x@data))
  # 6. bytes/points (always 2 for Rev 3 firmware)
  traces_hd$bytes <- rep.int(2L, ncol(x@data))
  # 7. Time Window
  # traces_hd$win <- rep(x@dz * (nrow(x@data) - 1), ncol(x@data))
  # traces_hd$win <- rep(x@dz * nrow(x@data) , ncol(x@data))
  wwin <- round(diff(range(x@depth))/(nrow(x@data)-1)* (nrow(x@data) ), 5)
  traces_hd$win <- rep(wwin , ncol(x@data))
  # 8. # of stacks
  # traces_hd$stacks <- rep(as.integer(x@hd$NUMBER_OF_STACKS), ncol(x@data))
  # DT1
  if(length(x@hd) > 0 && !is.null(x@hd$NUMBER_OF_STACKS) 
     && is.numeric(as.integer(x@hd$NUMBER_OF_STACKS))){
    traces_hd$stacks <- rep(as.integer(x@hd$NUMBER_OF_STACKS), ncol(x@data))
    # rd3 / rd7
  }else if(length(x@hd) > 0 && !is.null(x@hd$STACKS) 
           && is.numeric(as.integer(x@hd$STACKS))){
    traces_hd$stacks <- rep(as.integer(x@hd$STACKS), ncol(x@data))
  }else{
    traces_hd$stacks <- rep(1L, ncol(x@data))
  }
  # 9.-10. GPS X-position (double*8 number)
  # 11.-12. GPS Y-position (double*8 number)
  # 13.-14. GPS Z-position (double*8 number)
  if(length(x@coord) > 0 && sum(is.na(x@coord)) == 0){
    traces_hd$GPSx <- x@coord[,1]
    traces_hd$GPSy <- x@coord[,2]
    traces_hd$GPSz <- x@coord[,3]
  }else{
    traces_hd$GPSx <- rep.int(0L, ncol(x@data))
    traces_hd$GPSy <- rep.int(0L, ncol(x@data))
    traces_hd$GPSz <- rep.int(0L, ncol(x@data))
  }
  # 15.-17. receiver x-, y- and z-position
  if(length(x@rec) > 0 && sum(is.na(x@rec)) == 0){
    traces_hd$recx <- x@rec[,1]
    traces_hd$recy <- x@rec[,2]
    traces_hd$recz <- x@rec[,3]
  }else{
    traces_hd$recx <- rep.int(0L, ncol(x@data))
    traces_hd$recy <- rep.int(0L, ncol(x@data))
    traces_hd$recz <- rep.int(0L, ncol(x@data))
  }
  # 18.-20. transmitter x-, y- and z-position
  if(length(x@trans) > 0 && sum(is.na(x@trans)) == 0){
    traces_hd$transx <- x@trans[,1]
    traces_hd$transy <- x@trans[,2]
    traces_hd$transz <- x@trans[,3]
  }else{
    traces_hd$transx <- rep.int(0L,ncol(x@data))
    traces_hd$transy <- rep.int(0L,ncol(x@data))
    traces_hd$transz <- rep.int(0L,ncol(x@data))
  }
  # 21. time-zero adjustment | where: point(x) = point(x + adjustment)
  # traces_hd$time0 <- 1L + round(x@time0/x@dz, 2)
  traces_hd$time0 <- rep(0L, ncol(x@data)) # 1L + as.integer(round(x@time0/x@dz))
  # traces_hd$time0 <- 1L + as.integer(round(x@time0/x@dz))
  # 22. zero flag: 0 = data okay, 1=zero data
  traces_hd$zeroflag <- rep.int(0L, ncol(x@data)) 
  # 23. not used
  traces_hd$NA7 <- rep.int(0L, ncol(x@data))
  # 24. Time of day data collected in seconds past midnight.
  if(length(x@time) > 0 ){
    aa <- as.POSIXct(x@time[1], origin = "1970-01-01")
    bb <- format(aa, format = "%Y-%m-%d")
    myDay <- as.double(as.POSIXct(as.Date(bb), origin="1970-01-01"))
    traces_hd$time <- x@time - myDay
  }else{
    traces_hd$time <- rep(0, ncol(x@data))
  }
  # 25. Comment flag: 1 = comment attached.
  traces_hd$x8 <- rep.int(0L, ncol(x@data)) 
  traces_hd$x8[trimStr(x@fid) != ""] <- 1L
  # 26.-32. Comment string of 28 characters
  traces_hd$com <-  lapply(trimStr(x@fid), function(x){
    if(nchar(x) > 28){
      x <- charToRaw(substr(x, 1, 28))
    }else{
      # x <- paste0(c(rep(" ", 28 - nchar(x))), x, collapse = "")
      x <- c(charToRaw(x) , raw(28 - nchar(x)))
    }
    return(x)
  })
  
  # FILE NAMES
  fPath <- file.path(dirname(fPath), .fNameWExt(fPath))
  
  # WRITE DT1 FILE
  dt1_file <- file(paste0(fPath, ".DT1") , "wb")
  for(i in 1:ncol(x@data)){
    for(j in seq_along(traces_hd)){
      # real*4, storage.mode = double
      writeBinary(traces_hd[[j]][[i]], dt1_file, what = binMod[j], 
                  size = binSize[j], eos = NULL, endian = endian)
    }
    # comment28 <- as.character(traces_hd$com[i])
    # if(nchar(comment28) > 28) comment28 <- substr(comment28, 0, 28)
    # com_add <- paste(c(rep(" ", 28-nchar(comment28)), comment28), 
    #                  sep = "", collapse = "")
    # writeChar(com_add, dt1_file,nchars =28,eos = NULL)
    # suppressWarnings( writeChar(comment28, dt1_file,nchars = 28,eos = NULL))
    # for(k in 1:nnchar){
    # writeChar("^@", dt1_file)
    # }
    # two-byte integers
    writeBinary(x@data[,i], dt1_file, what = "integer", size = 2, 
                endian = endian)
  }
  
  # writeChar(traces_hd[[j]][i], dt1_file, nchars = 28, eos = NULL)
  
  close(dt1_file)
  
  # for(i in 1:ncol(x@data)){
  #   for(j in 1:25){
  #     realData4 <- traces_hd[[j]][i]
  #     storage.mode(realData4) <- "double"
  #     writeBin(realData4, dt1_file, size = 4, endian = "little")
  #   }
  #   comment28 <- as.character(traces_hd$com[i])
  #   if(nchar(comment28) > 28) comment28 <- substr(comment28, 0, 28)
  #   com_add <- paste(c(rep(" ", 28-nchar(comment28)), comment28), 
  #                    sep = "", collapse = "")
  #   writeChar(com_add, dt1_file, nchars = 28, eos = NULL)
  #   # suppressWarnings( writeChar(comment28, dt1_file,nchars = 28,eos = NULL))
  #   # for(k in 1:nnchar){
  #   # writeChar("^@", dt1_file)
  #   # }
  #   # two-byte integers
  #   writeBin(traceData[,i], dt1_file, size = 2, endian = "little")
  # }
  # close(dt1_file)
  
  # j<-0
  
  # j<-j+1
  # traces_hd[[indexDT1Header[j]]][i]
  
  #-------------------------
  # HD FILE: traces
  hd_file <- file(paste0(fPath, ".HD") , "w+")
  writeLines("1234", con = hd_file, sep = "\r\r\n")
  if(!is.null(x@hd$gprdevice)){
    writeLines(as.character(x@hd$gprdevice), con = hd_file, sep = "\r\r\n")
  }else{
    writeLines("Data from RGPR", con = hd_file, sep = "\r\r\n")
  }
  writeLines(as.character(x@date), con = hd_file, sep = "\r\r\n")
  writeLines(paste0("NUMBER OF TRACES   ","= ", as.character(ncol(x@data))), 
             con = hd_file, sep = "\r\r\n")
  writeLines(paste0("NUMBER OF PTS/TRC  ","= ", as.character(nrow(x@data))), 
             con = hd_file, sep = "\r\r\n")
  writeLines(paste0("TIMEZERO AT POINT  ", "= ",
                    as.character(1+round(mean(x@time0)/x@dz,2))), 
             con = hd_file, sep = "\r\r\n")
  writeLines(paste0("TOTAL TIME WINDOW  ", "= ", 
                    # as.character(x@dz*(nrow(x@data)))), 
                    as.character(wwin)), 
             con = hd_file, sep = "\r\r\n")
  # startpos <- 0
  # if(!is.null(x@hd$startpos)){
  #   startpos <- x@hd$startpos
  # }
  writeLines(paste0("STARTING POSITION  ", "= ", as.character(x@pos[1])), 
             con = hd_file, sep = "\r\r\n")
  
  # endpos <- (ncol(x@data)-1)*x@dx
  # if(!is.null(x@hd$endpos)){
  #   endpos <- x@hd$endpos
  # }
  # writeLines(paste0("FINAL POSITION     ", "= ", as.character(endpos)), 
  writeLines(paste0("FINAL POSITION     ", "= ", as.character(tail(x@pos, 1))), 
             con = hd_file, sep = "\r\r\n")
  
  # writeLines(paste0("STEP SIZE USED     ", "= ", as.character(x@dx)),
  writeLines(paste0("STEP SIZE USED     ", "= ", as.character(abs(diff(range(x@pos)))/(ncol(x@data) - 1))),
             con = hd_file, sep = "\r\r\n")
  writeLines(paste0("POSITION UNITS     ", "= ", "m"), 
             con = hd_file, sep = "\r\r\n")
  if(x@posunit != "m"){
    warning('Position units were defined as "metres"!\n')
  }
  writeLines(paste0("NOMINAL FREQUENCY  ","= ", as.character(x@freq)), 
             con = hd_file, sep = "\r\r\n")
  writeLines(paste0("ANTENNA SEPARATION ","= ", as.character(x@antsep)), 
             con = hd_file, sep = "\r\r\n")
  pulservoltage <- 0
  if(!is.null(x@hd$PULSER_VOLTAGE_V)){
    pulservoltage <- x@hd$PULSER_VOLTAGE_V
  }
  writeLines(paste0("PULSER VOLTAGE (V) ", "= ", as.character(pulservoltage)), 
             con = hd_file, sep = "\r\r\n")
  nstacks <- 1
  if(!is.null(x@hd$NUMBER_OF_STACKS)){
    nstacks <- x@hd$NUMBER_OF_STACKS
  }
  writeLines(paste0("NUMBER OF STACKS   ", "= ", as.character(nstacks)), 
             con = hd_file, sep = "\r\r\n")
  writeLines(paste0("SURVEY MODE        ", "= ", capFirstLetter(x@surveymode)), 
             con = hd_file, sep = "\r\r\n")
  
  # NIC SERIAL#        = 0044-5029-0015
  # TX SERIAL#         = 0034-3741-0010
  # RX SERIAL#         = 0033-3740-0010
  # SPIDAR MCS         = OFF
  # Serial#            = 
  # CAL tm             =
    

  close(hd_file)
  return(fPath)
}


writeBinary <- function(object, con, what = "numeric", size = NA_integer_,
                        endian = .Platform$endian, useBytes = FALSE, 
                        eos = ""){
  if(what == "character"){
    storage.mode(object) <- "character"
    writeChar(object, con, nchars = size, eos = eos, useBytes = useBytes)
  }else if(what == "numeric"){
    storage.mode(object) <- "double"
    writeBin(object, con, size = size, endian = endian, useBytes = useBytes)
  }else if(what == "integer"){
    storage.mode(object) <- "integer"
    writeBin(object, con, size = size, endian = endian, useBytes = useBytes)
  }else if(what == "raw"){
    storage.mode(object) <- "raw"
    writeBin(object, con, size = 1, endian = endian, useBytes = useBytes)
  }
  
}
