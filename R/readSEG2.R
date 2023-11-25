

.readSEG2 <- function(x, fName = character(0), desc = character(0),
                      fPath = character(0), Vmax = NULL, ext = ""){
  if(is.null(Vmax)) Vmax <- 50
  
  ntr <- dim(x$data)[2]
  nspl <- dim(x$data)[1]
  nset <- dim(x$data)[3]
  
  # find SAMPLING_INTERVAL or SAMPLE_INTERVAL or SPR_SAMPLING_INTERVAL
  sel <- grepl("(SAMPL)(.+)(_INTERVAL)", x$HD$strings)
  ddt <- extractPattern(x$HD$strings[sel], pat = "(?<dt>[0-9]+)")
  data_dt <- 0.4 # ns default
  if(length(ddt) && ddt != ""){
    ddt <- as.numeric(ddt)
    if(!is.na(ddt)){
      data_dt <- ddt / 1000 # ns
    }
  }
  
  # find date yyyy/mm/dd or mm/dd/yyyy or mm/dd/yy or yy/mm/dd or yy-mm-dd, etc.
  sel <- grepl("([0-9]{2}|[0-9]{4})[/|-]([0-9]{2})[/|-]([0-9]{2}|[0-9]{4})", x$HD$strings)
  ddate <- extractPattern(x$HD$strings[sel], pat = "(?<date>[0-9]{2}[/|-][0-9]{2}[/|-][0-9]{2})")
  data_date <- format(Sys.time(), "%Y-%m-%d")   # default
  if(length(ddate) > 0){
    ddate <- as.Date(ddate, "%m/%d/%y")  # c("%Y-%m-%d", "%Y/%m/%d")
    if(!is.na(ddate)){
      data_date <- format(ddate, "%Y-%m-%d")
    }
  }
  
  # find time hh:mm:ss
  sel <- grepl("([0-9]{2})[:]([0-9]{2})[:]([0-9]{2})", x$HD$strings)
  dtime <- extractPattern(x$HD$strings[sel], pat = "(?<TIME>[0-9]{2}[:][0-9]{2}[:][0-9]{2})")
  data_time <- as.double(as.POSIXct(data_date,   origin = as.Date(data_date))) + seq_len(ntr)
  if(length(dtime) > 0){
    traceTime <- as.double(as.POSIXct(paste0(data_date, " ", dtime),   origin = as.Date(data_date)))
    if(!is.na(traceTime)){
      data_time <- traceTime  + seq_len(ntr)
    }
  }
  
  #--- dx spacing
  # 1. search for the triger mode -> key word
  sel <- grepl("TRIGGER(.+)MODE", x$HD$strings)
  spmode <- extractPattern(x$HD$strings[sel], pat = "MODE(?<ID>.+)")
  # 2. use the key word to find the dx spacing
  sel <- grepl(paste0(trimStr(spmode), "(.+)INTERVAL"), x$HD$strings)
  dx <- as.numeric(strsplit(x$HD$strings[sel], " ")[[1]][2])
  
  # Byte value    Data format
  # 01            16-bit fixed point      
  # 02            32-bit fixed point      
  # 03            20-bit floating point (SEG convention)      
  # 04            32-bit floating point (IEEE standard)      
  # 05            64-bit floating point (IEEE standard) 
  if(x$THD$dataFormatCode == 1){
    nbits <- 16
  }else if(x$THD$dataFormatCode == 2){
    nbits <- 32
  }else if(x$THD$dataFormatCode == 3){
    nbits <- 20
  }else if(x$THD$dataFormatCode == 4){
    nbits <- 32
  }else if(x$THD$dataFormatCode == 5){
    nbits <- 64
  }else{
    16
  }
  
  antfreq <- 0
  message("Antenna frequency set to 0 MHz. Set it with 'antfreq(x) <- ... '")
  antsep <- 0
  message("Antenna separation set to 0 ", "m", 
          ". Set it with 'antsep(x) <- ... '")
  
  if(nset == 1){
    if(length(dim(x$data)) == 3) x$data <- x$data[,,1]
    y <- new("GPR",   
             version      = "0.2",
             data        = bits2volt(Vmax = Vmax, nbits = nbits) * x$data,
             traces      = seq_len(ntr),
             fid         = rep("", ntr),
             coord       = matrix(nrow = 0, ncol = 3),
             pos         = seq(from = 0, by = dx, length.out = ntr),
             depth       = seq(from = 0, by = data_dt, length.out = nspl),
             rec         = matrix(nrow = 0, ncol = 3),
             trans       = matrix(nrow = 0, ncol = 3),
             time0       = rep(0, ntr),
             time        = data_time,
             proc        = character(0),
             vel         = list(v = 0.1),
             name        = fName,
             description = desc,
             filepath    = fPath,
             dz          = data_dt, 
             dx          = dx,
             depthunit   = "ns",
             posunit     = "m",
             freq        = antfreq, 
             antsep      = antsep,     # check
             surveymode  = "reflection",
             date        = data_date,
             crs         =  character(0),
             hd          = c(x$HD, x$THD)
    )
  }else{
    message("I return a GPR set = a 3D object with all antenna data.\n",
            "To extract for example the 2nd data set, do 'x[,,2].")
    if(length(ext) ==  nset){
      set_names <- paste0(fName, ".", ext)
    }else{
      set_names <-  paste("antenna", seq_len(nset))
    }
    y <- new("GPRset",   
             version      = "0.2",
             data        = bits2volt(Vmax = Vmax, nbits = nbits) * x$data,
             traces      = seq_len(ntr),
             fid         = rep("", ntr),
             coord       = matrix(nrow = 0, ncol = 3),
             pos         = seq(0, dx, length.out = ntr),
             depth       = seq(0, by = data_dt, length.out = nspl),
             rec         = matrix(nrow = 0, ncol = 3),
             trans       = matrix(nrow = 0, ncol = 3),
             time0       = rep(0, ntr),
             time        = data_time,
             proc        = character(0),
             vel         = list(v = 0.1),
             name        = fName,
             description = desc,
             filepath    = fPath,
             dz          = data_dt, 
             dx          = dx,
             depthunit   = "ns",
             posunit     = "m",
             freq        = antfreq, 
             antsep      = antsep,     # check
             surveymode  = "reflection",
             date        = data_date,
             crs         =  character(0),
             hd          = c(x$HD, x$THD),
             setnames    = set_names,
             sets        = seq_len(nset),
             setunit     = rep("", nset)
    )
  }
  return(y)
}




#' Read US Radar files
#' 
#' RA1, RA2 and RAD files (SEG-2 format)
#' @export
readUSRadar <- function(dsn){
  readSEG2(dsn)
}

#' Read SEG-2 
#' 
#' SEG2 as well as US Radar (RA1, RA2 and RAD) files
#' @export
readSEG2 <- function(dsn){
  if(!is.list(dsn)) dsn <- as.list(dsn)
  dsnall <- dsn
  dsn <- dsn[[1]]
  if( !inherits(dsn, "connection") ){
    dsn <- file(dsn, "rb")
  }
  HD <- list()
  
  #----- FILEDESCRIPTOR Identifier label.
  # The File Descriptor Block ID (bytes 0 and 1 of this block and of the file) 
  # contains the integer 3a55 (in hexadecimal).  This integer identifies the file 
  # as a seismic data file following this standard and identifies this block as 
  # the Record Descriptor Block (55 appears first, since it is the low byte). 
  HD$uu <- readBin(dsn, what = integer(), n = 1, size = 2)
  if(HD$uu != 14933){
    close(dsn)
    stop("Not a 'SEG-2' file type")
  }
  
  # 0-1 3a55 (File Descriptor Block ID)
  # 2-3           REVISION           NUMBER           
  # 4-5 SIZE OF TRACE POINTER SUB-BLOCK (M) 
  # 6-7 NUMBER OF TRACES IN FILE (N)   
  # 8 SIZE OF STRING TERMINATOR   
  # 9 FIRST STRING TERMINATOR CHARACTER 
  # 10 SECOND STRING TERMINATOR CHARACTER 
  # 11 SIZE OF LINE TERMINATOR 
  # 12 FIRST LINE TERMINATOR CHARACTER 
  # 13 SECOND LINE TERMINATOR CHARACTER            
  # 14-31           RESERVED           
  # 32-35  POINTER TO TRACE DESCRIPTOR BLOCK 1 
  # 36-39  POINTER TO TRACE DESCRIPTOR BLOCK 2
  #--------------------------- #
  # ----  POINTER TO TRACE DESCRIPTOR B
  # 33 + M  STRING 1
  #         STRING 2
  # . . . . . . . . .    
  # M STRING Z
  
  # read file descriptor block
  # uint16
  HD$revNumber          <- readBin(dsn, what = integer(), n = 1, size = 2, signed = TRUE)
  HD$sizeOfTracePointer <- readBin(dsn, what = integer(), n = 1, size = 2, signed = FALSE)
  HD$nbOfTraces         <- readBin(dsn, what = integer(), n = 1, size = 2, signed = FALSE)
  # The String Terminator is one or two non-printable ASCII characters (decimal 
  # ASCII codes 0 through 31) used to separate the strings that hold the 
  # information in character string form in this (the File Descriptor) block, 
  # and the Trace Descriptor Blocks.  Byte 8 is 01 (hex) and bytes 9 and 10 are 
  # 00 (hex) indicating the string terminator used by the StrataVisor is the 
  # NULL character. 
  # uint8
  HD$sizeOfST           <- readBin(dsn, what = integer(), n = 1, size = 1, signed = FALSE) # fread (fid,1,'uchar')
  HD$firstST            <- readBin(dsn, what = integer(), n = 1, size = 1, signed = FALSE) 
  HD$secondST           <- readBin(dsn, what = integer(), n = 1, size = 1, signed = FALSE) 
  HD$sizeOfLT           <- readBin(dsn, what = integer(), n = 1, size = 1, signed = FALSE)
  HD$firstLT            <- readBin(dsn, what = integer(), n = 1, size = 1, signed = FALSE) 
  HD$secondLT           <- readBin(dsn, what = integer(), n = 1, size = 1, signed = FALSE) 
  # reserved           <- readBin(dsn, what = integer(), n = 1, size = 1, signed = FALSE) 
  invisible(seek(dsn, where = 32, origin = "start"))
  
  # uint32
  tracePointer      <- readBin(dsn, what = integer(), n = HD$nbOfTraces, size = 4)
  tracePointer <- int32touint32(tracePointer)
  
  
  # uint8
  n_textblock <- tracePointer[1] - 32 - HD$sizeOfTracePointer
  invisible(seek(dsn, where = 32 + HD$sizeOfTracePointer, origin = "start"))
  textBlock <- readBin(dsn, what = integer(), n = n_textblock, size = 1, signed = FALSE) 
  lst <- split(textBlock[textBlock != 0], cumsum(textBlock == 0)[textBlock != 0])
  v <- sapply(lst, intToUtf8, USE.NAMES = FALSE)
  HD$strings <- as.vector(v[sapply(v, nchar, USE.NAMES = FALSE) > 1])
  
  #------ TRACE HEADER 
  # 
  # fmt = {'uint16',{'TraceDescriptor','TraceBlockSize'};
  #   'uint32',{'TraceDataSize','NumberOfSamples'};...
  #   'uint8', {'TraceDataFormatCode'} };
  
  # matrix(nrow = HD$nbOfTraces, ncol = 5, 
  #               dimnames = list(NULL,
  #                               c("desc", "blockSize", "dataSize", "nsamples", "dataFormatCode")))
  
  THD <- list()
  invisible(seek(dsn, where = tracePointer[1], origin = "start"))
  THD$desc <- readBin(dsn, what = integer(), n = 1, size = 2, signed = FALSE)
  if(as.character(as.hexmode(THD$desc)) != "4422"){ 
    stop("Trace descriptor different from '4422' (hex):\n",
         as.character(as.hexmode(THD$desc)))
  }
  THD$blockSize <- readBin(dsn, what = integer(), n = 1, size = 2, signed = FALSE)
  THD$dataSize <- readBin(dsn, what = integer(), n = 1, size = 4)
  THD$nSamples <- readBin(dsn, what = integer(), n = 1, size = 4)
  THD$dataFormatCode <- readBin(dsn, what = integer(), n = 1, size = 1, signed = FALSE)
  # Byte value    Data format
  # 01            16-bit fixed point      
  # 02            32-bit fixed point      
  # 03            20-bit floating point (SEG convention)      
  # 04            32-bit floating point (IEEE standard)      
  # 05            64-bit floating point (IEEE standard) 
  DData <- list()
  DD <- matrix(ncol = HD$nbOfTraces, nrow = THD$nSamples)
  for( i in 1:HD$nbOfTraces){
    # invisible(seek(dsn, where = tracePointer[i], origin = "start"))
    # THD[i, 1] <- readBin(dsn, what = integer(), n = 1, size = 2, signed = FALSE)
    # if(as.character(as.hexmode(THD[i, 1])) != "4422"){ 
    #   stop("Trace descriptor different from '4422' (hex):\n",
    #        as.character(as.hexmode(THD[i, 1])))
    # }
    # THD$blockSize <- readBin(dsn, what = integer(), n = 1, size = 2, signed = FALSE)
    # THD$dataSize <- readBin(dsn, what = integer(), n = 1, size = 4)
    # THD$nSamples <- readBin(dsn, what = integer(), n = 1, size = 4)
    # THD$dataFormatCode <- readBin(dsn, what = integer(), n = 1, size = 1, signed = FALSE)
    invisible(seek(dsn, where = tracePointer[i] + THD$blockSize,  origin = "start"))
    DD[,i] <- readBin(dsn, what = integer(), n = THD$nSamples, size = 2)
  }
  DData[[1]] <- DD
  close(dsn)
  
  if(length(dsnall) > 1){
    # dsnall[[1]] <- NULL# <- dsnall[-1]
    for(k in seq_len(length(dsnall) - 1)){
      dsn <- dsnall[[k + 1]]
      if( !inherits(dsn, "connection") ){
        dsn <- file(dsn, "rb")
      }
      DD <- matrix(ncol = HD$nbOfTraces, nrow = THD$nSamples)
      for( i in 1:HD$nbOfTraces){
        invisible(seek(dsn, where = tracePointer[i] + THD$blockSize,  origin = "start"))
        DD[,i ] <- readBin(dsn, what = integer(), n = THD$nSamples, size = 2)
      }
      DData[[k+1]] <- DD
      close(dsn)
    }
  }
  # if(!missing(dsn2)){
  #   if( !inherits(dsn2, "connection") ){
  #     dsn2 <- file(dsn2, "rb")
  #   }
  #   DD <- matrix(ncol = HD$nbOfTraces, nrow = THD$nSamples)
  #   for( i in 1:HD$nbOfTraces){
  #     invisible(seek(dsn, where = tracePointer[i] + THD$blockSize,  origin = "start"))
  #     DD[,i ] <- readBin(dsn, what = integer(), n = THD$nSamples, size = 2)
  #   }
  #   DData[[2]] <- DD
  #   close(dsn2)
  # }
  # if(!missing(dsn3)){
  #   if( !inherits(dsn3, "connection") ){
  #     dsn3 <- file(dsn3, "rb")
  #   }
  #   DD <- matrix(ncol = HD$nbOfTraces, nrow = THD$nSamples)
  #   for( i in 1:HD$nbOfTraces){
  #     invisible(seek(dsn, where = tracePointer[i] + THD$blockSize,  origin = "start"))
  #     DD[, i] <- readBin(dsn, what = integer(), n = THD$nSamples, size = 2)
  #   }
  #   DData[[3]] <- DD
  #   close(dsn3)
  # }
  DData <- array(unlist(DData), dim = c(nrow(DData[[1]]), ncol(DData[[1]]), length(DData)))
  return(list(HD = HD, THD = THD, data = DData))
}


#' @export
readGPSUSRADAR <- function(dsn, toUTM = FALSE){
  coor <- read.table(dsn, header = FALSE, sep = ",", skip = 1)
  
  names(coor) <- c("nb", "id", "y", "x", "z", "NA1", "NA2", "NA3", "NA4")
  coor_crs <- "EPSG:4326"
  if(toUTM == TRUE){
    topoUTM <-  llToUTM(lat = coor[,"y"], 
                        lon = coor[,"x"])
    coor[, "x"] <- topoUTM$xy[,1]
    coor[,"y"] <- topoUTM$xy[,2]
    coor_crs <- topoUTM$crs
  } # else
  return(list(mrk = coor[c("x", "y", "z", "id")], crs = coor_crs))
}

