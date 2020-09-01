.gprDZT <- function(x, fName = character(0), desc = character(0),
                    fPath = character(0), Vmax = NULL){
  
  if(is.null(Vmax)) Vmax <- 50
  
  dd <- as.Date(x$hd$DATE, format = "%Y-%m-%d")
  if(is.na(dd)){
    dd <- Sys.Date()
  }
  traceTime <- rep(0, ncol(x$data))
  if(length(fName) == 0){
    x_name <- "LINE"
  }else{
    x_name <- fName
  }
  # defaults
  x_posunit   <- "m"
  x_depthunit <- "ns"
  x_pos       <- x$pos[1:ncol(x$data)]
  x_depth     <- x$depth[1:nrow(x$data)]
  x_dx        <- 1 / x$hd$SPM
  # Fiducial markers > each class has a different name (letter)
  x_fid       <- rep("", ncol(x$data))
  test <- which(x$hd$MRKS < 0)
  fidval <- LETTERS[as.numeric(as.factor(x$hd$MRKS[test]))]
  ufidval <- unique(fidval)
  for( i in seq_along(ufidval)){
    test2 <- which(fidval == ufidval[i])
    fid_nb <- seq_along(test2)
    x_fid[test][test2] <- paste0(ufidval[i], 
                                 sprintf(paste0("%0", max(nchar(fid_nb)), "d"), 
                                         fid_nb))
  }
  if(!is.null(x$dzx)){
    # pos
    if(!is.null(x$dzx$pos)){
      x_pos <- x$dzx$pos
    }
    # spatial sampling
    if(!is.null(x$dzx$dx)){
      x_dx <- x$dzx$dx
    } 
    if(all(x_fid == "") &&
       !is.null(x$dzx$markers) && 
       length(x$dzx$markers) == ncol(x$data)){
      x_fid <- x$dzx$markers
    }

    if(!is.null(x$dzx$hUnit)){
      x_posunit <-x$dzx$hUnit
      if(grepl("in", x_posunit)){
        # x_pos <- x_pos * 0.0254
        x_posunit <- "in"
      }
    }
  }
  antfreq <- getAntFreqGSSI(x$hd$ANT)
 
  is_naFreq <- is.na(antfreq) 
  if(sum( is_naFreq ) > 0){
    # estimate anntenna frequency from the name (it it contains ### MHz)
    antfreq[is_naFreq] <- freqFromString(x$hd$ANT[is_naFreq])
  }
  is_naFreq <- is.na(antfreq) 
  if(all(is_naFreq)){
    y_freq <- seq_along(antfreq)
    y_unit <- ""
  }else{
    y_freq <- antfreq
    y_freq[is_naFreq] <- 0
    y_unit <- "MHz"
  }
  if(sum( is_naFreq ) > 0){
    antfreq[is_naFreq] <- 0
    message("Frequency of ", paste0(x$hd$ANT[is_naFreq], collapse = ", "), 
            " set to 0 MHz.",
            " Set frequency with 'antfreq(x) <- ... '")
  }
  v <- 2 * x$hd$DEPTH / x$hd$RANGE
  
  # antenna sparation could be estimated from frequency...
  # antsep <- antSepFromAntFreq(antfreq)
  antsep <- 0
  message("Antenna separation set to 0 ", x_posunit, 
          ". Set it with 'antsep(x) <- ... '")
  
  if(isFALSE(Vmax)){
    dunit <- "bits"
  }else{
    dunit <- "mV"
  }
  
  
  if(x$hd$NCHAN > 1){
    # print("yep!")
    dimnames(x$data) <- list(NULL, seq_along(x_pos), NULL)
    new("GPRset",   
        #--- class GPRvirtual
        version      = "0.3",  
        name         = x_name,
        path         = fPath,
        desc         = desc,
        mode         = "CO",
        date         = dd,
        freq         = unname(antfreq), 
        
        data         = bits2volt(Vmax = Vmax, nbits = x$hd$BITS) * x$data,     
        dunit        = dunit,  
        dlab         = "amplitude", 
        
        spunit       = "",  
        crs          = NA_character_,  
        
        xunit        = x_posunit,  
        xlab         = "position",
        
        zunit        = x_depthunit,  
        zlab         = "two-way travel time",
        
        vel          = list(v),   
        
        # proc         = "list",
        # delineations = "list",
        md           = x$hd,  
        
        #--- class GPR
        z0           = rep(0, ncol(x$data)),    
        # time         = ,    
        antsep       = antsep,    
        markers      = trimStr(x_fid), 
        # ann          = "character", 
        
        # coord        = coord,     
        # rec          = coord_rec,     
        # trans        = coord_trans,     
        
        x            = x_pos,    
        z            = x$depth[1:nrow(x$data)],  
        
        #--- class GPRset
        y            = y_freq, #seq_len(x$hd$NCHAN),    # y-values, length = p
        yunit        = y_unit,  # set units, length = 1|p
        ylab         = "frequency"  # set names, length = 1|p
        
    )
  }else{
    colnames(x$data) <- 1:ncol(x$data)
    new("GPR",   
        #--- class GPRvirtual
        version      = "0.3",  
        name         = x_name,
        path         = fPath,
        desc         = desc,
        mode         = "CO",
        date         = dd,
        freq         = antfreq, 
        
        data         = bits2volt(Vmax = Vmax, nbits = x$hd$BITS) * x$data[,,1],     
        dunit        = dunit,  
        dlab         = "amplitude", 
        
        spunit       = "",  
        crs          = NA_character_, 
        
        xunit        = x_posunit,  
        xlab         = "position",
        
        zunit        = x_depthunit,  
        zlab         = "two-way travel time",
        
        vel          = list(v),   
        
        # proc         = "list",
        # delineations = "list",
        md           = x$hd,  
        
        #--- class GPR
        z0           = rep(0, ncol(x$data)),    
        # time         = ,    
        antsep       = antsep,    
        markers      = trimStr(x_fid), 
        # ann          = "character", 
        
        # coord        = coord,     
        # rec          = coord_rec,     
        # trans        = coord_trans,     
        
        x            = x_pos,    
        z            = x$depth[1:nrow(x$data)]  
    )
  }
}




#' Read GSSI GPR data
#' 
#' @param dsn [\code{character(1)|connection object}] data source name: 
#'            either the filepath to the GPR data (character),
#'            or an open file connection.
#' @return [\code{list(4)}] \code{hd} header data,
#'         \code{data} GPR data, \code{depth} time or depth, and \code{pos}
#'         position of the traces.
#' @seealso \code{\link{readDZG}}, \code{\link{readDZX}}
#' @name readDZT
#' @rdname readDZT
#' @export
readDZT <- function(dsn){
  
  if(!inherits(dsn, "connection")){
    dsn <- file(dsn, 'rb')
  }
  
  hd <- c()
  MINHEADSIZE <- 1024  # absolute minimum total header size
  nScans <- 0
  #--------------------------------- READ HEADER ------------------------------#  
  # 0x00ff ('\xff\a') if header, 0xfnff for old file
  #rh_tag <- readChar(dsn, nchars = 1, useBytes = TRUE)
  hd$TAG <- .readBin_ushort(dsn)
  # Offset to Data from beginning of file
  #   if rh_data < MINHEADSIZE then offset is MINHEADSIZE * rh_data
  #   else offset is MINHEADSIZE *rh_nchan
  hd$OFFSETDATA <- .readBin_ushort(dsn)   # rh_offsetdata
  # samples per scan
  hd$NSAMP <- .readBin_ushort(dsn)        # rh_nsamp
  # bits per data word (8,16, 32) *
  hd$BITS <- .readBin_ushort(dsn)         # rh_bits
  # Binary_offset
  # if rh_system is SIR 30, then equals repeats/sample
  #     otherwise is 0x80 for 8 bit data and 0x8000 for 16 bit data
  hd$ZERO <-.readBin_short(dsn)           # rh_zero
  # scans per second
  hd$SPS <- .readBin_float(dsn)           # rh_sps
  # scans per meter
  hd$SPM <- .readBin_float(dsn)           # rh_spm
  # meters per mark
  hd$MPM <- .readBin_float(dsn)           # rh_mpm
  # position (ns)
  hd$POSITION <- .readBin_float(dsn)      # rh_position
  # range (ns)
  hd$RANGE <- .readBin_float(dsn)         # rh_range
  # number of passes for 2-D files
  hd$NPASS <- .readBin_ushort(dsn)        # rh_npass
  # creation date
  creaDT <- .readRFDate(dsn, where = 32)
  # modification date
  modDT  <- .readRFDate(dsn, where = 36)
  hd$DATE <- creaDT$date
  hd$TIME <- creaDT$time
  # skip across some proprietary stuff
  seek(dsn, where = 44, origin = "start")
  # offset to text
  hd$OFFSETTEXT <- .readBin_ushort(dsn)   # rh_text
  # size of text
  hd$NTEXT <- .readBin_ushort(dsn)        # rh_ntext
  # offset to processing history
  hd$PROC <- .readBin_ushort(dsn)         # rh_proc
  # size of processing history
  hd$NPROC <- .readBin_ushort(dsn)        # rh_nproc
  # number of channels
  hd$NCHAN <- .readBin_ushort(dsn)        # rh_nchan
  # average dilectric
  hd$EPSR <- .readBin_float(dsn)          # rhf_epsr
  # position in meters (useless?)
  hd$TOP <- .readBin_float(dsn)           # rhf_top
  # range in meters
  hd$DEPTH <- .readBin_float(dsn)        # rhf_depth
  seek(dsn, where = 98, origin = "start")
  # antenna name
  ant_name <- character(hd$NCHAN)
  for(i in seq_len(hd$NCHAN)){
    seek(dsn, where = 98 + MINHEADSIZE * (i - 1), origin = "start")
    ant_name[i] <- suppressWarnings(readChar(dsn, nchars = 14, useBytes = FALSE))
  }
  # hd$ANT <- readChar(dsn, nchars = 14, useBytes = TRUE)
  hd$ANT <- ant_name
  # byte containing versioning bits
  hd$VSBYTE <- .readBin_ushort(dsn) 
  
  #--------------------------------- READ DATA --------------------------------#
  # number of bytes in file
  nB <- .flen(dsn)
  
  # whether or not the header is normal or big-->determines offset to data array
  if( hd$OFFSETDATA < MINHEADSIZE){
    hd$OFFSETDATA <- MINHEADSIZE * hd$OFFSETDATA
  }else{
    hd$OFFSETDATA <- MINHEADSIZE * hd$NCHAN
  }
  
  if(nScans == 0){ # read all the scans
    nNumScans <- (nB - hd$OFFSETDATA)/(hd$NCHAN * hd$NSAMP * hd$BITS/8);
  }
  
  seek(dsn, where = hd$OFFSETDATA, origin = "start")
  
  nNumSkipScans <- 0
  
  if(hd$BITS == 8){
    invisible(readBin(dsn, "integer", n = hd$NSAMP * nNumSkipScans * hd$NCHAN,
                      size = 2L))
    A <- matrix(nrow = hd$NSAMP, ncol = nNumScans * hd$NCHAN)
    A[] <- readBin(dsn, what = "int", n = prod(dim(A)),  size = 1)
    test <- A > 0
    A[ test] <- A[ test] - 129
    A[!test] <- A[!test] + 127
  }else if(hd$BITS == 16){
    #.skipBin(dsn, hd$NSAMP * nNumSkipScans * hd$NCHAN, size = 2)
    invisible(readBin(dsn, "integer", n = hd$NSAMP * nNumSkipScans * hd$NCHAN, 
                      size = 2L))
    A <- matrix(nrow = hd$NSAMP, ncol = nNumScans * hd$NCHAN)
    A[] <- readBin(dsn, what = "int", n = prod(dim(A)),  size = 2)
    test <- A > 0
    A[ test] <- A[ test] - 32769
    A[!test] <- A[!test] + 32767
  }else if(hd$BITS == 32){
    # invisible(readBin(dsn, "integer", n = hd$NSAMP * nNumSkipScans * hd$NCHAN, 
    #                   size = 2L))
    A <- matrix(nrow = hd$NSAMP, ncol = nNumScans * hd$NCHAN)
    A[] <- readBin(dsn, what = "int", n = prod(dim(A)),  size = 4) 
  }
  
  tt <- (seq_len(hd$NSAMP) - 1) * hd$RANGE /  (hd$NSAMP - 1 )
  # yy <- 1/hd$SPM * (seq_len(ncol(A) ) - 1)
  # plot3D::image2D(x = tt, y = yy, z = A)
  
  yy <- 1/hd$SPM * (seq_len(ncol(A) / hd$NCHAN) - 1)
  # Adata <- vector(mode = "list", length = hd$NCHAN)
  Adata <- array(dim = c(length(tt), length(yy), hd$NCHAN))
  for(i in seq_len(hd$NCHAN)){
    # Adata[[i]] <- A[, seq(i, by = hd$NCHAN, to = ncol(A))]
    Adata[ , , i] <- A[, seq(i, by = hd$NCHAN, to = ncol(A))]
    if(i == 1){
      hd$MRKS <- Adata[2,,1]
      Adata[1:2, , 1] <- 0   
    }
    # plot3D::image2D(y = tt[1:nrow(Adata[[i]])], x = yy, 
    # z = t(Adata[[i]][nrow(Adata[[i]]):1,]))
  }
  
  .closeFileIfNot(dsn)
  return(list(hd = hd, data = Adata, depth = tt, pos = yy))
}

# setwd("/media/huber/Seagate1TB/UNIBAS/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT")
# dsn <- "dzt/jarvis/PROJECT001__014.DZG"
# dsn <- "dzt/jarvis/PROJECT001__014.DZT"
# mrk <- readDZG(dsn)

#' Read GSSI GPS data
#' 
#' @param dsn [\code{character(1)|connection object}] data source name: 
#'            either the filepath to the GPR data (character),
#'            or an open file connection.
#' @return [\code{data.frame(,5)}] position (\code{x}, \code{y}, \code{z}),
#'         trace id (\code{id}), and time (\code{time}).
#' @seealso \code{\link{readDZT}}, \code{\link{readDZX}}
#' @name readDZG
#' @rdname readDZG
#' @export
readDZG <- function(dsn){
  x <- scan(dsn, what = character(), sep = "\n", quiet = TRUE)
  
  test_gssis <- grepl("(\\$GSSIS)", x, ignore.case = TRUE, useBytes = TRUE )
  test_gpgga <- grepl("(\\$GPGGA)", x, ignore.case = TRUE, useBytes = TRUE )
  
  if(sum(test_gssis) != sum(test_gpgga)){
    stop("File '.dzg' is corrupted! I cannot read it... sorry.")
  }
  
  pat_gssis <- paste0("\\$(?<ID>GSSIS),(?<tr>[0-9]+),(?<time>[-]?[0-9.]+)") 
  pat_gpgga <- paste0("\\$(?<ID>GPGGA),(?<UTC>[0-9.]+),(?<lat>[0-9.]+),",
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
  gpgga <- extractPattern(x[test_gpgga], pattern = pat_gpgga, 
                          start = 0, stop = -1)  
  gssis <- extractPattern(x[test_gssis], pattern = pat_gssis, 
                          start = 0, stop = -1)
  
  dim(gpgga) <- c(sum(test_gpgga), 11)
  gpgga <- as.data.frame(gpgga, stringsAsFactors = FALSE)
  colnames(gpgga) <- c("ID", "UTC", "lat", "NS", "lon", "EW", 
                       "fix", "NbSat", "HDOP", "H", "mf")
  dim(gssis) <- c(sum(test_gssis), 3)
  gssis <- as.data.frame(gssis, stringsAsFactors = FALSE)
  colnames(gssis) <- c("ID", "trace", "time")
  
  xyzt <- .getLonLatFromGPGGA(gpgga)
  
  # trace number start at 0!!
  mrk <- cbind(xyzt[ ,1:3], as.integer(gssis$trace) + 1,  xyzt[ ,4])
  # mrk <- as.matrix(mrk)
  names(mrk) <- c("x", "y", "z", "id", "time")
  
  .closeFileIfNot(dsn)
  return(mrk)
}

#' Read GSSI's .dzx file
#' 
#' .dzx files are xml files
#' @param dsn [\code{character(1)|connection object}] data source name: 
#'            either the filepath to the GPR data (character),
#'            or an open file connection.
#' @return [\code{list}] contains the markers, the trace position and 
#'         the spatial sampling.
#' @seealso \code{\link{readDZT}}, \code{\link{readDZG}}
#' @name readDZX
#' @rdname readDZX
#' @export
readDZX <- function(dsn){
  
  if(!inherits(dsn, "connection")){
    dsn <- file(dsn, 'rb')
  }
  
  xmltxt <-  verboseF(readLines(dsn), verbose = FALSE)
  if(length(xmltxt) == 0){
    .closeFileIfNot(dsn)
    return(NULL)
  }
  doc <- verboseF(XML::xmlParse(xmltxt),  verbose = FALSE)
  
  lst <- list()
  
  glbProp <- XML::xmlChildren(doc)$DZX[["GlobalProperties"]]
  if(!is.null(glbProp)){
    unitsPerMark <- XML::xmlElementsByTagName(glbProp, "unitsPerMark")
    if(length(unitsPerMark) > 0){
      unitsPerMark <- as.numeric(XML::xmlValue(unitsPerMark[[1]]))
      if(unitsPerMark > 0){
        lst$unitsPerMark <- unitsPerMark
      }
    }
    unitsPerScan <- XML::xmlElementsByTagName(glbProp, "unitsPerScan")
    if(length(unitsPerScan) > 0){
      unitsPerScan <- as.numeric(XML::xmlValue(unitsPerScan[[1]]))
      if(unitsPerScan > 0){
        lst$unitsPerScan <- unitsPerScan
      }
    }
    vUnit <- XML::xmlElementsByTagName(glbProp, "verticalUnit")
    if(length(vUnit) > 0){
      vUnit <- XML::xmlValue(vUnit[[1]])
      lst$vUnit <- vUnit
    }
    hUnit <- XML::xmlElementsByTagName(glbProp, "horizontalUnit")
    if(length(hUnit) > 0){
      hUnit <- XML::xmlValue(hUnit[[1]])
      lst$hUnit <- hUnit
    }
  }
  
  # Scan range !!
  # FIXME : multi channel files
  fl <- XML::xmlChildren(doc)$DZX[["File"]]
  if(!is.null(fl)){
    s1 <- XML::xmlElementsByTagName(fl, "scanRange", recursive = TRUE)
    if(length(s1) > 0){
      s0 <- as.integer(strsplit(XML::xmlValue(s1[[1]]), split = ",")[[1]])
      nscans <- length(s0[1]:s0[2])
      #--- select all the distance tags
      dst <- XML::xmlElementsByTagName(fl, "distance", recursive = TRUE)
      #--- select the sibling tags "scan" and "mark"
      # here I assume that all tags "distance" have a sibling tag "mark" and "scan"
      if(length(dst) > 0){
        f <- function(x){
          papa <- XML::xmlParent(x)
          i1 <- as.numeric(XML::xmlValue(XML::xmlElementsByTagName(papa, "scan")))
          i2 <- XML::xmlValue(XML::xmlElementsByTagName(papa, "mark"))
          if(length(i2) == 0) i2 <- ""
          i3 <- as.numeric(XML::xmlValue(x))  # distance
          return(unname(c(i1, i2, i3)))
        }
        uu <- sapply(dst, f, USE.NAMES = FALSE)
        if(inherits(uu, "matrix")){
          id <- as.integer(uu[1, ]) + 1L
          pos <- as.numeric(uu[3,])
          lst$dx <- mean(diff(pos)/ (diff(id) - 1))
          lst$pos <- approx(id, pos, seq_len(nscans))$y
          lst$markers <- character(length = nscans)
          lst$markers[id] <- uu[2,]
        }else{
          message("I was unable to read the markers in the file *.dzx")
        }
        
      }
    }
    .closeFileIfNot(dsn)
  
    if(length(lst) > 0){
      return(lst)
    }else{
      return(NULL)
    }
  }
}

.xmlValueSibling <- function(x, after = FALSE){
  XML::xmlValue(XML::getSibling(x, after = after))
}

.readRFDate <- function(con, where = 31){
  seek(con, where = where, origin = "start")
  rhb_cdt0 <- readBin(con, what = "raw", n = 4L, size = 1L, endian = "little")
  
  aa <- rawToBits(rhb_cdt0)
  xdate <- paste(.bit2int(aa[25 + (7:1)]) + 1980, 
                 sprintf("%02d", .bit2int(aa[21 + (4:1)])),  # sprintf()
                 sprintf("%02d", .bit2int(aa[16 + (5:1)])), sep = "-")
  xtime <- paste(sprintf("%02d", .bit2int(aa[11 + (5:1)])),
                 sprintf("%02d", .bit2int(aa[5 + (6:1)])),
                 sprintf("%02d", .bit2int(aa[5:1])* 2), sep = ":" )
  return(list(date = xdate, time = xtime))
}
