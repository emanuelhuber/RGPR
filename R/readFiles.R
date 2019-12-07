#' Read a GPR data file
#' 
#' Note: argument \code{fPath} is depreacted. Use \code{dsn} instead.
#' 
#' Supported file format
#' \itemize{
#'   \item Sensors & Software file format (*.dt1 , *.hd).
#'         \code{readGPR(dsn = 'xline.dt1')}
#'   \item MALA file format (*.rd3, *.rad).
#'         \code{readGPR(dsn = 'xline.rd3')}
#'   \item RadSys Zond GPR device (*.sgy). 
#'         \strong{Note: it is not the SEG-Y file format)}.
#'         \code{readGPR(dsn = 'xline.sgy')}  
#'   \item GSSI file format (*.dzt).
#'         \code{readGPR(dsn = 'xline.dzt')}
#'   \item ASCII file format (*.txt): either 4-column format 
#'         (x,t,amplitude) or matrix-format (without header/rownames).
#'         \code{readGPR(dsn = 'xline.txt')}  
#'   \item R object file format (*rds). These files are created by saving the
#'         \code{GPR} object with 
#'         \code{writeGPR(x, fPath = 'xline.rds', type = "rds")}.
#'         \code{readGPR(dsn = 'xline.txt')}  
#' }
#' TO DO:
#' \itemize{
#'   \item Impulse Radar: read markers file
#'   \item ...
#' }
#' @param dsn data source name: either the filepath to the GPR data (character),
#'            or an open file connection (can be a list of filepahts or
#'            open file connections)
#' @param desc Short description of the file (character).
#' @param dsn2 deprecated
#' @param format lenth-one character vector required if the file extension is
#'               not appearent in the filepath or the connection (either
#'               \code{dt1}, \code{rad}, \code{dzt}, \code{sgy}, \code{iprb},
#'               \code{txt}, \code{rds})
#' @param Vmax length-one numeric vector: nominal analog input voltage used 
#'             for the bits to volt transformation. 
#'             It assumes that \code{Vmin = -Vmax}. If \code{Vmax = NULL},
#'             no bits to Volt transformation is applied.
#' @param fPath Filepath (character). DEPRECATED. Use \code{dsn} instead.
#' @param ch For multi-frequency GSSI files (*.dzt), which channel is red.
#' @param verbose (boolean). If \code{FALSE}, all messages and warnings are
#'                suppressed (use with care).
#' @param interp_pos logical: should the trace position be interpolated if possible? TRUE or FALSE
#' @param method A length-three character vector defining the interpolation
#'               methods (same methods as in \code{signal::interp1}:
#'               "linear", "nearest", "pchip", "cubic", and "spline"). 
#'               First element for the interpolation of the 
#'               inter-trace distances, 
#'               second element for the interpolation of the horizontal 
#'               trace positions, and third element for the interpolation
#'               of the vertical trace positions.
#' @return The GPR data as object of the class RGPR.
#' @seealso \code{\link{writeGPR}}
#' @examples
#' \dontrun{
#' # argument dsn is a file path
#' x1 <- readGPR(dsn = "data/RD3/DAT_0052.rd3")
#' y1 <- readGPR("data/FILE____050.DZT")
#' 
#' # argument dsn is a connection
#' con <- file("data/RD3/DAT_0052.rd3", "rb")   # binary mode
#' con2 <- file("data/RD3/DAT_0052.rad", "rt")  # text mode
#' x2 <- readGPR(dsn = con, dsn2 = con2)
#' close(con)
#' close(con2)
#' 
#' con <- file(dsn = "data/FILE____050.DZT", "rb")
#' y1 <- readGPR(con)
#' close(con)
#' }
#' @name readGPR
#' @rdname readGPR
#' @export
readGPR <- function(dsn, desc = "", dsn2 = NULL, format = NULL, Vmax = 50,
                    fPath, ch = 1, verbose = TRUE, interp_pos = TRUE, 
                    method = c("linear", "linear", "linear")){
  
  if(!missing(fPath)){
    if(missing(dsn)){
      dsn <- fPath
    }
    warning("Use argument 'dsn' instead of 'fPath' because ",
            "argument 'fPath' is deprecated.")
  }
  
  if(!is.null(dsn2)){
    warning("Argument 'dsn2' is deprecated. Use instead ",
            "'dsn = list(dsn1, dsn2)")
  }
  
  dsn <- Filter(Negate(is.null), dsn)
  # file path, name and extension for character or connection
  fPath <- sapply(dsn, getFPath, USE.NAMES = FALSE)
  ext <- sapply(fPath, .fExt, USE.NAMES = FALSE)
  fName <- sapply(fPath, .fNameWExt, USE.NAMES = FALSE)
  if(!is.list(dsn)) dsn <- as.list(dsn)
  names(dsn)   <- toupper(ext)
  names(fName) <- toupper(ext)
  names(fPath) <- toupper(ext)
  
  
  #--------------------------- SENSORS & SOFTWARE -----------------------------#
  #--------------------------- DT1 + HD (+ GPS) -------------------------------#
  if("DT1" %in% toupper(ext)){
    if(length(dsn) == 1){
      if(inherits(dsn, "connection")){
        stop("Please add an additional connection to 'dsn' in 'readGPR()' for ",
             "the header file '*.hd'")
      }
      # get HD (+ GPS) file(s)
      dsn <- list(DT1 = getFName(fPath[1], ext = ".DT1")$dt1,  # dsn, 
                  HD  = getFName(fPath[1], ext = ".HD")$hd, 
                  GPS = getFName(fPath[1], ext = ".GPS", throwError = FALSE)$gps)
    }else if( !("HD" %in% toupper(ext))){
      stop("Missing connection or filepath to '*.hd' file.") 
    }else if( !("GPS" %in% toupper(ext)) ){
      dsn <- list(DT1 = dsn[["DT1"]], 
                  HD = dsn[["HD"]],
                  GPS = getFName(fPath[1], ext = ".GPS", throwError = FALSE)$gps)
    }
    hd  <- verboseF( readHD(dsn[["HD"]]), verbose = verbose)
    dt1 <-  verboseF( readDT1(dsn[["DT1"]], ntr = hd$ntr, npt = hd$npt), 
                      verbose = verbose)
    x <- verboseF(.gprDT1(list(hd = hd$HD, dt1 = dt1$dt1hd, data = dt1$data ), 
                          fName = fName[["DT1"]], fPath = fPath[["DT1"]], 
                          desc = desc, Vmax = Vmax),  verbose = verbose)
    if( !is.null(dsn[["GPS"]]) && isTRUE(interp_pos)){
      x <- tryCatch({
              gps <-  verboseF(readGPS(dsn[["GPS"]]), verbose = verbose)
              if(!is.null(gps)){
                x <- interpPos(x, gps, tol = sqrt(.Machine$double.eps), 
                               method = method)
                crs(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
                x
              }else{
                x
              }
            },
            error = function(cond) {
              message("I could not either read your GPS data ",
                      "or interpolate the trace position.")
              # Choose a return value in case of error
              return(x)
            }#,
            # warning = function(cond) {
            #   message(paste("URL caused a warning:", url))
            #   message("Here's the original warning message:")
            #   message(cond)
            #   # Choose a return value in case of warning
            #   return(NULL)
            # },
            # finally={
            #   # NOTE:
            #   # Here goes everything that should be executed at the end,
            #   # regardless of success or error.
            #   # If you want more than one expression to be executed, then you 
            #   # need to wrap them in curly brackets ({...}); otherwise you could
            #   # just have written 'finally=<expression>' 
            #   message(paste("Processed URL:", url))
            #   message("Some other message at the end")
            # }
            )    
    }
    # plot(x)
    
  #----------------------------------- MALA -----------------------------------#
  #-------------------------- RD3 + RAD (+ COR) -------------------------------#
  }else if( any( c("RD3", "RD7") %in% toupper(ext) ) ){
    if(length(dsn) == 1){
      if(inherits(dsn, "connection")){
        stop("Please add an additional connection to 'dsn' in 'readGPR()' for ",
             "the header file '*.rad'")
      }
      # get RAD (+ COR) file(s)
      dsn <- list(RD37 = getFName(fPath[1], ext = paste0(".", toupper(ext)))[[tolower(ext)]], #dsn, 
                  RAD  = getFName(fPath[1], ext = ".RAD")$rad, 
                  COR  = getFName(fPath[1], ext = ".COR", throwError = FALSE)$cor)
    }else if( !("RAD" %in% toupper(ext))){
      stop("Missing connection or filepath to '*.rad' file.") 
    }else if( !("COR" %in% toupper(ext)) ){
      dsn[["COR"]] <- getFName(fPath[1], ext = ".COR", throwError = FALSE)$cor
    }
    nbytes <- 4
    i <- which("RD7" == toupper(ext) )
    if(length(i) == 0){
      i <- which("RD3" == toupper(ext) )
      nbytes <- 2
    } 
    dsn[["RD37"]] <- dsn[[i]] # FIXME: 
    # assume that the ext is correct 
    # (upper/lower case)
    rad  <- verboseF( readRAD(dsn[["RAD"]]), verbose = verbose)
    rd37 <-  verboseF( readRD37(dsn[["RD37"]], ntr = rad$ntr, npt = rad$npt, 
                                nbytes = nbytes), verbose = verbose)
    
    x <- verboseF( .gprRD3(list(hd = rad$HD, data = rd37), 
                                    fName = fName[[i]], fPath = fPath[[i]],  
                                    desc = desc, nbits = 8*nbytes, Vmax = Vmax), 
                   verbose = verbose)
    if( !is.null(dsn[["COR"]]) ){
      x <- tryCatch({
              x_cor <-  verboseF(readCOR(dsn[["COR"]]), verbose = verbose)
              if(!is.null(x_cor) && isTRUE(interp_pos)){
                x <- interpPos(x, x_cor, tol = sqrt(.Machine$double.eps), 
                               method = method)
                crs(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
                x
              }else{
                x
              }
            },
            error = function(cond) {
              message("I could not either read your GPS data ",
                      "or interpolate the trace position.")
              # Choose a return value in case of error
              return(x)
            })
    }
  #---------------------------SEG-Y +  EASY RADAR -----------------------------#
  #------------------------------- SEG/SEG-Y ----------------------------------#
  }else if(any(c("SGY", "SEGY") %in% toupper(ext))){
    i <- which(grepl("SGY|SEGY", toupper(ext)))[1]
    if( !inherits(dsn[[i]], "connection") ){
      dsn <- file(dsn[[i]], "rb")
    }
    ENDIAN <- "big"
    THD <- readSGY_textual_file_header(dsn, ENDIAN = "big")
    test <- FALSE
    # if(all(validEnc(THD))){
    test <- any(verboseF(grepl("Prism", THD), verbose = FALSE)) & 
      any(verboseF(grepl("Radar Systems, Inc.", THD), 
                   verbose = FALSE))
    # }
    # read RadSys Zond System
    if( test ){
      A <- verboseF( readSEGY_RadSys_Zond_GPR(dsn), verbose = verbose)
      x <- verboseF( .gprSEGY(A, fName = fName, fPath = fPath, 
                              desc = desc, Vmax = Vmax), verbose = verbose)
      # read classical SEG-Y file
    }else{
      A <- verboseF(readSGY(dsn), verbose = verbose)
      x <- verboseF( .gprSGY(A, fName = fName, fPath = fPath, 
                             desc = desc, Vmax = Vmax), verbose = verbose)
      return(x)
    }
    # plot(x)
    # A <- tryCatch({verboseF(readSGY(dsn), 
    #                         verbose = verbose)},
    #               error = function(e){return(NULL)})
    # if(is.null(A)){
    #   # z <- readGPR(dsn)
    #   if(in)
    #   A <- verboseF( readSEGY_RadSys_Zond_GPR(dsn), verbose = verbose)
    #   x <- verboseF( .gprSEGY(A, fName = fName, fPath = fPath, 
    #                           desc = desc, Vmax = Vmax), verbose = verbose)
    # }else{
    #   x <- verboseF( .gprSGY(A, fName = fName, fPath = fPath, 
    #                          desc = desc, Vmax = Vmax), verbose = verbose)
    #   return(x)
    # }
  #---------------------------- IMPULSE RADAR ---------------------------------#
  #---------------------- IPRB + IPRH (+ COR + TIME + MRK) --------------------#
  }else if("IPRB" %in% toupper(ext)){
    
    # fName <- getFName(dsn, ext = c(".iprh", ".iprb"))
    # #--- READ OPTIONAL FILES
    # fNameOpt <- getFName(dsn, ext = c(".cor", ".time", ".mrk"), 
    #                      throwError = FALSE)
    
    if(length(dsn) == 1){
      if(inherits(dsn, "connection")){
        stop("Please add an additional connection to 'dsn' in 'readGPR()' for ",
             "the header file '*.iprh'")
      }
      # get HD (+ GPS) file(s)
      dsn <- list(IPRB = getFName(fPath[1], ext = ".iprb")$iprb, #dsn, 
                  IPRH = getFName(fPath[1], ext = ".iprh")$iprh, 
                  COR  = getFName(fPath[1], ext = ".cor",  throwError = FALSE)$cor,
                  TIME = getFName(fPath[1], ext = ".time", throwError = FALSE)$time,
                  MRK  = getFName(fPath[1], ext = ".mrk",  throwError = FALSE)$mrk
      )
    }else if( !("IPRH" %in% toupper(ext))){
      stop("Missing connection or filepath to '*.iprh' file.") 
      # FIXME: adapt below... 
    }else{
      dsn <- list(IPRB = dsn[["IPRB"]], 
                  IPRH = dsn[["IPRH"]])
      if( !("COR" %in% toupper(ext)) ){
        dsn <- c(dsn, list(COR = getFName(fPath[1], ext = ".cor", throwError = FALSE)$cor))
      }
      if( !("TIME" %in% toupper(ext)) ){
        dsn <- c(dsn, list(TIME = getFName(fPath[1], ext = ".time", throwError = FALSE)$time))
      }
      if( !("MRK" %in% toupper(ext)) ){
        dsn <- c(dsn, list(MRK = getFName(fPath[1], ext = ".mrk", throwError = FALSE)$mrk))
      }
    }
    hd   <- verboseF( readIPRH(dsn[["IPRH"]]), verbose = verbose)
    iprb <- verboseF( readIPRB(dsn[["IPRB"]], ntr = hd$ntr, npt = hd$npt, 
                               nbytes = hd$nbytes), verbose = verbose)
    iprball <- list(hd = hd$HD, data = iprb)
    if( !is.null(dsn[["TIME"]])){
      iprball <- c(iprball, list(time = readTIME(dsn[["TIME"]])))
    }
    if( !is.null(dsn[["MRK"]])){
      iprball <- c(iprball, list(mrk = readMRK(dsn[["MRK"]])))
    }
    x <- verboseF(.gprImpulseRadar(iprball, 
                          fName = fName[["IPRB"]], fPath = fPath[["IPRB"]], 
                          desc = desc, Vmax = Vmax),  verbose = verbose)
    if( !is.null(dsn[["COR"]]) && isTRUE(interp_pos)){
      x <- tryCatch({
              x_cor <-  verboseF(readIPRCOR(dsn[["COR"]]), verbose = verbose)
              x <- interpPos(x, x_cor, tol = sqrt(.Machine$double.eps), method = method)
              crs(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
              x
            },
            error = function(cond) {
              message("I could not either read your GPS data ",
                      "or interpolate the trace position.")
              # Choose a return value in case of error
              return(x)
            })
    }
  #--------------------------------- GSSI -------------------------------------#
  #------------------------------ DZT (+ DZX, DZG) ----------------------------#
  }else if("DZT" %in% toupper(ext)){
    if(length(dsn) == 1){
      if(inherits(dsn, "connection")){
        stop("Please add an additional connection to 'dsn' in 'readGPR()' for ",
             "the header file '*.iprh'")
      }
      dsn <- list(DZT = dsn[["DZT"]], 
                  DZX = getFName(fPath[1], ext = ".DZX", throwError = FALSE)$dzx,
                  DZG = getFName(fPath[1], ext = ".DZG", throwError = FALSE)$dzg)
    }else{
      if( !("DZX" %in% toupper(ext)) ){
        dsn <- c(dsn, list(DZX = getFName(fPath[1], ext = ".DZX", throwError = FALSE)$dzx))
      }
      if( !("DZG" %in% toupper(ext)) ){
        dsn <- c(dsn, list( DZG = getFName(fPath[1], ext = ".DZG", throwError = FALSE)$dzg))
      }
      
    }
    dzt <- verboseF( readDZT(dsn[["DZT"]]), verbose = verbose)
    if(!is.null(dsn[["DZX"]])){
      dzx <- verboseF( readDZX(dsn[["DZX"]]), verbose = verbose)
      dzt <- c(dzt, list(dzx = dzx))
    }
    x <- verboseF( .gprDZT(dzt, 
                           fName = fName[["DZT"]], fPath = fPath[["DZT"]], 
                           desc = desc, Vmax = Vmax, ch = ch), verbose = verbose)
    if( !is.null(dsn[["DZG"]]) && isTRUE(interp_pos)){
      x <- tryCatch({
              x_mrk <-  verboseF(readDZG(dsn[["DZG"]]), verbose = verbose)
              x <- interpPos(x, x_mrk, tol = sqrt(.Machine$double.eps), 
                             method = method)
              crs(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
              x
            },
            error = function(cond) {
              message("I could not either read your GPS data ",
                      "or interpolate the trace position.")
              # Choose a return value in case of error
              return(x)
            })
    }
  #------------------------------- 3d RADAR -----------------------------------#
  #------------------------------------ VOL -----------------------------------#
  }else if("VOL" %in% toupper(ext)){
    A <- verboseF( readVOL(dsn), verbose = verbose)
    x <- verboseF( .gprVOL(A, fName = fName, fPath = fPath, 
                           desc = desc, Vmax = Vmax), verbose = verbose)
    if(A$hd$dim == "3D"){
      warning("return a 'GPRcube' object with complex numbers.",
              " Current processing and plotting functions will most likely not ",
              "work on the returned object. Please contact me:\n",
              "emanuel.huber@alumni.ethz.ch")
    }else{
      warning("Still experimental. Don't hesitate to contact me:\n",
              "emanuel.huber@alumni.ethz.ch")
    }
    return(x)
  #------------------------------- UTSI ---------------------------------------#
  #---------------------- DAT + HDR (+ GPS + GPT) -----------------------------#
  }else if("DAT" %in% toupper(ext)){
    if(length(dsn) == 1){
      if(inherits(dsn, "connection")){
        stop("Please add an additional connection to 'dsn' in 'readGPR()' for ",
             "the header file '*.hdr'")
      }
      dsn <- list(DAT = getFName(fPath[1], ext = ".DAT")$dat,  # dsn, 
                  HDR = getFName(fPath[1], ext = ".HDR")$hdr, 
                  GPS = getFName(fPath[1], ext = ".GPS", throwError = FALSE)$gps,
                  GPT = getFName(fPath[1], ext = ".GPT", throwError = FALSE)$gpt)
    }else if( !("HDR" %in% toupper(ext))){
      stop("Missing connection or filepath to '*.hdr' file.") 
    }else{
        dsn <- list(DAT = dsn[["DAT"]], HDR = dsn[["HDR"]])
      if( !("GPS" %in% toupper(ext)) ){
        dsn <- c(dsn, list( GPS = getFName(fPath[1], ext = ".GPS", throwError = FALSE)$gps))
      }
      if( !("GPT" %in% toupper(ext)) ){
        dsn <- c(dsn, list( GPT = getFName(fPath[1], ext = ".GPT", throwError = FALSE)$gpt))
      }
    }
    # A <- verboseF( readUtsi(dsn), verbose = verbose)
    # x <- verboseF( .gprUtsi(A, fName = fName, fPath = fPath, 
    #                         desc = desc, Vmax = Vmax), verbose = verbose)
    
    hd <- readUtsiHDR(dsn[["HDR"]]) 
    z <- readUtsiDat(dsn[["DAT"]], splPerScan = hd$splPerScan, bits = hd$bits)
    # z[["hd"]] <- hd
    
    x <- verboseF( .gprUtsi(c(z, list(hd = hd)), 
                            fName = fName[["DAT"]], fPath = fPath[["DAT"]], 
                           desc = desc, Vmax = Vmax), verbose = verbose)
    if( !is.null(dsn[["GPS"]]) && !is.null(dsn[["GPT"]])  && isTRUE(interp_pos)){
      x <- tryCatch({
              gpt <- verboseF(readUtsiGPT(dsn[["GPT"]]), verbose = verbose)
              if(length(gpt) > 0 ){
                x_cor <-  verboseF(readUtsiGPS(dsn[["GPS"]], gpt), 
                                   verbose = verbose)
                x <- interpPos(x, x_cor, tol = sqrt(.Machine$double.eps), 
                               method = method)
                crs(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
                x
              }else{
                x
              }
            },
            error = function(cond) {
              message("I could not either read your GPS data ",
                      "or interpolate the trace position.")
              # Choose a return value in case of error
              return(x)
            })
    }
  }else if("TXT" %in% toupper(ext)){
    # fName <- .fNameWExt(fPath)
    A <- verboseF( readTXT(dsn[["TXT"]]), verbose = verbose)
    x <- verboseF( .gprTXT(A, fName = fName, fPath = fPath, 
                           desc = desc, Vmax = Vmax), verbose = verbose)
  
  }else if("RDS" %in% toupper(ext)){
    x <- verboseF( readRDS(dsn[["RDS"]]), verbose = verbose)
    if(class(x) == "GPR"){
      x@filepath <- fPath
    }else if(class(x) == "list"){
      versRGPR <- x[["version"]]
      if(versRGPR == "0.1"){
        for(i in seq_along(x[['delineations']])){
          x[['delineations']][[i]][, 5] <- -x[['delineations']][[i]][, 5]
        }
      }
      y <- new("GPR",
               version = x[['version']],
               data = x[['data']],
               traces = x[['traces']],           # x$dt1$traces
               depth = x[['depth']],
               pos = x[['pos']],                 # x$dt1$position of the traces
               time0 = x[['time0']],             # x$dt1$time0
               time = x[['time']],               # x$dt1$time
               fid = trimStr(x[['fid']]),        # x$dt1$fid <-> x$dt1$x8
               ann = trimStr(x[['ann']]),        # x$dt1$fid <-> x$dt1$x8
               coord = x[['coord']],             # x$dt1$topo  of the traces
               rec = x[['rec']],                # x$dt1$recx,x$dt1$recy,x$dt1$recz
               trans = x[['trans']],
               coordref = x[['coordref']],       # x$dt1$topo of the traces
               freq = x[['freq']],
               dz = x[['dz']],
               dx = x[['dx']],
               antsep = x[['antsep']],
               name = x[['name']],
               description = x[['description']],
               filepath =x[['filepath']],
               depthunit = x[['depthunit']],
               posunit = x[['posunit']],
               surveymode = x[['surveymode']],
               date = x[['date']],
               crs = x[['crs']],
               proc = x[['proc']],               # processing steps
               vel = x[['vel']],                 # m/ns
               delineations = x[['delineations']],
               hd =  x[['hd']]                   # header
      )
      y@filepath <- fPath
      x <- y
    }
  }else{
    stop(paste0("File extension not recognised!\n",
                "Must be '.DT1', '.dzt', '.rd3', '.sgy', '.segy', '.rds'\n",
                "'.iprb', '.iprh', '.dat', or '.vol'."))
  }
  if(grepl("CMP", x@surveymode)){
    x@surveymode <- "CMP"
    if(length(x@rec) == 0 || length(x@trans) == 0){
      x@antsep <- seq(x@antsep, by = x@dx, length.out = length(x))
    }else{
      x@antsep <- sqrt(colSums((x@rec - x@trans)^2))
    }
  }
  return(x)
}



#' Extract frequency from string
#' 
#' Extract with regex the antenna frequency in a string
#' @export
freqFromString <- function(s){
  if(grepl("MHz", s, ignore.case = TRUE)){
    a <- regexpr("[0-9]+.MHZ",  s, ignore.case = TRUE, perl = TRUE)
  }else{
    a <- regexpr("[0-9]+",  s, ignore.case = TRUE, perl = TRUE)
  }
  b <- regmatches(s,  a)
  as.numeric(gsub("[^0-9]", "", b))
}
# s <- "1230 fds 200-MHZ 12.3"
# s <- "1230MLF"
# s <- "D1230MLF"
# freqFromString(s)


#' @export
antSepFromAntFreq <- function(antfreq, verbose = TRUE){
  ant <- list(f = c(12.5, 25, 50, 100, 110, 200, 225, 450,   900, 1200),
              s = c( 8,    4,  2,   1,   1, 0.5, 0.5, 0.25, 0.17, 0.075))
  antsep <- approx(ant$f, ant$s, xout = antfreq)$y
  antsep <- round(antsep, 3)
  if(verbose){
    message("Antenna separation (", antsep, " m) estimated from antenna", 
            " frequency (", antfreq, " MHz).",
            "\nCorrect if wrong with 'antsep(x) <- ...'")
  }
  
  if(is.na(antsep)) antsep <- numeric(0)
  return(antsep)
}




# A = GPR$hd
# if position = TRUE, return in addition the row number
# if number = TRUE, try to convert the value into a number
.getHD <- function(A, string, number = TRUE, position = FALSE){
  if(number){
    value <- as.numeric(A[trimStr(A[,1]) == string, 2])
  }else{
    value <- A[trimStr(A[,1]) == string,2]
  }
  if(length(value)>0){
    if(position){
      pos <- which((trimStr(A[,1]) == string ) == TRUE)[1]
      return(c(value, pos))
    }else{
      return(value)
    }
  }else{
    return(NULL)
  }
}

getFPath <- function(x){
  if(inherits(x, "connection")){
    summaryCon <- summary.connection(x)
    return( summaryCon$description )
  }else{
    return(x)
  }
}

# open file if not already openend
.openFileIfNot <- function(dsn){
  if(!inherits(dsn, "connection")){
    return( file(dsn, 'rb') )
  }else{
    return(dsn)
  }
}

.closeFileIfNot <- function(dsn){
  if(inherits(dsn, "connection")){
    close(dsn )
  }
}


#-----------------------------------------
# bit to integer conversion
.bit2int <- function(x){
  sum(as.integer(x) * 2^( rev(seq_along(x)) - 1))
}

# number of bytes in connection
# file.info(filename)$size
.flen <- function(con){
  pos0 <- seek(con)
  seek(con,0,"end")
  pos <- seek(con)
  seek(con,where=pos0,"start")
  return(pos)
}

# unsigned integer, 2 bytes
.readBin_int1 <- function(x, n = 1){
  readBin(x, what = "int", n = n, size = 1, signed = FALSE, endian = "little")
}

# unsigned integer, 2 bytes
.readBin_ushort <- function(x, n = 1){
  readBin(x, what = "int", n = n, size = 2, signed = FALSE, endian = "little")
}

# signed integer, 2 bytes
.readBin_short <- function(x){
  readBin(x, what = "int", size = 2, signed = TRUE, endian = "little")
}

# float, 4 bytes
.readBin_float <- function(x){
  readBin(x, what = "numeric", size = 4, endian = "little")
}

# float, 8 bytes
.readBin_float8 <- function(x){
  readBin(x, what = "numeric", size = 8, endian = "little")
}

# char, 4 bytes
.readBin_char <- function(x, n = 1){
  readBin(x, what = "character", n = n, size = 1, endian = "little")
}

#  returns the current position in the specified file/connection con
.ftell <- function(con){
  return(seek(con))
}

# .skipBin <- function(con, n, size = 1L){
#   if(n > 0) invisible(readBin(con, "integer", n = n, size = size))
# }
