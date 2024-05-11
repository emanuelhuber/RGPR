#' Read a GPR data file
#' 
#' Note: argument \code{fPath} is depreacted. Use \code{dsn} instead.
#' 
#' Supported file format
#' \itemize{
#'   \item Sensors & Software file format (*.dt1 , *.hd, *.gps).
#'         \code{readGPR(dsn = 'xline.dt1')}
#'   \item MALA file format (*.rd3/*.rd7, *.rad, *.cor).
#'         \code{readGPR(dsn = 'xline.rd3')}
#'   \item ImpulseRadar file format  (*.iprb, *.iprh, *.cor, *.time).
#'         \code{readGPR(dsn = 'xline.iprb')}
#'   \item GSSI file format (*.dzt, *.dzx, *.dzg).
#'         \code{readGPR(dsn = 'xline.dzt')}
#'   \item Geomatrix Earth Science Ltd file format (*.sgpr).
#'         \code{readGPR(dsn = 'xline.sgpr')}
#'   \item Transient Technologies (*.dat, *.hdr, *.gpt, *.gps).
#'   \item IDS (*.dt, *.gec).
#'         \code{readGPR(dsn = 'xline.dt')}
#'   \item SEG-Y file format (*.sgy/*.segy).
#'         \code{readGPR(dsn = 'xline.sgy')} 
#'   \item SEG-2 file format (*.sg2/*.seg2).
#'         \code{readGPR(dsn = 'xline.sg2')}  
#'   \item RadSys Zond GPR device (*.sgy). 
#'         \strong{Note: it is not the SEG-Y file format)}.
#'         \code{readGPR(dsn = 'xline.sgy')}
#'   \item US Radar (*.RA1*, *RA2*, *RAD*). 
#'         \code{readGPR(dsn = 'xline.RA1')}  
#'   \item ASCII file format (*.txt): either 4-column format 
#'         (x,t,amplitude) or matrix-format (without header/rownames).
#'         \code{readGPR(dsn = 'xline.txt')}  
#'   \item GPRmax file format (*.out): hdf5
#'   \item R object file format (*rds). These files are created by saving the
#'         \code{GPR} object with 
#'         \code{writeGPR(x, fPath = 'xline.rds', type = "rds")}.
#'         \code{readGPR(dsn = 'xline.txt')}  
#' }
#' 
#' @section GSSI files (*.dzt):
#' The first trace sample is constant and does not belong to the signal 
#' (no idea what is does mean). 
#' That is why RGPR removes the first sample of each trace.
#' The second sample of each trace is used to indicate if there is a fiducial 
#' marker. RGPR extracts this information (maybe not always correctly) and 
#' removes the second sample of each trace. That is the reason why the *.dzt 
#' files exported by RGPR have two samples/trace less.
#' 
#' @section Mala files (*.rd3/rd7, *.rad):
#' RGPR check the dimension of the data against the number of traces and 
#' samples specified in the *.rad file and try to find the correct setup.
#' 
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
#'             It assumes that \code{Vmin = -Vmax}. If \code{Vmax = NULL}, the 
#'             default values depending on the file format and given by the
#'             GPR device manufacturer will be used. If \code{Vmax = FALSE},
#'             no bits to Volt transformation is applied (i.e., the bit values
#'             are returned).
#' @param fPath Filepath (character). DEPRECATED. Use \code{dsn} instead.
#' @param ch For multi-frequency GSSI files (*.dzt), which channel is red.
#' @param verbose (boolean). If \code{FALSE}, all messages and warnings are
#'                suppressed (use with care).
#' @param interp_pos logical: should the trace position be interpolated if possible? TRUE or FALSE
#' @param toUTM logical: if \code{TRUE} project GPS coordinates (WGS84) to
#'               the corresponding UTM coordinate reference system. 
#' @param method A length-three character vector defining the interpolation
#'               methods (same methods as in \code{signal::interp1}:
#'               "linear", "nearest", "pchip", "cubic", and "spline"). 
#'               First element for the interpolation of the 
#'               inter-trace distances, 
#'               second element for the interpolation of the horizontal 
#'               trace positions, and third element for the interpolation
#'               of the vertical trace positions.
#' @param endian The endian-ness (\code{"big"} or \code{"little"}) of the 
#'                target system for 
#'                the file. Default value is \code{.Platform$endian}. 
#'                If the endianness is not correct, RGPR will automatically 
#'                detect it and try the other endianness. That means that it is 
#'                not necessary to set the correct endianness 
#'                (but if you do so, 'readGPR()' will be faster).
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
readGPR <- function(dsn, desc = "", dsn2 = NULL, format = NULL, Vmax = NULL,
                    fPath, ch = 1, verbose = TRUE, interp_pos = TRUE, 
                    toUTM = TRUE,
                    method = c("linear", "linear", "linear"), 
                    endian =  .Platform$endian){
  
  if(!missing(fPath)){
    if(missing(dsn)){
      dsn <- fPath
    }
    warning("Argument 'fPath' is deprecated. Use instead ",
            "'dsn' (data source name)")
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
  
  
  #----------------------------------------------------------------------------#
  # Sensors & Software ----------
  # DT1 + HD (+ GPS)
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
    dt1 <- verboseF( readDT1(dsn[["DT1"]]), verbose = verbose)
    x <- verboseF(.gprDT1(list(hd = hd$HD, dt1 = dt1$dt1hd, data = dt1$data ), 
                          fName = fName[["DT1"]], fPath = fPath[["DT1"]], 
                          desc = desc, Vmax = Vmax),  verbose = verbose)
    if( !is.null(dsn[["GPS"]]) && isTRUE(interp_pos)){
      x <- tryCatch({
              gps <-  verboseF(readGPS(dsn[["GPS"]], toUTM = toUTM), verbose = verbose)
              if(!is.null(gps)){
                ## HERE: convert GPR to UTM
                # FIXME
                x <- interpPos(x, gps$mrk, tol = sqrt(.Machine$double.eps), 
                               method = method)
               crs(x) <- gps$crs
              }
               return(x)
            },
            error = function(cond) {
              message("I could neither read your GPS data ",
                      "nor interpolate the trace position.")
              # Choose a return value in case of error
              return(x)
            })    
    }
  
  #----------------------------------------------------------------------------#
  # SEG-2 ----------
  # SEG2/SG2
  }else if( any( c("SEG2", "SG2") %in% toupper(ext) ) ){
    i <- which(grepl("SEG2|SG2", toupper(ext)))[1]
    dsn <- list(SG2  = dsn[[i]], 
                GPS = getFName(fPath[1], ext = ".GPS", throwError = FALSE)$gps)
    if(!is.null(dsn[["GPR"]])){
      warning("Reading of GPS data for SEG2 files not yet implemented.\n",
              "Please contact me: emanuel.huber@pm.me")
    }
    # print(dsn)
    A <- verboseF( readSEG2(dsn = dsn[["SG2"]]))
    x <- verboseF( .readSEG2(A))
  #----------------------------------------------------------------------------#
  # US Radar ----------
  # RA1, RA2, RAD
  }else if( any( c("RA1", "RA2", "RAD") %in% toupper(ext) ) ){
    USRExt <-  c("RA1", "RA2", "RAD")
    tst <- USRExt %in% toupper(ext)
    dsn <- list(USRADAR  = dsn[USRExt[tst][1]], 
                GPS = getFName(fPath[1], ext = ".GPS", throwError = FALSE)$gps)
    # if(!is.null(dsn[["GPS"]])){
    #   warning("Reading of GPS data for SEG2 files not yet implemented.\n",
    #           "Please contact me: emanuel.huber@pm.me")
    # }
    # print(dsn)
    A <- verboseF( readSEG2(dsn = dsn[["USRADAR"]]))
    x <- verboseF( .readSEG2(A, fName = fName[[1]], fPath = fPath[[1]],  
                             desc = desc, Vmax = Vmax, ext = names(dsn$USRADAR)))
    if(!is.null(dsn[["GPS"]])){
      x <- tryCatch({
        gps <-  verboseF(readGPSUSRADAR(dsn[["GPS"]], toUTM = toUTM), verbose = verbose)
        if(!is.null(gps) && isTRUE(interp_pos)){
          x <- interpPos(x, gps$mrk, tol = sqrt(.Machine$double.eps), 
                         method = method)
          crs(x) <- gps$crs
        }
        return(x)
      },
      error = function(cond) {
        message("I could neither read your GPS data ",
                "nor interpolate the trace position.")
        # Choose a return value in case of error
        return(x)
      })
    }
  #----------------------------------------------------------------------------#
  # Mala ----------
  # RD3 + RAD (+ COR)
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
              gps <-  verboseF(readCOR(dsn[["COR"]], toUTM = toUTM), verbose = verbose)
              if(!is.null(gps) && isTRUE(interp_pos)){
                x <- interpPos(x, gps$mrk, tol = sqrt(.Machine$double.eps), 
                               method = method)
                crs(x) <- gps$crs
              }
              return(x)
            },
            error = function(cond) {
              message("I could neither read your GPS data ",
                      "nor interpolate the trace position.")
              # Choose a return value in case of error
              return(x)
            })
    }    
  #----------------------------------- IDS -----------------------------------#
  #-------------------------- DT  (+ GEC) -------------------------------#
  # IDS -------
  }else if( "DT" %in% toupper(ext)  ){
    if(length(dsn) == 1){
      dsn <- list(DT  = dsn[["DT"]], 
                  GEC = getFName(fPath[1], ext = ".GEC", throwError = FALSE)$gec)
    }else{
      if( !("GEC" %in% toupper(ext)) ){
        dsn <- c(dsn, list(GEC = getFName(fPath[1], ext = ".GEC", throwError = FALSE)$gec))
      }
    }
    y <- verboseF( readDT(dsn[["DT"]]), verbose = verbose)
    x <- verboseF( .gprDT(y, 
                           fName = fName[["DT"]], fPath = fPath[["DT"]], 
                           desc = desc, Vmax = Vmax), verbose = verbose)
    if( !is.null(dsn[["GEC"]]) && isTRUE(interp_pos)){
      x <- tryCatch({
        x_mrk <-  verboseF(readGEC(dsn[["GEC"]]), verbose = verbose)
        x <- interpPos(x, x_mrk, tol = sqrt(.Machine$double.eps), 
                       method = method)
        crs(x) <- paste0("+init=epsg:32635 +proj=utm +zone=", 
                         x_mrk[1,"crs"], 
                         " +datum=WGS84 +units=m +no_defs## +ellps=WGS84 +towgs84=0,0,0")
        x
      },
      error = function(cond) {
        message("I could neither read your GPS data ",
                "nor interpolate the trace position.")
        # Choose a return value in case of error
        return(x)
      })
    }
  #----------------------- TRANSIENT TECHNOLOGIES -----------------------------#
  #---------------------------------- SGPR ------------------------------------#
  # TRANSIENT TECHNOLOGIES ----
  }else if( "SGPR" %in% toupper(ext)  ){
    y <- verboseF( readSGPR(dsn[["SGPR"]]), verbose = verbose)
    x <- verboseF( .gprSGPR(y, 
                           fName = fName[["SGPR"]], fPath = fPath[["SGPR"]], 
                           desc = desc, Vmax = Vmax), verbose = verbose)
  #---------------------------SEG-Y +  EASY RADAR -----------------------------#
  #------------------------------- SEG/SEG-Y ----------------------------------#
  # SEGY ----
  }else if(any(c("SGY", "SEGY") %in% toupper(ext))){
    i <- which(grepl("SGY|SEGY", toupper(ext)))[1]
    dsn <- dsn[[i]]
    # if( !inherits(dsn[[i]], "connection") ){
    #   dsn <- file(dsn[[i]], "rb")
    # }
    # if(is.null(endian)) endian <- .Platform$endian
    # ENDIAN <- "big"
    # THD <- readSGY_textual_file_header(dsn, ENDIAN = endian)
    # test <- any(verboseF(grepl("Prism", THD), verbose = FALSE)) & 
    #   any(verboseF(grepl("Radar Systems, Inc.", THD), 
    #                verbose = FALSE))
    # # read RadSys Zond System
    # if( test ){
    #   # if(verbose) message("This is not a classical SEG-Y file... I try to read it!")
    #   ndn <- c("little", "big")
    #   A <- tryCatch({verboseF( readSEGY_RadSys_Zond_GPR(dsn, ENDIAN = endian), 
    #                            verbose = verbose)},
    #                 error = function(cond){
    #                   message("failed attempt... ",
    #                           "I try the other endianness, namely '",
    #                           ndn[ndn != endian], "'!")
    #                   return(NULL)})
    #   if(is.null(A)){
    #     A <- tryCatch({verboseF(readSEGY_RadSys_Zond_GPR(dsn, 
    #                                                      ENDIAN = ndn[ndn != endian]), 
    #                     verbose = verbose)},
    #                     error = function(cond){
    #                       message("failed attempt, again... ",
    #                               "please contact me\n",
    #                               "emanuel.huber@pm.me")
    #                       return(NULL)})
    #   }
    #   if(is.null(A)){ stop() }
    #   x <- verboseF( .gprSEGY(A, fName = fName, fPath = fPath, 
    #                           desc = desc, Vmax = Vmax), verbose = verbose)
      # read classical SEG-Y file
    # }else{
      # if(verbose) message("This is a classical SEG-Y file... I try to read it!")
      ndn <- c("little", "big")
      # message(endian)
      # message(dsn)
      A <- tryCatch({verboseF(readSGY(dsn, ENDIAN = endian), 
                              verbose = verbose)},
                    error = function(cond){
                      # message("failed attempt... ",
                      #         "I try the other endianness, namely '",
                      #        ndn[ndn != endian], "'!")
                      return(NULL)})
      if(is.null(A)){
        # message(ndn[ndn != endian])
        # message(dsn)
        # readSGY(dsn, ENDIAN = ndn[ndn != endian])
        A <- tryCatch({verboseF(readSGY(dsn, ENDIAN = ndn[ndn != endian]), 
                                verbose = verbose)},
                      error = function(cond){
                        # message("failed attempt! ",
                        #         "please contact me\n",
                        #         "emanuel.huber@pm.me")
                        return(NULL)})
      }
      if(is.null(A)){ stop("I tried both ending: bit and little without success...") }
      # print(names(A))
      # A <- verboseF(readSGY(dsn, ENDIAN = endian), verbose = verbose)
      x <- verboseF( .gprSGY(A, fName = fName, fPath = fPath, 
                             desc = desc, Vmax = Vmax), verbose = verbose)
      
      if(isTRUE(interp_pos)){
        if(inherits(x, "GPR") && !is.null(x@hd$xyz)){
          if(nrow(x@hd$xyz) == ncol(x)){
            test <- !duplicated(x@hd$xyz[, 1:2])
            if(sum(test) > 0){
              x <- x[, test]
              x@coord <- x@hd$xyz[test,]
              x@pos <- posLine(x@hd$xyz[test, 1:2])
              x@dx <- mean(diff(x@pos))
            }
          }else{
            message("I found some coordinates!\n",
                    "Check them with 'gethd(x)$xyz'...")
          }
        }
      }
      return(x)
    # }
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
  # IPRB -----
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
              gps <-  verboseF(readIPRCOR(dsn[["COR"]], toUTM = toUTM), verbose = verbose)
              x <- interpPos(x, x_cor, tol = sqrt(.Machine$double.eps), 
                             method = method)
              crs(x) <- gps$crs
              # if(toUTM == TRUE){
              #   warning("Option 'toUTM' not yet implemented!")
              # }
              x
            },
            error = function(cond) {
              message("I could neither read your GPS data ",
                      "nor interpolate the trace position.")
              # Choose a return value in case of error
              return(x)
            })
    }
  #--------------------------------- GSSI -------------------------------------#
  #------------------------------ DZT (+ DZX, DZG) ----------------------------#
  # DZT -----
  }else if("DZT" %in% toupper(ext)){
    if(length(dsn) == 1){
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
              gps <-  verboseF(readDZG(dsn[["DZG"]], toUTM = toUTM), verbose = verbose)
              x <- interpPos(x, gps$mrk, tol = sqrt(.Machine$double.eps), 
                             method = method)
              crs(x) <- gps$crs
              # if(toUTM == TRUE){
              #   warning("Option 'toUTM' not yet implemented!")
              # }
              x
            },
            error = function(cond) {
              message("I could neither read your GPS data ",
                      "nor interpolate the trace position.")
              # Choose a return value in case of error
              return(x)
            })
    }
  #------------------------------- 3d RADAR -----------------------------------#
  #------------------------------------ VOL -----------------------------------#
  }else if("VOL" %in% toupper(ext)){
    A <- verboseF( readVOL(dsn$VOL), verbose = verbose)
    x <- verboseF( .gprVOL(A, fName = fName, fPath = fPath, 
                           desc = desc, Vmax = Vmax), verbose = verbose)
    if(A$hd$dim == "3D"){
      warning("return a 'GPRcube' object with complex numbers.",
              " Current processing and plotting functions will most likely not ",
              "work on the returned object. Please contact me:\n",
              "emanuel.huber@pm.me")
    }else{
      warning("Still experimental. Don't hesitate to contact me:\n",
              "emanuel.huber@pm.me")
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
                if(toUTM == TRUE){
                  warning("Option 'toUTM' not yet implemented!")
                }
                x
              }else{
                x
              }
            },
            error = function(cond) {
              message("I could neither read your GPS data ",
                      "nor interpolate the trace position.")
              # Choose a return value in case of error
              return(x)
            })
    }
  }else if("OUT" %in% toupper(ext)){
    x <- verboseF(readOUT(dsn[["OUT"]]), verbose = verbose)
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
        if(!is.null(x[['delineations']])){
          if(ncol(x[['delineations']][[1]]) >= 5 ){
            for(i in seq_along(x[['delineations']])){
              x[['delineations']][[i]][, 5] <- -x[['delineations']][[i]][, 5]
            }
          }
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
               vel =  x[['vel']],                 # m/ns
               # vel = list(v = x[['vel']]),                 # m/ns
               delineations = x[['delineations']],
               hd =  x[['hd']]                   # header
      )
      y@filepath <- fPath
      return(y)
    }
  }else{
    stop(paste0("File extension not recognised!\n",
                "Must be '.dt1', '.dzt', '.rd3', '.sgy', '.segy', '.rds'\n",
                "'.iprb', '.iprh', '.dat', '.sgpr', '.dt', '.vol', '.seg2'\n", 
                "'.sg2', '.out'."))
  }
  if(grepl("CMP", x@surveymode)){
    x@surveymode <- "CMP"
    if(length(x@antsep) == 1){
      if(length(x@rec) == 0 || length(x@trans) == 0){
        x@antsep <- seq(x@antsep, by = x@dx, length.out = length(x))
      }else{
        x@antsep <- sqrt(colSums((x@rec - x@trans)^2))
      }
    }
  }
  if(any(is.na(x@data))){
    x@data <- apply(x@data, 2, fillNAprevious)
  }
  if(grepl("meter", posunit(x)) || grepl("metre", posunit(x))){
    posunit(x) <- "m" 
  }
  return(x)
}



# http://www.cookbook-r.com/Manipulating_data/Filling_in_NAs_with_last_non-NA_value/

# x <- c(NA, NA, 1, 2, 4, NA, 3, 2, NA, NA, 1, NA)

#' @export
fillNAprevious <- function(x){
  z  <- !is.na(x)                  # indicates the positions of y whose values we do not want to overwrite
  # z2  <- z | !cumsum(z)             # for leading NA's in y, z will be TRUE, otherwise it will be FALSE where y has a NA and TRUE where y does not have a NA
  # tst <- xor(z, z2)
  # y <- y[z2][cumsum(z2)]
  # These are the non-NA values from x only
  # Add a leading NA for later use when we index into this vector
  x <- c(NA, x[z])
  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  z2 <- cumsum(z)+1
  # The original vector with gaps filled
  x <- x[z2]
  x[is.na(x)] <- 0
  return(x)
}


#' Extract frequency from string
#' 
#' Extract with regex the antenna frequency in a string
#' @export
freqFromString <- function(s){
  s <- iconv(s, "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by '
  if(grepl("MHz", s, ignore.case = TRUE)){
    a <- regexpr("[0-9]+.MHZ",  s, ignore.case = TRUE, perl = TRUE)
  }else{
    a <- regexpr("[0-9]+",  s, ignore.case = TRUE, perl = TRUE)
  }
  b <- regmatches(s,  a)
  b <- as.numeric(gsub("[^0-9]", "", b))
  if(length(b) == 0){
    return(NULL)
  }else{
    return(b)
  }
}

#' @export
freqFromStringMHzGHz <- function(s){
  s <- iconv(s, "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by '
  a <- regexpr("[0-9]+.(MHZ|GHZ)",  s, ignore.case = TRUE, perl = FALSE)
  b <- regmatches(s,  a)
  b <- as.numeric(gsub("[^0-9]", "", b))
  if(length(b) == 0){
    return(NULL)
  }else{
    return(b)
  }
}
# s <- "1230 fds 200-MHZ 12.3"
# s <- "1230MLF"
# s <- "D1230MLF"
# freqFromString(s)


#' @export
antSepFromAntFreq <- function(antfreq, verbose = TRUE){
  ant <- list(f = c(12.5, 25, 50, 100,  200, 450,   900, 1200, 5000),
             s = c( 8,    4,  2,   1,  0.5, 0.25, 0.17, 0.075, 0))
  # antsep <- approx(ant$f, ant$s, xout = antfreq)$y
  antsep <- signal::interp1(x = ant$f, y = ant$s, xi = antfreq, 
                            method = "linear", extrap = TRUE)
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


readBinary <- function(con, what, n = 1L, size = NA_integer_, signed = TRUE,
                       endian = .Platform$endian, useBytes = FALSE){
  if(what == "character"){
    readChar(con, nchars = size, useBytes = useBytes)
  }else{
    readBin(con, what = what, n = n, size = size, signed = signed, endian = endian)
  }
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

# read characters and coerce them to numeric
ascii2num <- function(con, n){
  x <- readBin(con, what = integer(), n = n, size = 1)
  return( as.numeric(intToUtf8(x)) )
}

int2ascii <- function(con, n){
  x <- readBin(con, what = integer(), n = n, size = 1)
  return( (intToUtf8(x)) )
}

readBinChar <- function(con, n = 1L, size = NA_integer_, signed = TRUE, 
                        endian = .Platform$endian){
  intToUtf8(readBin(con, what = integer(), n = n, size = size, 
                    signed = signed, endian = endian))
}

#' Fix a 32 bit unsigned integer that has been read as signed
int32touint32 <- function(x, nbits = 32){
  signs <- sign(x)
  x[signs < 0] <- x[signs < 0] + 2^nbits
  return(x)
}
