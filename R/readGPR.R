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
#' @param Vmax length-one numeric vector: nominal analog input voltage used 
#'             for the bits to volt transformation. 
#'             It assumes that \code{Vmin = -Vmax}. If \code{Vmax = NULL},
#'             no bits to Volt transformation is applied.
#' @param verbose (boolean). If \code{FALSE}, all messages and warnings are
#'                suppressed (use with care).
#' @param interpGPS logical: should the trace position be interpolated if possible? TRUE or FALSE
#' @param method A length-three character vector defining the interpolation
#'               methods (same methods as in \code{signal::interp1}:
#'               "linear", "nearest", "pchip", "cubic", and "spline"). 
#'               First element for the interpolation of the 
#'               inter-trace distances, 
#'               second element for the interpolation of the horizontal 
#'               trace positions, and third element for the interpolation
#'               of the vertical trace positions.
#' @return The GPR data as object of the class RGPR.
# #' @seealso \code{\link{writeGPR}}
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
readGPR <- function(dsn, desc = "", Vmax = 50,
                    verbose = TRUE, interpGPS = TRUE, 
                    method = c("linear", "linear", "linear")){
  
  dsn <- Filter(Negate(is.null), dsn)
  # file path, name and extension for character or connection
  fPath <- sapply(dsn, getFPath, USE.NAMES = FALSE)
  ext <- sapply(fPath, .fExt, USE.NAMES = FALSE)
  fName <- sapply(fPath, .fNameWExt, USE.NAMES = FALSE)
  if(!is.list(dsn)) dsn <- as.list(dsn)
  names(dsn)   <- toupper(ext)
  names(fName) <- toupper(ext)
  names(fPath) <- toupper(ext)
  
  x_gps <- NULL
  
  #--------------------------- SENSORS & SOFTWARE -----------------------------#
  #--------------------------- DT1 + HD (+ GPS) -------------------------------#
  if("DT1" %in% toupper(ext)){
    if(all(sapply(dsn, inherits, "connection"))){
      if(!("HD" %in% toupper(ext))) stop("Missing connection to '*.hd' file.") 
    }else{
      if(is.na(fPath["HD"])) fPath["HD"] <- fPath["DT1"]
      if(is.na(fPath["GPS"])) fPath["GPS"] <- fPath["DT1"]
      dsn[["DT1"]] <-  getFName(fPath["DT1"], ext = ".DT1")$dt1
      dsn[["HD"]]  <-  getFName(fPath["HD"],  ext = ".HD" )$hd
      dsn[["GPS"]] <-  getFName(fPath["GPS"], ext = ".GPS", 
                                throwError = FALSE)$gps
    }
    hd  <- verboseF( readHD(dsn[["HD"]]), verbose = verbose)
    dt1 <- verboseF( readDT1(dsn[["DT1"]], ntr = hd$ntr, npt = hd$npt), 
                     verbose = verbose)
    x <- verboseF(.gprDT1(list(hd = hd$HD, dt1 = dt1$dt1hd, data = dt1$data ), 
                          fName = fName[["DT1"]], fPath = fPath[["DT1"]], 
                          desc = desc, Vmax = Vmax),  verbose = verbose)
    if( !is.null(dsn[["GPS"]])){
      x_gps <-  verboseF(readGPS(dsn[["GPS"]]), verbose = verbose)
    }
    # if(length(dsn) == 1){
    #   if(inherits(dsn, "connection")){
    #     stop("Please add an additional connection to 'dsn' for ",
    #          "the header file '*.hd'")
    #   }
    #   # get HD (+ GPS) file(s)
    #   dsn <- list(DT1 = getFName(fPath[1], ext = ".DT1")$dt1,  # dsn, 
    #               HD  = getFName(fPath[1], ext = ".HD")$hd, 
    #               GPS = getFName(fPath[1], ext = ".GPS", throwError = FALSE)$gps)
    # }else if( !("HD" %in% toupper(ext))){
    #   stop("Missing connection or filepath to '*.hd' file.") 
    # }else if( !("GPS" %in% toupper(ext)) ){
    #   dsn <- list(DT1 = dsn[["DT1"]], 
    #               HD  = dsn[["HD"]],
    #               GPS = getFName(fPath[1], ext = ".GPS", throwError = FALSE)$gps)
    # }
    # hd  <- verboseF( readHD(dsn[["HD"]]), verbose = verbose)
    # dt1 <- verboseF( readDT1(dsn[["DT1"]], ntr = hd$ntr, npt = hd$npt), 
    #                   verbose = verbose)
    # x <- verboseF(.gprDT1(list(hd = hd$HD, dt1 = dt1$dt1hd, data = dt1$data ), 
    #                       fName = fName[["DT1"]], fPath = fPath[["DT1"]], 
    #                       desc = desc, Vmax = Vmax),  verbose = verbose)
    # if( !is.null(dsn[["GPS"]]) && isTRUE(interp_pos)){
    #   x <- tryCatch({
    #           gps <-  verboseF(readGPS(dsn[["GPS"]]), verbose = verbose)
    #           if(!is.null(gps)){
    #             x <- interpPos(x, gps, tol = sqrt(.Machine$double.eps), 
    #                            method = method)
    #             crs(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
    #             x
    #           }else{
    #             x
    #           }
    #         },
    #         error = function(cond) {
    #           message("I could neither read your GPS data ",
    #                   "nor interpolate the trace position.")
    #           # Choose a return value in case of error
    #           return(x)
    #         }#,
    #         # warning = function(cond) {
    #         #   message(paste("URL caused a warning:", url))
    #         #   message("Here's the original warning message:")
    #         #   message(cond)
    #         #   # Choose a return value in case of warning
    #         #   return(NULL)
    #         # },
    #         # finally={
    #         #   # NOTE:
    #         #   # Here goes everything that should be executed at the end,
    #         #   # regardless of success or error.
    #         #   # If you want more than one expression to be executed, then you 
    #         #   # need to wrap them in curly brackets ({...}); otherwise you could
    #         #   # just have written 'finally=<expression>' 
    #         #   message(paste("Processed URL:", url))
    #         #   message("Some other message at the end")
    #         # }
    #         )    
    # }
    # # plot(x)
    
  #----------------------------------- MALA -----------------------------------#
  #-------------------------- RD3 + RAD (+ COR) -------------------------------#
 # }else if(any( c("RD3", "RD7") %in% toupper(ext) )){
 #    if(all(sapply(dsn, inherits, "connection"))){
 #      if(!("RAD" %in% toupper(ext))) stop("Missing connection to '*.rad' file.") 
 #    }else{
 #      dsn[["DT1"]] <-  getFName(fPath["DT1"], ext = ".DT1")$dt1
 #      dsn[["RAD"]] <-  getFName(fPath["RAD"],  ext = ".RAD" )$rad
 #      dsn[["COR"]] <-  getFName(fPath["COR"], ext = "COR", 
 #                                throwError = FALSE)$cor
 #    }
 #    hd  <- verboseF( readHD(dsn[["HD"]]), verbose = verbose)
 #    dt1 <- verboseF( readDT1(dsn[["DT1"]], ntr = hd$ntr, npt = hd$npt), 
 #                     verbose = verbose)
 #    x <- verboseF(.gprDT1(list(hd = hd$HD, dt1 = dt1$dt1hd, data = dt1$data ), 
 #                          fName = fName[["DT1"]], fPath = fPath[["DT1"]], 
 #                          desc = desc, Vmax = Vmax),  verbose = verbose)
 #    if( !is.null(dsn[["GPS"]])){
 #      x_gps <-  verboseF(readGPS(dsn[["GPS"]]), verbose = verbose)
 #    }
    #--- --- --- --- --- --- --- --- --- ---
  # }else if( any( c("RD3", "RD7") %in% toupper(ext) ) ){
  #   if(length(dsn) == 1){
  #     if(inherits(dsn, "connection")){
  #       stop("Please add an additional connection to 'dsn' in 'readGPR()' for ",
  #            "the header file '*.rad'")
  #     }
  #     # get RAD (+ COR) file(s)
  #     dsn <- list(RD37 = getFName(fPath[1], ext = paste0(".", toupper(ext)))[[tolower(ext)]], #dsn, 
  #                 RAD  = getFName(fPath[1], ext = ".RAD")$rad, 
  #                 COR  = getFName(fPath[1], ext = ".COR", throwError = FALSE)$cor)
  #   }else if( !("RAD" %in% toupper(ext))){
  #     stop("Missing connection or filepath to '*.rad' file.") 
  #   }else if( !("COR" %in% toupper(ext)) ){
  #     dsn[["COR"]] <- getFName(fPath[1], ext = ".COR", throwError = FALSE)$cor
  #   }
  #   nbytes <- 4
  #   i <- which("RD7" == toupper(ext) )
  #   if(length(i) == 0){
  #     i <- which("RD3" == toupper(ext) )
  #     nbytes <- 2
  #   } 
  #   dsn[["RD37"]] <- dsn[[i]] # FIXME: 
  #   # assume that the ext is correct 
  #   # (upper/lower case)
  #   rad  <- verboseF( readRAD(dsn[["RAD"]]), verbose = verbose)
  #   rd37 <-  verboseF( readRD37(dsn[["RD37"]], ntr = rad$ntr, npt = rad$npt, 
  #                               nbytes = nbytes), verbose = verbose)
  #   
  #   x <- verboseF( .gprRD3(list(hd = rad$HD, data = rd37), 
  #                                   fName = fName[[i]], fPath = fPath[[i]],  
  #                                   desc = desc, nbits = 8*nbytes, Vmax = Vmax), 
  #                  verbose = verbose)
  #   if( !is.null(dsn[["COR"]]) ){
  #     x <- tryCatch({
  #             x_cor <-  verboseF(readCOR(dsn[["COR"]]), verbose = verbose)
  #             if(!is.null(x_cor) && isTRUE(interp_pos)){
  #               x <- interpPos(x, x_cor, tol = sqrt(.Machine$double.eps), 
  #                              method = method)
  #               crs(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
  #               x
  #             }else{
  #               x
  #             }
  #           },
  #           error = function(cond) {
  #             message("I could neither read your GPS data ",
  #                     "nor interpolate the trace position.")
  #             # Choose a return value in case of error
  #             return(x)
  #           })
  #   }
  # #---------------------------SEG-Y +  EASY RADAR -----------------------------#
  # #------------------------------- SEG/SEG-Y ----------------------------------#
  # }else if(any(c("SGY", "SEGY") %in% toupper(ext))){
  #   i <- which(grepl("SGY|SEGY", toupper(ext)))[1]
  #   if( !inherits(dsn[[i]], "connection") ){
  #     dsn <- file(dsn[[i]], "rb")
  #   }
  #   ENDIAN <- "big"
  #   THD <- readSGY_textual_file_header(dsn, ENDIAN = "big")
  #   test <- FALSE
  #   # if(all(validEnc(THD))){
  #   test <- any(verboseF(grepl("Prism", THD), verbose = FALSE)) & 
  #     any(verboseF(grepl("Radar Systems, Inc.", THD), 
  #                  verbose = FALSE))
  #   # }
  #   # read RadSys Zond System
  #   if( test ){
  #     A <- verboseF( readSEGY_RadSys_Zond_GPR(dsn), verbose = verbose)
  #     x <- verboseF( .gprSEGY(A, fName = fName, fPath = fPath, 
  #                             desc = desc, Vmax = Vmax), verbose = verbose)
  #     # read classical SEG-Y file
  #   }else{
  #     A <- verboseF(readSGY(dsn), verbose = verbose)
  #     x <- verboseF( .gprSGY(A, fName = fName, fPath = fPath, 
  #                            desc = desc, Vmax = Vmax), verbose = verbose)
  #     return(x)
  #   }
  # #---------------------------- IMPULSE RADAR ---------------------------------#
  # #---------------------- IPRB + IPRH (+ COR + TIME + MRK) --------------------#
  # }else if("IPRB" %in% toupper(ext)){
  #   
  #   # fName <- getFName(dsn, ext = c(".iprh", ".iprb"))
  #   # #--- READ OPTIONAL FILES
  #   # fNameOpt <- getFName(dsn, ext = c(".cor", ".time", ".mrk"), 
  #   #                      throwError = FALSE)
  #   
  #   if(length(dsn) == 1){
  #     if(inherits(dsn, "connection")){
  #       stop("Please add an additional connection to 'dsn' in 'readGPR()' for ",
  #            "the header file '*.iprh'")
  #     }
  #     # get HD (+ GPS) file(s)
  #     dsn <- list(IPRB = getFName(fPath[1], ext = ".iprb")$iprb, #dsn, 
  #                 IPRH = getFName(fPath[1], ext = ".iprh")$iprh, 
  #                 COR  = getFName(fPath[1], ext = ".cor",  throwError = FALSE)$cor,
  #                 TIME = getFName(fPath[1], ext = ".time", throwError = FALSE)$time,
  #                 MRK  = getFName(fPath[1], ext = ".mrk",  throwError = FALSE)$mrk
  #     )
  #   }else if( !("IPRH" %in% toupper(ext))){
  #     stop("Missing connection or filepath to '*.iprh' file.") 
  #     # FIXME: adapt below... 
  #   }else{
  #     dsn <- list(IPRB = dsn[["IPRB"]], 
  #                 IPRH = dsn[["IPRH"]])
  #     if( !("COR" %in% toupper(ext)) ){
  #       dsn <- c(dsn, list(COR = getFName(fPath[1], ext = ".cor", throwError = FALSE)$cor))
  #     }
  #     if( !("TIME" %in% toupper(ext)) ){
  #       dsn <- c(dsn, list(TIME = getFName(fPath[1], ext = ".time", throwError = FALSE)$time))
  #     }
  #     if( !("MRK" %in% toupper(ext)) ){
  #       dsn <- c(dsn, list(MRK = getFName(fPath[1], ext = ".mrk", throwError = FALSE)$mrk))
  #     }
  #   }
  #   hd   <- verboseF( readIPRH(dsn[["IPRH"]]), verbose = verbose)
  #   iprb <- verboseF( readIPRB(dsn[["IPRB"]], ntr = hd$ntr, npt = hd$npt, 
  #                              nbytes = hd$nbytes), verbose = verbose)
  #   iprball <- list(hd = hd$HD, data = iprb)
  #   if( !is.null(dsn[["TIME"]])){
  #     iprball <- c(iprball, list(time = readTIME(dsn[["TIME"]])))
  #   }
  #   if( !is.null(dsn[["MRK"]])){
  #     iprball <- c(iprball, list(mrk = readMRK(dsn[["MRK"]])))
  #   }
  #   x <- verboseF(.gprImpulseRadar(iprball, 
  #                         fName = fName[["IPRB"]], fPath = fPath[["IPRB"]], 
  #                         desc = desc, Vmax = Vmax),  verbose = verbose)
  #   if( !is.null(dsn[["COR"]]) && isTRUE(interp_pos)){
  #     x <- tryCatch({
  #             x_cor <-  verboseF(readIPRCOR(dsn[["COR"]]), verbose = verbose)
  #             x <- interpPos(x, x_cor, tol = sqrt(.Machine$double.eps), method = method)
  #             crs(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
  #             x
  #           },
  #           error = function(cond) {
  #             message("I could neither read your GPS data ",
  #                     "nor interpolate the trace position.")
  #             # Choose a return value in case of error
  #             return(x)
  #           })
  #   }
  # #--------------------------------- GSSI -------------------------------------#
  # #------------------------------ DZT (+ DZX, DZG) ----------------------------#
  }else if("DZT" %in% toupper(ext)){
    if(all(sapply(dsn, inherits, "connection"))){
      #if(!("HD" %in% toupper(ext))) stop("Missing connection to '*.hd' file.") 
    }else{
      if(is.na(fPath["DZX"])) fPath["DZX"] <- fPath["DZT"]
      if(is.na(fPath["DZG"])) fPath["DZG"] <- fPath["DZT"]
      dsn[["DZT"]] <-  getFName(fPath["DZT"], ext = ".DZT")$dzt
      dsn[["DZX"]] <-  getFName(fPath["DZX"],  ext = ".DZX", 
                                throwError = FALSE )$dzg
      dsn[["GPS"]] <-  getFName(fPath["DZG"], ext = ".DZG", 
                                throwError = FALSE)$dzg
    }
    dzt <- verboseF( readDZT(dsn[["DZT"]]), verbose = verbose)
    if(!is.null(dsn[["DZX"]])){
      dzx <- verboseF( readDZX(dsn[["DZX"]]), verbose = verbose)
      dzt <- c(dzt, list(dzx = dzx))
    }
    x <- verboseF( .gprDZT(dzt, fName = fName[["DZT"]], fPath = fPath[["DZT"]], 
                           desc = desc, Vmax = Vmax), 
                   verbose = verbose)
    if( !is.null(dsn[["DZG"]])){
      x_gps <-  verboseF(readDZG(dsn[["DZG"]]), verbose = verbose)
    }
  # }else if("DZT" %in% toupper(ext)){
  #   if(length(dsn) == 1){
  #     if(inherits(dsn, "connection")){
  #       stop("Please add an additional connection to 'dsn' in 'readGPR()' for ",
  #            "the header file '*.iprh'")
  #     }
  #     dsn <- list(DZT = dsn[["DZT"]], 
  #                 DZX = getFName(fPath[1], ext = ".DZX", throwError = FALSE)$dzx,
  #                 DZG = getFName(fPath[1], ext = ".DZG", throwError = FALSE)$dzg)
  #   }else{
  #     if( !("DZX" %in% toupper(ext)) ){
  #       dsn <- c(dsn, list(DZX = getFName(fPath[1], ext = ".DZX", throwError = FALSE)$dzx))
  #     }
  #     if( !("DZG" %in% toupper(ext)) ){
  #       dsn <- c(dsn, list( DZG = getFName(fPath[1], ext = ".DZG", throwError = FALSE)$dzg))
  #     }
  #     
  #   }
  #   dzt <- verboseF( readDZT(dsn[["DZT"]]), verbose = verbose)
  #   if(!is.null(dsn[["DZX"]])){
  #     dzx <- verboseF( readDZX(dsn[["DZX"]]), verbose = verbose)
  #     dzt <- c(dzt, list(dzx = dzx))
  #   }
  #   x <- verboseF( .gprDZT(dzt, 
  #                          fName = fName[["DZT"]], fPath = fPath[["DZT"]], 
  #                          desc = desc, Vmax = Vmax, ch = ch), verbose = verbose)
  #   if( !is.null(dsn[["DZG"]]) && isTRUE(interp_pos)){
  #     x <- tryCatch({
  #             x_mrk <-  verboseF(readDZG(dsn[["DZG"]]), verbose = verbose)
  #             x <- interpPos(x, x_mrk, tol = sqrt(.Machine$double.eps), 
  #                            method = method)
  #             crs(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
  #             x
  #           },
  #           error = function(cond) {
  #             message("I could neither read your GPS data ",
  #                     "nor interpolate the trace position.")
  #             # Choose a return value in case of error
  #             return(x)
  #           })
  #   }
  # #------------------------------- 3d RADAR -----------------------------------#
  # #------------------------------------ VOL -----------------------------------#
  # }else if("VOL" %in% toupper(ext)){
  #   A <- verboseF( readVOL(dsn), verbose = verbose)
  #   x <- verboseF( .gprVOL(A, fName = fName, fPath = fPath, 
  #                          desc = desc, Vmax = Vmax), verbose = verbose)
  #   if(A$hd$dim == "3D"){
  #     warning("return a 'GPRcube' object with complex numbers.",
  #             " Current processing and plotting functions will most likely not ",
  #             "work on the returned object. Please contact me:\n",
  #             "emanuel.huber@alumni.ethz.ch")
  #   }else{
  #     warning("Still experimental. Don't hesitate to contact me:\n",
  #             "emanuel.huber@alumni.ethz.ch")
  #   }
  #   return(x)
  # #------------------------------- UTSI ---------------------------------------#
  # #---------------------- DAT + HDR (+ GPS + GPT) -----------------------------#
  # }else if("DAT" %in% toupper(ext)){
  #   if(length(dsn) == 1){
  #     if(inherits(dsn, "connection")){
  #       stop("Please add an additional connection to 'dsn' in 'readGPR()' for ",
  #            "the header file '*.hdr'")
  #     }
  #     dsn <- list(DAT = getFName(fPath[1], ext = ".DAT")$dat,  # dsn, 
  #                 HDR = getFName(fPath[1], ext = ".HDR")$hdr, 
  #                 GPS = getFName(fPath[1], ext = ".GPS", throwError = FALSE)$gps,
  #                 GPT = getFName(fPath[1], ext = ".GPT", throwError = FALSE)$gpt)
  #   }else if( !("HDR" %in% toupper(ext))){
  #     stop("Missing connection or filepath to '*.hdr' file.") 
  #   }else{
  #       dsn <- list(DAT = dsn[["DAT"]], HDR = dsn[["HDR"]])
  #     if( !("GPS" %in% toupper(ext)) ){
  #       dsn <- c(dsn, list( GPS = getFName(fPath[1], ext = ".GPS", throwError = FALSE)$gps))
  #     }
  #     if( !("GPT" %in% toupper(ext)) ){
  #       dsn <- c(dsn, list( GPT = getFName(fPath[1], ext = ".GPT", throwError = FALSE)$gpt))
  #     }
  #   }
  #   # A <- verboseF( readUtsi(dsn), verbose = verbose)
  #   # x <- verboseF( .gprUtsi(A, fName = fName, fPath = fPath, 
  #   #                         desc = desc, Vmax = Vmax), verbose = verbose)
  #   
  #   hd <- readUtsiHDR(dsn[["HDR"]]) 
  #   z <- readUtsiDat(dsn[["DAT"]], splPerScan = hd$splPerScan, bits = hd$bits)
  #   # z[["hd"]] <- hd
  #   
  #   x <- verboseF( .gprUtsi(c(z, list(hd = hd)), 
  #                           fName = fName[["DAT"]], fPath = fPath[["DAT"]], 
  #                          desc = desc, Vmax = Vmax), verbose = verbose)
  #   if( !is.null(dsn[["GPS"]]) && !is.null(dsn[["GPT"]])  && isTRUE(interp_pos)){
  #     x <- tryCatch({
  #             gpt <- verboseF(readUtsiGPT(dsn[["GPT"]]), verbose = verbose)
  #             if(length(gpt) > 0 ){
  #               x_cor <-  verboseF(readUtsiGPS(dsn[["GPS"]], gpt), 
  #                                  verbose = verbose)
  #               x <- interpPos(x, x_cor, tol = sqrt(.Machine$double.eps), 
  #                              method = method)
  #               crs(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
  #               x
  #             }else{
  #               x
  #             }
  #           },
  #           error = function(cond) {
  #             message("I could neither read your GPS data ",
  #                     "nor interpolate the trace position.")
  #             # Choose a return value in case of error
  #             return(x)
  #           })
  #   }
  # }else if("TXT" %in% toupper(ext)){
  #   # fName <- .fNameWExt(fPath)
  #   A <- verboseF( readTXT(dsn[["TXT"]]), verbose = verbose)
  #   x <- verboseF( .gprTXT(A, fName = fName, fPath = fPath, 
  #                          desc = desc, Vmax = Vmax), verbose = verbose)
  # 
  # }else if("RDS" %in% toupper(ext)){
  #   x <- verboseF( readRDS(dsn[["RDS"]]), verbose = verbose)
  #   if(class(x) == "GPR"){
  #     x@filepath <- fPath
  #   }else if(class(x) == "list"){
  #     versRGPR <- x[["version"]]
  #     if(versRGPR == "0.1"){
  #       for(i in seq_along(x[['delineations']])){
  #         x[['delineations']][[i]][, 5] <- -x[['delineations']][[i]][, 5]
  #       }
  #     }
  #     y <- new("GPR",
  #              version = x[['version']],
  #              data = x[['data']],
  #              traces = x[['traces']],           # x$dt1$traces
  #              depth = x[['depth']],
  #              pos = x[['pos']],                 # x$dt1$position of the traces
  #              time0 = x[['time0']],             # x$dt1$time0
  #              time = x[['time']],               # x$dt1$time
  #              fid = trimStr(x[['fid']]),        # x$dt1$fid <-> x$dt1$x8
  #              ann = trimStr(x[['ann']]),        # x$dt1$fid <-> x$dt1$x8
  #              coord = x[['coord']],             # x$dt1$topo  of the traces
  #              rec = x[['rec']],                # x$dt1$recx,x$dt1$recy,x$dt1$recz
  #              trans = x[['trans']],
  #              coordref = x[['coordref']],       # x$dt1$topo of the traces
  #              freq = x[['freq']],
  #              dz = x[['dz']],
  #              dx = x[['dx']],
  #              antsep = x[['antsep']],
  #              name = x[['name']],
  #              description = x[['description']],
  #              filepath =x[['filepath']],
  #              depthunit = x[['depthunit']],
  #              posunit = x[['posunit']],
  #              surveymode = x[['surveymode']],
  #              date = x[['date']],
  #              crs = x[['crs']],
  #              proc = x[['proc']],               # processing steps
  #              vel = x[['vel']],                 # m/ns
  #              delineations = x[['delineations']],
  #              hd =  x[['hd']]                   # header
  #     )
  #     y@filepath <- fPath
  #     x <- y
  #   }
  }else{
     stop("File extension not recognised!\n",
          "Must be '.DT1', '.dzt', '.rd3', '.sgy', '.segy', '.rds'\n",
          "'.iprb', '.iprh', '.dat', or '.vol'.")
  }
  #-----
  if(!is.null(x_gps)){
    if(interpGPS){
      message("No interpolation: interPos() & crs() not ready")
      # x <- tryCatch({
      #   x <- interpPos(x, x_gps, tol = sqrt(.Machine$double.eps), 
      #                  method = method)
      #   crs(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
      #   x
      # },
      # error = function(cond){
      #   message("I could not interpolate the GPS data.")
      #   return(x)
      # })
    }else{
      x@md[["GPS"]] <- x_gps
    }
  }
  #-----
  if(grepl("CMP", x@mode)){
    x@surveymode <- "CMP"
    if(length(x@rec) == 0 || length(x@trans) == 0){
      x@antsep <- seq(x@antsep, by = x@dx, length.out = length(x))
    }else{
      x@antsep <- sqrt(colSums((x@rec - x@trans)^2))
    }
  }
  return(x)
}













