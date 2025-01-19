#' Read a GPR data file
#' 
#' Read GPR data file from various manufacturers and interpolate trace 
#' positions.
#' 
#' @section Supported file format:
#' 
#' \tabular{llll}{
#' **Manufacturer**        \tab **Mandatory files** \tab **optional GPS files**  \tab **other optional files**       \cr
#' Sensors & software           \tab **.dt1**, .hd      \tab .gps           \tab                        \cr
#' MALA  16 bits                \tab **.rd3**, .rad     \tab .cor           \tab                        \cr
#' MALA  32 bits                \tab **.rd7**, .rad     \tab .cor           \tab                        \cr
#' ImpulseRadar                 \tab **.iprb**, .iprh   \tab .cor           \tab .time , .mrk           \cr
#' GSSI                         \tab **.dzt**           \tab .dzg           \tab                        \cr
#' Geomatrix Earth Science Ltd  \tab **.dat**, .hdr     \tab .gps, .gpt     \tab                        \cr
#' Radar Systems, Inc.          \tab **.sgy/.segy**     \tab                \tab                        \cr
#' SEG-Y                        \tab **.sgy/.segy**     \tab                \tab                        \cr
#' R internal file format       \tab **.rds**           \tab                \tab                        \cr
#' txt files                    \tab **.txt**           \tab                \tab                        \cr
#' }
#' 
#' @section Notes:
#' 
#' \itemize{
#'   \item If the class of `dsn` is character, `readGPR` is 
#'         insensitive to the case of the extension (.DT1 or dt1)
#'   \item If `dsn` is a list of connections or a character vector, the 
#'         order of the elements does not play any rolle.
#'   \item If you use connections, `dsn` must contain at least all the 
#'         connections to the mandatory files (in any order). If there is
#'         more than one mandatory file, use a list of connection.
#'   \item If you use a file path for `dsn` (character), you only
#'         need to provide the path to the main mandatory file (marked in bold
#'         in the table): RGPR will find the other files if they only differ
#'         in their extension (same name). If the files have different names,
#'         you must provide at least the path to all the mandatory files.
#'   \item If an optional GPS data file is passed in `dsn` or exists,
#'         the GPS data file will be red even if `interpGPS = FALSE`. 
#'         The (formated) content of the GPS data file is then stored as 
#'         meta-data and can be retrieved with `metadata(x)$GPS`.
#'   \item If an optional GPS data file (longitude, latitude) is red 
#'         (when `interpGPS = TRUE` and a GPS data file exists), the 
#'         coordinates will be per default projected into the corresponding 
#'         UTM (WGS 84) projection (see [interpCoords()]).
#'   \item When reading GPR data, the clipped signal values are directly
#'         estimated from the bit values and stored as metadata.
#'         They can be retrieved with `metadata(x)$clip`.
#' }
#' 
#' @param dsn (`character|connection`) Data source name: either the 
#'            filepath to the GPR data (character),
#'            or an open file connection (can be a list of filepaths or
#'            open file connections). See Details.
#' @param desc (`character[1]`) Short description of the data.
#' @param Vmax (`numeric[1]|NULL`) Nominal analog input voltage used 
#'             for the bits to volt transformation. 
#'             It assumes that `Vmin = -Vmax`. If `Vmax = NULL`,
#'             no bits to Volt transformation is applied.
#' @param verbose (`logical[1]`) If `FALSE`, all messages and warnings are
#'                suppressed (use with care).
#' @param interpGPS (`logical[1]`) Should the trace position be interpolated 
#'                  if possible (that means if a GPS file is available)? 
#' @param UTM (`logical[1]|character[1]`) If `TRUE` it is assumed 
#'            that the coordinates are in the in geographic (longitude/latitude) 
#'            coordinate reference system (WGS84) and the coordinates are
#'            projected into the guessed UTM WGS84 coordinate reference 
#'            system (RGPR guesses the UTM zone based on the coordinates).
#'            Only used if `interpGPS = TRUE`.
#' @param ... additional parameters to be passed to [interpCoords()]
#' @return (`GPR|GPRset`) If the data contains more than one frequency
#'         data, it returns an object of the class `GPRset`. Else
#'         an object of the class `GPR`.
#' @seealso [writeGPR()], [interpCoords()], and [metadata()]
#' @examples
#' \dontrun{
#' # argument dsn is a file path
#' x1 <- readGPR(dsn = "data/RD3/DAT_0052.rd3")
#' y1 <- readGPR("data/FILE____050.DZT")
#' 
#' # argument dsn is a connection
#' con <- file("data/RD3/DAT_0052.rd3", "rb")   # binary mode
#' con2 <- file("data/RD3/DAT_0052.rad", "rt")  # text mode
#' x2 <- readGPR(dsn = list(con, con2))
#' 
#' con <- file(dsn = "data/FILE____050.DZT", "rb")
#' y1 <- readGPR(con)
#' }
#' @name readGPR
#' @rdname readGPR
#' @export
readGPR <- function(dsn, desc = "", Vmax = NULL,
                    verbose = TRUE, interpGPS = TRUE,
                    UTM = TRUE,
                    ...){
  
  #------------------- check arguments
  msg <- checkArgInit()
  FUN <- function(x){
    inherits(x, "character") | inherits(x, "connection")
  }
  if(length(dsn) > 1){
    test <- sapply(dsn, FUN, USE.NAMES = FALSE)
  }else{
    test <- FUN(dsn)
  }
  if(!all(test)){
    msg <- c(msg, paste0("arg 'dsn': Must be a character or a ",
             "connection of length one or more\n"))
  } 
  msg <- checkArg(desc,      msg, "STRING")
  msg <- checkArg(Vmax,      msg, "NUMERIC1_NULL", Inf)
  msg <- checkArg(verbose,   msg, "LOGICAL_LEN",   1)
  msg <- checkArg(interpGPS, msg, "LOGICAL_LEN",   1)
  checkArgStop(msg)
  #------------------- end check
  
  if(length(dsn) == 1 && inherits(dsn, "connection")){
    fPath <- getFPath(dsn)
  }else{
    dsn <- Filter(Negate(is.null), dsn)
    # file path, name and extension for character or connection
    fPath <- sapply(dsn,   getFPath,   USE.NAMES = FALSE)
  }
  ext   <- sapply(fPath, .fExt,      USE.NAMES = FALSE)
  fName <- sapply(fPath, .fNameWExt, USE.NAMES = FALSE)
  if(!is.list(dsn)){
    if(inherits(dsn, "character")) dsn <- as.list(dsn)
    if(inherits(dsn, "connection")){
      dsn <- list(dsn)
    } 
  } 
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
      x_gps <-  verboseF(readGPS(dsn[["GPS"]], UTM = UTM), verbose = verbose)
    }
    
  #----------------------------------- MALA -----------------------------------#
  #-------------------------- RD3 + RAD (+ COR) -------------------------------#
 }else if(any( c("RD3", "RD7") %in% toupper(ext) )){
    if(all(sapply(dsn, inherits, "connection"))){
      if(!("RAD" %in% toupper(ext))) stop("Missing connection to '*.rad' file.")
    }else{
      ii <- grep("RD3|RD7", fPath,  ignore.case = TRUE)
      if(is.na(fPath["RAD"])) fPath["RAD"] <- fPath[ii[1]]
      if(is.na(fPath["COR"])) fPath["COR"] <- fPath[ii[1]]
      dsn[["RD37"]] <-  getFName(fPath[ii[1]], ext = paste0(".", toupper(ext)))[[tolower(ext)]]
      dsn[["RAD"]] <-  getFName(fPath["RAD"],  ext = ".RAD" )$rad
      dsn[["COR"]] <-  getFName(fPath["COR"], ext = ".COR",
                                throwError = FALSE)$cor
    }
    # nbytes <- 4
    nbytes <- ifelse(grepl("RD7", ext, ignore.case = TRUE), 4, 2)
    # i <- which("RD7" == toupper(ext) )
    # if(length(i) == 0){
    #   i <- which("RD3" == toupper(ext) )
    #   nbytes <- 2
    # }
    
    rad  <- verboseF( readRAD(dsn[["RAD"]]), verbose = verbose)
    rd37 <-  verboseF( readRD37(dsn[["RD37"]], ntr = rad$ntr, npt = rad$npt,
                                nbytes = nbytes), verbose = verbose)
    
    x <- verboseF( .gprRD3(list(hd = rad$HD, data = rd37),
                           fName = fName[ii[1]], fPath = fPath[ii[1]],
                           desc = desc, nbits = 8*nbytes, Vmax = Vmax),
                   verbose = verbose)
    if( !is.null(dsn[["COR"]])){
      x_gps <-  verboseF(readCOR(dsn[["COR"]]), verbose = verbose)
    }
 # --- --- --- --- --- --- --- --- --- ---
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
      # if(is.null(x_gps)){
      #   message("I couldn't find coordinates in GPS file.")
      # }
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
  #             "emanuel.huber@pm.me")
  #   }else{
  #     warning("Still experimental. Don't hesitate to contact me:\n",
  #             "emanuel.huber@pm.me")
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
  }else if("RDS" %in% toupper(ext)){
     x <- verboseF( .read_RDS(dsn[["RDS"]]), verbose = verbose)
  
  }else{
     stop("File extension not recognised!\n",
          "Must be '.DT1', '.dzt', '.rd3', '.sgy', '.segy', '.rds'\n",
          "'.iprb', '.iprh', '.dat', or '.vol'.")
  }
  #-----
  if(!is.null(x_gps)){
    if(!inherits(x_gps, "sf")) stop("'x_gps' must inherit 'sf'!")
    if(interpGPS){
      if(verbose) message("Coordinates interpolation from GPS data")
      x <- tryCatch({
        dots       <- list(...)
        r         <- dots[["r"]]               # default = NULL
        # UTM <- ifelse(is.null(dots[["UTM"]]), TRUE, dots[["UTM"]])
        interp3D  <- ifelse(is.null(dots[["interp3D"]]) , FALSE, dots[["interp3D"]])       # default = NULL
        tol       <- dots[["tol"]]             # default = NULL
        plot      <- ifelse(is.null(dots[["plot"]]), FALSE, dots[["plot"]])
        method    <- c("linear", "linear", "linear")
        if(!is.null(dots[["method"]])){
          if(length(dots[["method"]]) == 3){
            method <- dots[["method"]]
          }else{
            if(verbose) warning("'method' must have 3 elements. I take default values.")
          }
        }
        x <- interpCoords(x, x_gps, tt = NULL, r = r, 
                 UTM = UTM, 
                 interp3D = interp3D,
                 tol = tol, verbose = verbose, plot = plot,
                 method = method)
        # x <- interpPos(x, x_gps, tol = sqrt(.Machine$double.eps),
        #                method = method)
        x
      },
      error = function(cond){
        if(verbose) message("I could not interpolate the GPS data. You can retrieve the GPS data with `metadata(x)$GPS`")
        x@md[["GPS"]] <- x_gps
        return(x)
      })
    }else{
      x@md[["GPS"]] <- x_gps
      if(verbose) message("I found GPS coordinates. You can retrieve them with `metadata(x)$GPS`")
    }
  }else if(isTRUE(verbose) && isTRUE(interpGPS) && !is.null(dsn[["GPS"]])){
    warning(x@name, ": Either I could not find a GPS file or ",
              "could not find coordinates in the GPS file.") 
  }
  #-----
  if(grepl("CMP", x@mode)){
    x@mode <- "CMP"
    x@xlab <- "Antenna separation"
    if(length(x@rec) == 0 || length(x@trans) == 0){
      x@antsep <- x@x #seq(x@antsep, by = x@dx, length.out = length(x))
    }else{
      x@antsep <- sqrt(colSums((x@rec - x@trans)^2))
    }
  }
  return(x)
}













