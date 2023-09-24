#------------------------------------------#
#----------- CLASS DEFINITION -------------#

#' Class GPR
#' 
#' An S4 class to represent a ground-penetrating radar (GPR) data.
#' 
#' @section Survey mode:  Difference between reflection and CMP and WARR
#' \describe{
#'   \item{reflection}{The trace positions increase from trace to trace. The
#'                     antenna separation distance is constant.
#'                     Amplitude = f(depth, position)}
#'   \item{CMP}{The trace positions, \code{x@pos}, are constant and equal to 
#'              zero. The antenna separation distance increases increase from
#'              trace to trace.
#'              Amplitude = f(depth, antsep)}
#'   \item{WARR}{The trace positions, \code{x@pos}, are not constant because
#'              while the signal is emitted from the same position, the signal
#'              the signal is recorded from different positions. 
#'              The antenna separation distance increases increase from
#'              trace to trace.
#'              Is "CMP" a special case of "WARR"?
#'              Amplitude = f(depth, antsep)}
#'   \item{CMPAnalysis}{No trace positions, \code{x@pos = numeric(0)},
#'                      no antenna separation, \code{x@antsep = numeric(0)},
#'                      coherence/semblance = f(depth, velocity)}
#' }
#' @slot version A length-one character vector indicating the version of RGPR
#' @slot data A \eqn{m \times n} numeric matrix consiting of a 
#'            cross-section of signal amplitudes as a function of the GPR 
#'            position. The columns of \code{data} correspond to the GPR 
#'            traces and the row of \code{data} to the time/depth samples.
#' @slot traces A length-m numeric vector corresponding to the trace number.
#' @slot depth A length-n numeric vector indicating the sampling time or
#'              the vertical position of the trace samples.
#' @slot pos A length-m numeric vector indicating the relative position of
#'            the trace along the survey profile.
#' @slot time0 A length-m numeric vector containing the 'time-zero' of every
#'              trace.
#' @slot time A length-m numeric vector containing the recording time of 
#'            every trace.
#' @slot fid A length-m character vector containing fiducial markers associated
#'           with the traces.
#' @slot ann A length-m character vector containing annotations associated 
#'          with the traces.
#' @slot coord A \eqn{m \times 3} matrix containing the (x, y, z) positions of
#'             every trace.
#' @slot rec  A \eqn{m \times 3} matrix containing the (x, y, z) positions of
#'             the receiver for every trace.
#' @slot trans A \eqn{m \times 3} matrix containing the (x, y, z) positions of
#'             the transmitter for every trace.
#' @slot coordref A length-3 numeric vector containing the coordinates of a
#'                local reference.
#' @slot freq A length-one numeric vector corresponding to the GPR antennae
#'            frequency (in MHz).
#' @slot dz A length-one numeric vector corresponding to the time or depth
#'          sampling step.
#' @slot dx A length-one numeric vector corresponding to the trace step.
#' @slot antsep A length-one numeric vector corresponding to the antenna
#'              separation.
#' @slot name A length-one character vector containing the name of the GPR
#'            data.
#' @slot description A length-one character vector containing the description 
#'                  of the GPR data.
#' @slot filepath A length-one character vector containing the file path of 
#'                the original GPR data.
#' @slot depthunit A length-one character vector corresponding to the 
#'                  time/depth unit (e.g., "ns", "m").
#' @slot posunit A length-one character vector corresponding to the 
#'               (x, y)-unit (e.g., "m").
#' @slot surveymode A length-one character vector containing the survey mode
#'                  (e.g., "Reflection", "CMP")
#' @slot date A length-one character vector containing the date of the survey
#'            in the format "yyyy-mm-dd".
#' @slot crs A length-one character vector containing the coordinate 
#'          reference system following the R notation of proj4string 
#'          from the PROJ.4 library. 
#' @slot proc A length-varying character vector whose each element correspond
#'            to a processing step applied to the data.
#' @slot vel  A list containing the velocity model.
#' @slot delineations A list containing delineated structures.
#' @slot hd A list containing less relevant additional informations.
#' @name GPR-class
#' @rdname GPR-class
#' @export
setClass(
  Class="GPR",  
  slots=c(
    version      = "character",   # version of the class
    data         = "array",     # matrix one column per trace
    traces       = "numeric",  # numbering of each trace (from 1 to ntr)
    depth        = "numeric",  # depth position
    pos          = "numeric",    # position  of the traces
    time0        = "numeric",  # time-zero (first air-wave arrival)
    time         = "numeric",   # time of the trace recording
    fid          = "character",   # fiducial marks, defaults = rep("", ncol(x))!
    ann          = "character",  # annotation (e.g. intersections)
    coord        = "matrix",   # coordinates (x,y,z) of each traces
    rec          = "matrix",     # coordinates (x,y,z) of the receiver antenna
    trans        = "matrix",   # coordinates (x,y,z) of the transmitter antenna
    coordref     = "numeric", # coordinates references
    freq         = "numeric",   # antenna frequency
    dz           = "numeric",   # time/depth sampling
    dx           = "numeric",     # spatial trace sampling
    antsep       = "numeric",   # antenna separation
    name         = "character",  # name of the profile
    description  = "character",  # description of the pro
    filepath     = "character",  # filepath of the profile
    depthunit    = "character", # time/depth unit
    posunit      = "character",  # spatial unit
    surveymode   = "character", # survey mode (reflection/CMP)
    date         = "character",    # date of the survey , format %Y-%m-%d
    crs          = "character",  # coordinate reference system of coord
    proc         = "character",  # processing steps
    vel          = "list",      # velocity model
    delineations = "list",  # delineated lines
    hd           = "list"      # header from *.dt1 file
  )
)




#============================== READ GPR DATA =================================#


# 
# 
# .gprImpulseRadar <- function(x, fName = character(0), desc = character(0),
#                              fPath = character(0), Vmax = 50){ 
#   rec_coord <- matrix(nrow = 0, ncol = 0)
#   trans_coord <- matrix(nrow = 0, ncol = 0)
#   coord <- matrix(nrow = 0, ncol = 0)
#   pos_used <- integer(nrow(x$hd))
#   nbits <- .getHD(x$hd, "DATA VERSION", position = TRUE)
#   if(!is.null(nbits)){
#     pos_used[nbits[2]] <- 1L
#   }else{
#     nbits <- 16
#   }
#   nTr <- ncol(x$data)
#   dx <- .getHD(x$hd, "USER DISTANCE INTERVAL", position = TRUE)
#   if(!is.null(dx)){
#     pos_used[dx[2]] <- 1L
#   }else{
#     dx <- 1
#   }
#   ttw  <- .getHD(x$hd,"TIMEWINDOW", position = TRUE)
#   if(!is.null(ttw)){
#     dz <- ttw[1]/nrow(x$data)
#     pos_used[ttw[2]] <- 1L
#   }else{
#     warning("time/depth resolution unknown! I take dz = 0.4 ns!\n")
#     dz <- 0.4
#     ttw  <- nrow(x$data) * dz
#   }
#   nT0 <- .getHD(x$hd, "ZERO LEVEL", position = TRUE)
#   if(!is.null(nT0)){
#     pos_used[nT0[2]] <- 1L
#   }else{
#     nT0 <- 1
#   }
#   if(!is.null(x$time)){
#     traceTime <- as.double(as.POSIXct(paste(x$time[,2], x$time[,3])))
#   }else{
#     traceTime <- rep(0, nTr)
#   }
#   afreq <- .getHD(x$hd, "ANTENNA", position = TRUE, number = FALSE)
#   if(!is.null(afreq)){
#     antfreq <- freqFromString(afreq[1])
#     pos_used[as.integer(afreq[2])] <- 1L
#   }else{
#     antfreq <- 0
#     message("Antenna frequency set to 0 MHz. Set it with 'antfreq(x) <- ... '")
#   }
#   antsep <- .getHD(x$hd, "ANTENNA SEPARATION", position = TRUE)
#   if(!is.null(antsep)){
#     pos_used[antsep[2]] <- 1L
#   }else{
#     # antsep[1] <- antSepFromAntFreq(antfreq)
#     antsep[1] <- 0
#     message("Antenna separation set to 0 ", "m", 
#             ". Set it with 'antsep(x) <- ... '")
#   }
#   surveyDate <- .getHD(x$hd, "DATE", position = TRUE, number = FALSE)
#   if(!is.null(surveyDate)){
#     d <- as.character(as.Date(surveyDate[1], "%Y-%m-%d"))
#     pos_used[surveyDate[2]] <- 1L
#   }else{
#     surveyDate[1] <- 0
#   }
#   x$hd2 <- x$hd[!pos_used,]
#   if(nrow(x$hd2) > 0){
#     key <-  trimStr(x$hd2[,1])
#     test <- key!=""
#     key <- key[test]
#     key2 <- gsub("[[:punct:]]", replacement = "", key)
#     key2 <- gsub(" ", replacement = "_", key2)
#     nameL <- trimStr(x$hd2[test,2])
#     names(nameL) <- as.character(key2)
#     sup_hd <- as.list(nameL)
#   }
#   
#   new("GPR",   version="0.2",
#       data = bits2volt(Vmax = Vmax, nbits = nbits[1])*x$data,
#       traces = seq_len(nTr),                       # trace number
#       fid = rep("", nTr),                          # markes/fid
#       coord = coord,                               # trace coordinates
#       pos = seq(0, by = dx[1], length.out = nTr),  # trace position
#       depth = seq(0, by = dz, length.out = nrow(x$data)),
#       rec = rec_coord,                             # recorder coordinates
#       trans = trans_coord,                         # transmitter coordinates
#       time0 = rep((nT0[1] - 1) * dz, nTr),            # time-zero
#       time = traceTime,                            # sampling time
#       proc = character(0),                         # processing steps
#       vel = list(0.1),                             # m/ns
#       name = fName,
#       description = desc,
#       filepath = fPath,
#       dz = dz, 
#       dx = dx[1],                                   # "STEP SIZE USED"
#       depthunit = "ns",
#       posunit = "m",
#       freq = antfreq, 
#       antsep = antsep[1], 
#       surveymode = "reflection",
#       date = d,
#       crs = character(0),
#       hd = sup_hd                      # header
#   )
# }
# 
# 
# 




# .gprDZT <- function(x, fName = character(0), desc = character(0),
#                     fPath = character(0), Vmax = 50, ch = 1){
#   
#   #  take the channel ch
#   if(ch > length(x$data)){
#     stop("The data has only ", length(x$data), "channel(s)")
#   }
#   x$data <- x$data[[ch]]
#   antName <- x$hd$ANT[ch]
#   
#   dd <- as.Date(x$hd$DATE, format = "%Y-%m-%d")
#   dd <- as.character(dd)
#   if(is.na(dd)){
#     dd <- format(Sys.time(), "%Y-%m-%d")
#   }
#   ttime <- yy <- 1/x$hd$SPS * (seq_len(ncol(x$data)) - 1)
#   traceTime <- as.double(as.POSIXct(strptime(paste(dd, "01:30:00"), 
#                                              "%Y-%m-%d %H:%M:%S") )) + ttime
#   if(length(fName) == 0){
#     x_name <- "LINE"
#   }else{
#     x_name <- fName
#   }
#   # defaults
#   x_posunit   <- "m"
#   x_depthunit <- "ns"
#   x_pos       <- x$pos[1:ncol(x$data)]
#   x_depth     <- x$depth[1:nrow(x$data)]
#   x_dx        <- 1 / x$hd$SPM
# 
#   # Fiducial markers > each class has a different name (letter)
#   x_fid       <- rep("", ncol(x$data))
#   test <- which(x$hd$MRKS < 0)
#   fidval <- LETTERS[as.numeric(as.factor(x$hd$MRKS[test]))]
#   ufidval <- unique(fidval)
#   for( i in seq_along(ufidval)){
#     test2 <- which(fidval == ufidval[i])
#     fid_nb <- seq_along(test2)
#     x_fid[test][test2] <- paste0(ufidval[i], 
#                                  sprintf(paste0("%0", max(nchar(fid_nb)), "d"), 
#                                                      fid_nb))
#   }
#   
#   if(!is.null(x$dzx)){
#     # spatial/horizontal units
#     # pos
#     if(!is.null(x$dzx$pos)){
#       x_pos <- x$dzx$pos
#       # x_dx <- mean(diff(x_pos))
#     }
#     # spatial sampling
#     if(!is.null(x$dzx$dx)){
#       x_dx <- x$dzx$dx
#     } 
#     # if(!is.null(x$dzx$unitsPerScan)){
#     #   x_dx <- x$dzx$unitsPerScan
#     # }
#     # fids/markers
#     if(all(x_fid == "") &&
#        !is.null(x$dzx$markers) && 
#        length(x$dzx$markers) == ncol(x$data)){
#       x_fid <- x$dzx$markers
#     }
#     # else if(all(x_fid == "") && !is.null(x$dzx$unitsPerMark)){
#     #   x_fid <- rep("", ncol(x$data))
#     #   x_fid_id <- which((x_pos %% x$dzx$unitsPerMark) == 0)
#     #   x_fid[x_fid_id] <- "FID"
#     # }
#     if(!is.null(x$dzx$hUnit)){
#       x_posunit <-x$dzx$hUnit
#       if(grepl("in", x_posunit)){
#         x_pos <- x_pos * 0.0254
#         x_posunit <- "m"
#       }
#     }
#     # # depth/vertical units
#     # if(!is.null(x$dzx$vUnit)){
#     #   x_depthunit <- x$dzx$vUnit
#     #   if(grepl("in", x_depthunit)){
#     #     x_depth <- x_depth * 0.0254
#     #     x_depthunit <- "m"
#     #   }
#     # }
#   }
#   antfreq <- switch(antName,
#                     '3200'   = numeric(0), # adjustable
#                     '3200MLF' = numeric(0), # adjustable
#                     '500MHz' = 500,
#                     '3207' = 100,
#                     '3207AP' = 100,
#                     '5106' = 200,
#                     '5106A' = 200,
#                     '50300' = 300,
#                     '350' = 350,
#                     '350HS' = 350,
#                     '50270' = 270,
#                     '50270S' = 270,
#                     '50400' = 400,
#                     '50400S' = 400,
#                     '800' = 800,
#                     '3101' = 900,
#                     '3101A' = 900,
#                     '51600' = 1600,
#                     '51600S' = 1600,
#                     '62000' = 2000,
#                     '62000-003' = 2000,
#                     '62300' = 2300,
#                     '62300XT' = 2300,
#                     '52600' = 2600,
#                     '52600S' = 2600,
#                     'D50800' = 800,
#                     numeric(0))  # 800,300,
#   if(length(antfreq) == 0){
#     # estimate anntenna frequency from the name (it it contains ### MHz)
#     antfreq <- freqFromString(antName)
#   }
#   if(length(antfreq) == 0){
#     antfreq <- 0
#     message("Antenna frequency set to 0 MHz. Set it with 'antfreq(x) <- ... '")
#    # antsep <- numeric(0)
#   }
#   #else{
#   #}
#   v <- 2 * x$hd$DEPTH / x$hd$RANGE
#   
#   # antenna sparation could be estimated from frequency...
#   # antsep <- antSepFromAntFreq(antfreq)
#   antsep <- 0
#   message("Antenna separation set to 0 ", x_posunit, 
#           ". Set it with 'antsep(x) <- ... '")
#   
#   new("GPR",   
#       version      = "0.2",
#       data        = bits2volt(Vmax = Vmax, nbits = x$hd$BITS) * x$data,
#       traces      = 1:ncol(x$data),
#       fid         = x_fid,
#       #coord = coord,
#       coord       = matrix(nrow = 0, ncol = 0),
#       pos         = x_pos,
#       depth       = x$depth[1:nrow(x$data)],
#       rec         = matrix(nrow = 0, ncol = 0),
#       trans       = matrix(nrow = 0, ncol = 0),
#       time0       = rep(0, ncol(x$data)),
#       # time = x$hdt[1,] * 3600 + x$hdt[2,] * 60 + x$hdt[3,],
#       time        = traceTime,
#       proc        = character(0),
#       vel         = list(v),
#       name        = x_name,
#       description = desc,
#       filepath    = fPath,
#       dz          =  x$hd$RANGE /  (x$hd$NSAMP - 1 ), 
#       dx          = x_dx,
#       depthunit   = x_depthunit,
#       posunit     = x_posunit,
#       freq        = antfreq, 
#       antsep      = antsep,     # check
#       surveymode  = "reflection",
#       date        = as.character(dd), #format(Sys.time(), "%d/%m/%Y"),
#       crs         = character(0),
#       hd          = x$hd
#   )
# }

# readSGY <- function(dsn, fName = "", fPath = "", desc = "", 
#                     Vmax = 50, verbose = TRUE){
#   x <- suppressMessages( suppressWarnings(rgdal::readOGR(dsn = dsn)))
#   i <- grep("SAMPLE_ARRAY", names(x))
#   # xi <- x[i]
#   # 
#   # slotNames(xi)
#   # 
#   # dim(xi@data)
#   
#   x_data <- t(as.matrix(x[i]@data))
#   x_dz <- mean(x[["SAMPLE_INTERVAL"]]/1000)
#   
#   # plot3D::image2D(as.matrix(xi@data))
#   y <- list(version = "0.2",
#             data = x_data * bits2volt(Vmax = Vmax),
#             name = fName,
#             description = desc,
#             filepath = fPath,
#             surveymode = "reflection",
#             traces = seq_len(ncol(x_data)),
#             freq = 0,                      # FIXME
#             dx = 1,                        # FIXME
#             time0 = rep(0, ncol(x_data)),  # FIXME
#             # time = ,
#             fid = rep("", ncol(x_data)),   # FIXME
#             ann = rep("", ncol(x_data)),   # FIXME
#             dz = x_dz,
#             depth = seq(0, by = x_dz, length.out = nrow(x_data)),
#             pos = seq_len(ncol(x_data)),   # FIXME
#             antsep = 0,
#             posunit = "m",
#             depthunit = "ns"
#   )
#   message("Set trace position with either 'pos(x) < -...' or 'coord(x) < -...'")
#   message("Set antenna frequency with 'antfreq(x) < -...' ")
#   message("Set antenna separation distance with 'antsep(x) < -...' ")
#   as(y, "GPR")
#   
# }



# .gprUtsi <- function(x, fName = fName, fPath = fPath, 
#          desc = desc, Vmax = Vmax){
#   
#   y <- new("GPR", 
#            version     = "0.2",
#            data        = x[["data"]] * bits2volt(Vmax = Vmax, 
#                                                  nbits = x$hd$bits),
#            traces      = 1:ncol(x[["data"]]),       # trace numbering
#            pos         = 1:ncol(x[["data"]]),                # trace position
#            depth       = 1:nrow(x[["data"]]),
#            time0       = rep(0, ncol(x[["data"]])),  
#            time        = rep(0, ncol(x[["data"]])), # time of trace records
#            proc        =  character(0),       
#            vel         = list(0.1),                 # m/ns
#            name        = fName,
#            description = desc,
#            filepath    = fPath,
#            fid         = trimStr(x[['fid']]),
#            dz          = 1, 
#            dx          = 1, 
#            depthunit   = "ns",
#            posunit     = "m",
#            freq        = 0,
#            antsep      = 0, 
#            surveymode  = "reflection",
#            date        = format(Sys.time(), "%Y-%m-%d"),
#            crs         = character(0),
#            hd          = list())
#   return(y)
# }

# #' Read a GPR data file
# #' 
# #' Note: argument \code{fPath} is depreacted. Use \code{dsn} instead.
# #' 
# #' Supported file format
# #' \itemize{
# #'   \item Sensors & Software file format (*.dt1 , *.hd).
# #'         \code{readGPR(dsn = 'xline.dt1')}
# #'   \item MALA file format (*.rd3, *.rad).
# #'         \code{readGPR(dsn = 'xline.rd3')}
# #'   \item RadSys Zond GPR device (*.sgy).
# #'         \strong{Note: it is not the SEG-Y file format)}.
# #'         \code{readGPR(dsn = 'xline.sgy')}
# #'   \item GSSI file format (*.dzt).
# #'         \code{readGPR(dsn = 'xline.dzt')}
# #'   \item ASCII file format (*.txt): either 4-column format
# #'         (x,t,amplitude) or matrix-format (without header/rownames).
# #'         \code{readGPR(dsn = 'xline.txt')}
# #'   \item R object file format (*rds). These files are created by saving the
# #'         \code{GPR} object with
# #'         \code{writeGPR(x, fPath = 'xline.rds', type = "rds")}.
# #'         \code{readGPR(dsn = 'xline.txt')}
# #' }
# #' @param dsn data source name: either the filepath to the GPR data (character),
# #'            or an open file connection.
# #' @param desc Short description of the file (character).
# #' @param dsn2 data source name for additional file connection if \code{dsn} is
# #'            an open file connection (e.g., open file connection to '*.hd'
# #'            file if \code{ds} is an open file connection to a '*.dt1' file).
# #' @param format lenth-one character vector required if the file extension is
# #'               not appearent in the filepath or the connection (either
# #'               \code{dt1}, \code{rad}, \code{dzt}, \code{sgy}, \code{iprb},
# #'               \code{txt}, \code{rds})
# #' @param Vmax length-one numeric vector: nominal analog input voltage used
# #'             for the bits to volt transformation.
# #'             It assumes that \code{Vmin = -Vmax}. If \code{Vmax = NULL},
# #'             no bits to Volt transformation is applied.
# #' @param fPath Filepath (character). DEPRECATED. Use \code{dsn} instead.
# #' @param ch For multi-frequency GSSI files (*.dzt), which channel is red.
# #' @param verbose (boolean). If \code{FALSE}, all messages and warnings are
# #'                suppressed (use with care).
# #' @return The GPR data as object of the class RGPR.
# #' @seealso \code{\link{writeGPR}}
# #' @examples
# #' \dontrun{
# #' # argument dsn is a file path
# #' x1 <- readGPR(dsn = "data/RD3/DAT_0052.rd3")
# #' y1 <- readGPR("data/FILE____050.DZT")
# #' 
# #' # argument dsn is a connection
# #' con <- file("data/RD3/DAT_0052.rd3", "rb")   # binary mode
# #' con2 <- file("data/RD3/DAT_0052.rad", "rt")  # text mode
# #' x2 <- readGPR(dsn = con, dsn2 = con2)
# #' close(con)
# #' close(con2)
# #' 
# #' con <- file(dsn = "data/FILE____050.DZT", "rb")
# #' y1 <- readGPR(con)
# #' close(con)
# #' }
# #' @name readGPR
# #' @rdname readGPR
# #' @export
# readGPR <- function(dsn, desc = "", dsn2 = NULL, format = NULL, Vmax = 50,
#                     fPath, ch = 1, verbose = TRUE){
#   # @aliases readGPR-methods
#   # setMethod("readGPR", "character", function(fPath, desc = "", ...){
#   if(!missing(fPath)){
#     if(missing(dsn)){
#       dsn <- fPath
#     }
#     warning("Use argument 'dsn' instead of 'fPath' because ",
#             "argument 'fPath' is deprecated.")
#   }
#   if( inherits(dsn, "connection") ){
#     summaryCon <- summary.connection(dsn)
#     fPath <- summaryCon$description
#     if(!is.null(format)){
#       ext <- format[1]
#     }else{
#       ext <- .fExt(summaryCon$description)
#     }
#     fName <- .fNameWExt(fPath)
#   }else{
#     fPath <- dsn
#     ext <- .fExt(fPath)
#     fName <- .fNameWExt(fPath)
#   }
#   # DT1
#   if("DT1" == toupper(ext) || "HD" == toupper(ext)){
#     # fName <- .fNameWExt(fPath)
#     A <- verboseF( readDT1(dsn, dsn2), verbose = verbose)
#     x <- verboseF( .gpr(A, fName = fName, fPath = fPath, 
#                         desc = desc, Vmax = Vmax),  verbose = verbose)
#   }else if("rds" == tolower(ext)){
#     x <- verboseF( readRDS(dsn), verbose = verbose)
#     if(class(x) == "GPR"){
#       x@filepath <- fPath
#     }else if(class(x) == "list"){
#       versRGPR <- x[["version"]]
#       if(versRGPR == "0.1"){
#         for(i in seq_along(x[['delineations']])){
#           x[['delineations']][[i]][, 5] <- -x[['delineations']][[i]][, 5]
#         }
#       }
#       y <- new("GPR",
#                version = x[['version']],
#                data = x[['data']],
#                traces = x[['traces']],           # x$dt1$traces
#                depth = x[['depth']],
#                pos = x[['pos']],                 # x$dt1$position of the traces
#                time0 = x[['time0']],             # x$dt1$time0
#                time = x[['time']],               # x$dt1$time
#                fid = trimStr(x[['fid']]),        # x$dt1$fid <-> x$dt1$x8
#                ann = trimStr(x[['ann']]),        # x$dt1$fid <-> x$dt1$x8
#                coord = x[['coord']],             # x$dt1$topo  of the traces
#                rec = x[['rec']],                # x$dt1$recx,x$dt1$recy,x$dt1$recz
#                trans = x[['trans']],
#                coordref = x[['coordref']],       # x$dt1$topo of the traces
#                freq = x[['freq']], 
#                dz = x[['dz']], 
#                dx = x[['dx']], 
#                antsep = x[['antsep']], 
#                name = x[['name']],
#                description = x[['description']],
#                filepath =x[['filepath']],
#                depthunit = x[['depthunit']],
#                posunit = x[['posunit']],
#                surveymode = x[['surveymode']],
#                date = x[['date']],
#                crs = x[['crs']],
#                proc = x[['proc']],               # processing steps
#                vel = x[['vel']],                 # m/ns
#                delineations = x[['delineations']],
#                hd =  x[['hd']]                   # header
#       )
#       y@filepath <- fPath
#       x <- y
#     }
#   }else if("RD3" == toupper(ext) || "RAD" == toupper(ext)){
#     # fName <- .fNameWExt(fPath)
#     A <- verboseF( readRD3(dsn, dsn2), verbose = verbose)
#     x <- verboseF( .gprRD3(A, fName = fName, fPath = fPath, 
#                            desc = desc, Vmax = Vmax), verbose = verbose)
#   }else if("RD7" == toupper(ext) || "RAD" == toupper(ext)){
#     # fName <- .fNameWExt(fPath)
#     A <- verboseF( readRD7(dsn, dsn2), verbose = verbose)
#     x <- verboseF( .gprRD3(A, fName = fName, fPath = fPath, desc = desc, 
#                            nbits = 32, Vmax = Vmax), verbose = verbose)
#   }else if("SGY" == toupper(ext) || "SEGY" == toupper(ext)){
#     if( !inherits(dsn, "connection") ){
#       dsn <- file(dsn, "rb")
#     }
#     ENDIAN <- "big"
#     THD <- readSGY_textual_file_header(dsn, ENDIAN = "big")
#     test <- FALSE
#     # if(all(validEnc(THD))){
#       test <- any(verboseF(grepl("Prism", THD), verbose = FALSE)) & 
#               any(verboseF(grepl("Radar Systems, Inc.", THD), 
#                         verbose = FALSE))
#     # }
#     # read RadSys Zond System
#     if( test ){
#       A <- verboseF( readSEGY_RadSys_Zond_GPR(dsn), verbose = verbose)
#       x <- verboseF( .gprSEGY(A, fName = fName, fPath = fPath, 
#                               desc = desc, Vmax = Vmax), verbose = verbose)
#     # read classical SEG-Y file
#     }else{
#       A <- verboseF(readSGY(dsn), verbose = verbose)
#       x <- verboseF( .gprSGY(A, fName = fName, fPath = fPath, 
#                              desc = desc, Vmax = Vmax), verbose = verbose)
#       return(x)
#     }
#     # A <- tryCatch({verboseF(readSGY(dsn), 
#     #                         verbose = verbose)},
#     #               error = function(e){return(NULL)})
#     # if(is.null(A)){
#     #   # z <- readGPR(dsn)
#     #   if(in)
#     #   A <- verboseF( readSEGY_RadSys_Zond_GPR(dsn), verbose = verbose)
#     #   x <- verboseF( .gprSEGY(A, fName = fName, fPath = fPath, 
#     #                           desc = desc, Vmax = Vmax), verbose = verbose)
#     # }else{
#     #   x <- verboseF( .gprSGY(A, fName = fName, fPath = fPath, 
#     #                          desc = desc, Vmax = Vmax), verbose = verbose)
#     #   return(x)
#     # }
#   }else if("IPRB" == toupper(ext) || "IPRH" == toupper(ext)){
#     # fName <- .fNameWExt(fPath)
#     A <- verboseF( readImpulseRadar(dsn, dsn2), verbose = verbose)
#     x <- verboseF( .gprImpulseRadar(A, fName = fName, fPath = fPath, 
#                           desc = desc, Vmax = Vmax), verbose = verbose)
#   }else if("DZT" == toupper(ext)){
#     # fName <- .fNameWExt(fPath)
#     A <- verboseF( readDZT(dsn), verbose = verbose)
#     x <- verboseF( .gprDZT(A, fName = fName, fPath = fPath, 
#                  desc = desc, Vmax = Vmax, ch = ch), verbose = verbose)
#   }else if("VOL" == toupper(ext)){
#     A <- verboseF( readVOL(dsn), verbose = verbose)
#     x <- verboseF( .gprVOL(A, fName = fName, fPath = fPath, 
#                            desc = desc, Vmax = Vmax), verbose = verbose)
#     if(A$hd$dim == "3D"){
#       warning("return a 'GPRcube' object with complex numbers.",
#               " Current processing and plotting functions are likely to not ",
#               "work on the returned object. Please contact me:\n",
#               "emanuel.huber@pm.me")
#     }else{
#       warning("Still experimental. Don't hesitate to contact me:\n",
#               "emanuel.huber@pm.me")
#     }
#     return(x)
#   }else if("DAT" == toupper(ext) || "HDR" == toupper(ext)){
#     A <- verboseF( readUtsi(dsn), verbose = verbose)
#     x <- verboseF( .gprUtsi(A, fName = fName, fPath = fPath, 
#                            desc = desc, Vmax = Vmax), verbose = verbose)
#   }else if("TXT" == toupper(ext)){
#     # fName <- .fNameWExt(fPath)
#     A <- verboseF( readTXT(dsn), verbose = verbose)
#     x <- verboseF( .gprTXT(A, fName = fName, fPath = fPath, 
#                  desc = desc, Vmax = Vmax), verbose = verbose)
#   }else{
#     stop(paste0("File extension not recognised!\n",
#                 "Must be '.DT1', '.dzt', '.rd3', '.sgy', '.segy', '.rds'\n",
#                 "'.iprb', '.iprh', '.dat', or '.vol'."))
#   }
#   if(grepl("CMP", x@surveymode)){
#     x@surveymode <- "CMP"
#     if(length(x@rec) == 0 || length(x@trans) == 0){
#       x@antsep <- seq(x@antsep, by = x@dx, length.out = length(x))
#     }else{
#       x@antsep <- sqrt(colSums((x@rec - x@trans)^2))
#     }
#   }
#   return(x)
# }



#================================= COERCION ===================================#

###--- Coercion from GPR to ...
setAs(from = "GPR", to = "matrix", def = function(from){ from@data } )

#' Coercion to matrix
#'
#' @name as.matrix
#' @rdname GPRcoercion
#' @export
setMethod("as.matrix",signature(x="GPR"),function(x){as(x,"matrix")})

setAs(from = "GPR", to = "vector", def = function(from){ from@data})

#' Coercion to vector
#'
#' @name as.vector
#' @rdname GPRcoercion
#' @export
setMethod("as.vector", signature(x="GPR"), 
          function(x,mode="any"){as.vector(x@data)})

# #' @importClassesFrom sp SpatialLines
# setAs(from = "GPR", to = "SpatialLines",
#       def = function (from) as.SpatialLines(from))
# 
# #' Coercion to SpatialLines
# #'
# #' @name as.SpatialLines
# #' @rdname GPRcoercion
# #' @export
# # as.SpatialLines <- function (x, ...){
# setMethod("as.SpatialLines", signature(x = "GPR"), function(x){
#   myLine <- sp::Line(x@coord[,1:2])
#   myLines <- sp::Lines(list(myLine), ID = x@name)
#   mySpatLines <- sp::SpatialLines(list(myLines))
#   if(length(crs(x)) == 0){
#     warning("no CRS defined!\n")
#   }else{
#     sp::proj4string(mySpatLines) <- sp::CRS(crs(x))
#   }
#   return(mySpatLines)
# })



#' Coercion to numeric
#'
#' @name as.numeric
#' @rdname GPRcoercion
#' @export
setMethod("as.numeric", "GPR",  function(x, ...) as.numeric(x@data))


#' Coercion to integer
#'
#' @name as.integer
#' @rdname GPRcoercion
#' @export
setMethod("as.integer", "GPR",  function(x, ...) as.integer(as.numeric(x@data)))


setMethod("as.double", "GPR",  function(x, ...) as.double(x@data))

###--- Coercion from ... to GPR
setAs(from = "matrix", to = "GPR", def = function (from) as.GPR.matrix(from))

setAs(from = "data.frame", to = "GPR", def = function (from) as.GPR.data.frame(from))


#' Coercion from data.frame to GPR
#'
#' @name as.GPR.data.frame
#' @rdname GPRcoercion
#' @export
as.GPR.data.frame <- function(x, ...){
  as.GPR.matrix(as.matrix(x))
}


#' Coercion from matrix to GPR
#'
#' @name as.GPR.matrix
#' @rdname GPRcoercion
#' @export
as.GPR.matrix <- function (x, ...){
  new("GPR", 
      version = "0.2",
      data = x,
      traces = 1:ncol(x),           # x$dt1$traces
      fid = rep("", ncol(x)),
      pos = (1:ncol(x) -1)*0.25,    # x$dt1$position  of the traces
      depth = (1:nrow(x) -1)*0.8,
      time0 = rep(0,ncol(x)),       # x$dt1$time0
      time = rep(0,ncol(x)),        # x$dt1$time
      proc = character(0),          # processing steps
      vel = list(v = 0.1),              # m/ns
      name = character(0),
      description = character(0),
      filepath = character(0),
      dz = 0.8, 
      dx = 0.25, 
      depthunit = "ns",
      posunit = "m",
      freq = 100, 
      antsep = 1, 
      surveymode = "reflection",
      date = format(Sys.time(), "%Y-%m-%d"),
      crs = character(0),
      hd = list()                   # header
  )
}

setAs(from = "list", to = "GPR", def = function (from) as.GPR.list(from))

#' Coercion from list to GPR
#'
#' @name as.GPR.list
#' @rdname GPRcoercion
#' @export
as.GPR.list <- function (x, ...){
  # prefix: "d_" for default
  if(all("data" != tolower(names(x)))){
    stop("The list must have a 'data' index name")
  }
  x[["data"]] <- as.matrix(x[["data"]])
  if(!is.matrix(x[["data"]])){
    stop("The element 'data' must be a matrix!")
  }
    if(is.null(x[["pos"]]) && !is.null(x[["dx"]]) && is.numeric(x[["dx"]])){
      x[["pos"]] <- (1:ncol(x[["data"]]) -1) * x[["dx"]]
    }else if( is.null(x[["pos"]]) ){
      x[["pos"]] <- (1:ncol(x[["data"]]) -1)*0.25
    }else{
      if(length(x[["pos"]]) != ncol(x[["data"]])){
        stop("length(x[['pos']]) must be equal to ncol(x[['data']]")
      }
      x[["dx"]] <- mean(diff(x[["pos"]]))
    }
    if(is.null(x[["depth"]]) && !is.null(x[["dz"]]) && is.numeric(x[["dz"]])){
      x[["depth"]] <- (1:nrow(x[["data"]]) -1) * x[["dz"]]
    }else if( is.null(x[["depth"]]) ){
      x[["depth"]] <- (1:nrow(x[["data"]]) -1)
    }else{
      if(length(x[["depth"]]) != nrow(x[["data"]])){
        stop("length(x[['depth']]) must be equal to nrow(x[['data']]")
      }      
      x[["dz"]] <- mean(diff(x[["depth"]]))
    }
    if(!is.null(x[["vel"]]) && !is.list(x[["vel"]])){
      x[["vel"]] <- list(v = x[["vel"]])
    }
    myArg <- as.list(match.call(definition = sys.function(-2),
                                call = sys.call(-2),
                                expand.dots = FALSE )
    )
    d_name <- paste(eval(myArg[2]))
    y <- new("GPR", 
             version     = "0.2",
             data        = as.matrix(x[["data"]]),
             traces      = 1:ncol(x[["data"]]),       # trace numbering
             fid          = rep("", ncol(x[["data"]])),
             pos         = x[["pos"]],                # trace position
             depth       = x[["depth"]],
             time0       = rep(0, ncol(x[["data"]])),  
             time        = rep(0, ncol(x[["data"]])), # time of trace records
             proc        =  character(0),       
             vel         = list(v = 0.1),                 # m/ns
             name        = as.character(d_name),
             description = paste0("coercion of ", as.character(d_name), 
                                  " (",typeof(x), ") into GPR"),
             filepath    = character(0),
             dz          = x[["dz"]], 
             dx          = x[["dx"]], 
             depthunit   = "ns",
             posunit     = "m",
             freq        = 100,
             antsep      = 1, 
             surveymode  = "reflection",
             date        = format(Sys.time(), "%Y-%m-%d"),
             crs         = character(0),
             hd          = list())
    sNames <- slotNames(y)
    sNames <- sNames[ !(sNames %in% c("data", "pos", "depth", "dz", "dx"))]
    for(i in seq_along(sNames)){
      if(!is.null(x[[sNames[i]]])){
        slot(y, sNames[i], check = TRUE) <- x[[sNames[i]]]
      }
    }
    return(y)
}    


#======================== MANIPULATING GPR OBJECTS ============================#

#' @export
setMethod("length", "GPR", function(x) ncol(x@data))

#' @export
setMethod("summary", "GPR", function(object, ...) 
  
#' @export
summary(as.vector(object@data)))

#' @export
setMethod("mean", "GPR", function(x, ...) mean(as.vector(x@data)))

#' @export
setMethod("median", "GPR", function(x, na.rm = FALSE) 
  median(as.vector(x@data),na.rm = FALSE))
# setMethod("range", "GPR", function(..., na.rm=FALSE) 
# range(as.matrix(...),na.rm=na.rm))

#' @export
setMethod(
  f = "apply", 
  signature = "GPR", 
  definition = function(X, MARGIN, FUN, ...){
    x_apply <- apply(X@data, MARGIN, FUN,...)
    if(MARGIN == 1 && is.null(dim(x_apply)) && length(x_apply) == nrow(X)){
      X[, 1:ncol(x_apply)] <- x_apply
    }
    return(x_apply)
  }
)


#' Form Row and Column Sums and Means
#' 
#' Form row and column sums and means 
#' @param x [\code{GPR}]
#' @param na.rm	[\code{logical(1)}]. Should missing values (including 
#' \code{NaN}) be omitted from the calculations?
#' @param dims [\code{integer(1)}]  Which dimensions are regarded as ‘rows’ or 
#'             ‘columns’ to sum over.(see \code{\link{colSums}}).
#' @aliases colSums,GPR-method
#' @rdname colSums
#' @export
setMethod("colSums", "GPR", function(x, na.rm = FALSE, dims = 1){
  x@data[1, ] <- colSums(x@data, na.rm = na.rm, dims = dims)
  x <- x[1,]
  return(x)
})

#' @aliases rowSums,GPR-method
#' @rdname colSums
#' @export
setMethod("rowSums", "GPR", function(x, na.rm = FALSE, dims = 1){
  x@data[, 1] <- rowSums(x@data, na.rm = na.rm, dims = dims)
  x <- x[,1]
  return(x)
})

#' @aliases colMeans,GPR-method
#' @rdname colSums
#' @export
setMethod("colMeans", "GPR", function(x, na.rm = FALSE, dims = 1){
  x@data[1, ] <-colMeans(x@data, na.rm = na.rm, dims = dims)
  x <- x[1,]
  return(x)
})

#' @aliases rowMeans,GPR-method
#' @rdname colSums
#' @export
setMethod("rowMeans", "GPR", function(x, na.rm = FALSE, dims = 1){
  x@data[, 1] <- rowMeans(x@data, na.rm = na.rm, dims = dims)
  x <- x[,1]
  return(x)
})


#' @export
setMethod(
  f="nrow", 
  signature="GPR", 
  definition=function(x){
    nrow(x@data)
  }
)

#' @export
setMethod(
  f="ncol", 
  signature="GPR", 
  definition=function(x){
    ncol(x@data)
  }
)
#' @export
setMethod(
  f="dim", 
  signature="GPR", 
  definition=function(x){
    dim(x@data)
  }
)





#=========================== GETTER / SETTER ==================================#

# ###' @rdname GPR-extract-methods
#' extract parts of GPR
#'
#' Object of the class GPR can be manipulated as matrix
#' @param x Object of class GPR
#' @param i integer
#' @param j integer
#' @name GPR-subset
#' @docType methods
#' @rdname GPR-subset
setMethod(
  f= "[",
  signature="GPR",
  definition=function(x, i, j, drop){
    rval <- x@data
    if(missing(i) && missing(j)){
      return(as.vector(x@data))
    }
    if(missing(i) || length(i) == 0) i <- seq_len(nrow(rval))
    #--- case 2D data
    if(length(dim(rval)) == 2) {
      # drop. <- ifelse(length(i) == 1, FALSE, drop)
      drop <- FALSE
      if(missing(j)){
        rval <- rval[i, , drop = drop]
        x@depth <- x@depth[i]
        if(!is.null(x@hd[["clip"]])){
          test <- .clipMat(x@hd[["clip"]], n = nrow(x))
          if(length(dim(test)) == 0){  # if not a matrix, there are no clipped values
            x@hd[["clip"]] <- NULL
          }else{
            x@hd[["clip"]][["clipmin"]] <- apply(test[i, , drop = FALSE], 2, function(x) which(x == -1))
            x@hd[["clip"]][["clipmax"]] <- apply(test[i, , drop = FALSE], 2, function(x) which(x == 1))
          }
        }
      }else { 
        if(length(j) == 0) j <- seq_len(ncol(rval))
        rval     <- rval[i, j, drop = drop]
        x@depth  <- x@depth[i]
        x@traces <- x@traces[j]
        x@pos    <- x@pos[j]
        x@time0  <- x@time0[j]
        x@time   <- x@time[j]
        x@fid    <- x@fid[j]
        if(length(x@antsep) > 1) x@antsep <- x@antsep[j]
        if(length(x@coord)  > 0) x@coord  <- x@coord[j, , drop = FALSE]
        if(length(x@rec)    > 0) x@rec    <- x@rec[j,   , drop = FALSE]
        if(length(x@trans)  > 0) x@trans  <- x@trans[j, , drop = FALSE]
        if(!is.null(x@hd[["clip"]])){
          if(!is.null(x@hd[["clip"]][["clipmin"]]) && length(x@hd[["clip"]][["clipmin"]]) >= max(j)){
            x@hd[["clip"]][["clipmin"]] <- x@hd[["clip"]][["clipmin"]][j]
          }else{
            x@hd[["clip"]][["clipmin"]] <- NULL
          }
          if(!is.null(x@hd[["clip"]][["clipmax"]]) && length(x@hd[["clip"]][["clipmax"]]) >= max(j)){
            x@hd[["clip"]][["clipmax"]] <- x@hd[["clip"]][["clipmax"]][j]
          }else{
            x@hd[["clip"]][["clipmax"]] <- NULL
          }
        }
      }
      if(drop && length(rval) == 1){ rval <- c(rval)}
    
    #--- case 1D data
    }else if(length(i) > 0){
      rval <- rval[i]
      x@depth <- x@depth[i]
      if(!is.null(x@hd[["clip"]])){
        test <- .clipMat(x@hd[["clip"]], n = nrow(x))
        x@hd[["clip"]][["clipmin"]] <- apply(test[i, , drop = FALSE], 2, function(x) which(x == -1))
        x@hd[["clip"]][["clipmax"]] <- apply(test[i, , drop = FALSE], 2, function(x) which(x == 1))
      }
    }
    x@dz <- abs(mean(diff(x@depth)))
    x@dx <- abs(mean(diff(x@pos)))
    x@data <- rval
    return(x)
  }
)


#' @name [<-
#' @rdname GPR-subset
setReplaceMethod(
  f="[",
  signature="GPR",
  definition=function(x,i,j,value){
    rval <- x@data
    n <- nrow(rval)
    if(missing(i)) i <- 1:n
    if(missing(j)){
      # if the indices object is a matrix, e.g., x[SEL], where SEL is a matric
      # containing TRUE/FALSE values
      if(length(dim(i)) == 2 ){
        i <- as.matrix(i)
        if(!is.matrix(i)){
          stop("invalid subscript: cannot be converted to matrix...")
        }
        x@data[i] <- value
        x@proc <- c(x@proc, "[<-")
        return (x)
      }else{
        j <- 1:ncol(x@data)
      }
    } #j <- 1:ncol(x@data)
    if(length(dim(x@data)) == 2) {
      x@data[i,j] <- value
    }else{
      rval <- rval[i]
    }
    x@proc <- c(x@proc, "[<-")
    return (x)
  }
)

#' Return data header
#'
#' Return data header
#' @rdname gethd
#' @export
setMethod("gethd", "GPR", function(x,hd=NULL){
  if(is.null(hd)){
    return(x@hd)
  }else{
    if(!is.null(x@hd[[hd]])){
      if(is.character(x@hd[[hd]])){
        as.character(x@hd[[hd]])
      }else{
        as.numeric(x@hd[[hd]])
      }
    }
  }
} 
)





#=============================== TEST FUNCTION: isXXX() =======================#


#' Return TRUE if surveymode is CMP
#' 
#' Return TRUE if surveymode is CMP
#' @name isCMP
#' @rdname isCMP
#' @export
setMethod("isCMP", "GPR", function(x){
  (grepl("CMP", toupper(x@surveymode)) || 
     grepl("WARR", toupper(x@surveymode))) && 
    !grepl("CMPANALYSIS", toupper(x@surveymode))
})


#' Return TRUE if the data are a function of time
#' 
#' @name isTimeUnit
#' @rdname isTimeUnit
#' @export
setMethod("isTimeUnit", "GPR", function(x){
  grepl("[s]$", x@depthunit)
})

#' Return TRUE if the data are a function of length
#' 
#' @name isLengthUnit
#' @rdname isLengthUnit
#' @export
setMethod("isLengthUnit", "GPR", function(x){
  !isTimeUnit(x)
} 
)




#============================= PROCESSING FUNCTIONS ===========================#


#' Trace projection
#'
#' Project the trace coordinates give a coordinate reference system.
#' @param x Object of the class GPR
#' @param CRSobj [\code{character(1)}] A string accepted by GDAL 
#'               (e.g., \code{"EPSG:2056"}, WKT-string).
#' @name trProject
#' @rdname trProject
#' @export
setMethod("trProject", "GPR", function(x, CRSobj){
  if(length(x@crs) == 0){
    stop("The data has no coordinate reference system (CRS).\n",
         "Therefore, I cannot project the coordinates in the new CRS.",
         "please add CRS with for example 'crs(x) <- \"+init=epsg:4326\".")
  }
  if(CRSobj == "UTM"){
    coordUTM <- llToUTM(lon = coord(x)[, 1],
                        lat = coord(x)[, 2])
    coord(x)[, 1:2] <- coordUTM$xy  # set coordinates
    crs(x) <- coordUTM$crs          # set estimated CRS
  }else{
    
    x@coord[, 1:2] <- sf::sf_project(from = crs(x), 
                                     to   = CRSobj,
                                     pts  = x@coord[, 1:2])
    
    # xsp <- as(x, "SpatialLines")
    # xsptrsf <- sp::spTransform(xsp, CRSobj)
    # x@coord[, 1:2] <- sp::coordinates(xsptrsf)[[1]][[1]]
    # x@crs <- as.character(CRSobj)
  }
  x@pos <- relTrPos(x)
  return(x)
})




#' DEPRECATED - Plot the trace plotEnvelope
#' 
#' Plot the amplitude envelope of each trace. See \code{envelope}.
#' 
#' @param x An object of the class GPR.
#' @param npad Integer. Padding to avoid Gibbs effect at the begining and
#'             end of the data because of the Hilbert transform.
#' @param FUN DEPRECATED. A function to be applied on each row of the GPR data 
#'            to estimate the wave amplitude as a function of time/depth.
#' @param add A length-one boolean vector. If TRUE the amplitude is plotted
#'            on the previous plot. If FALSE (default) a new plot is created.
#' @param all A length-one boolean vector. If TRUE all trace envelope are 
#'            plotted, if FALSE only the mean.
#' @param plotLog A length-one boolean vector. If TRUE the logarithm of the 
#'              amplitude of every trace is ploted on the estimate amplitude.
#'              Default is FALSE.
#' @examples
#' data(frenkeLine00)
#' plotAmpl(frenkeLine00, FUN = median)
#' @name plotEnvelope
#' @rdname envelope
#' @export
setMethod("plotEnvelope", "GPR", function(x, npad = 100, FUN = mean, add = FALSE, 
                                          all = FALSE, plotLog = TRUE, ...){
  #   op <- par(no.readonly=TRUE)
  stop("Deprecated!\nUse 'trPlot(envelope(x))' or ",
       "'trPlot(traceStat(envelope(x)))' instead.")
} 
)

#' DEPRECATED - Plot the trace amplitude
#' 
#' Plot the amplitude estimated over the whole GPR data as a function of 
#'  time/depth.
#' 
#' @param x An object of the class GPR.
#' @param FUN A function to be applied on each row of the GPR data to 
#'            estimate the wave amplitude as a function of time/depth.
#' @param add A length-one boolean vector. If TRUE the amplitude is plotted
#'            on the previous plot. If FALSE (default) a new plot is created.
#' @param all A length-one boolean vector. If TRUE the logarithm of the 
#'              amplitude of every trace is ploted on the estimate amplitude.
#'              Default is FALSE.
#' processing functions with their arguments applied previously on the
#' GPR data.
#' @examples
#' data(frenkeLine00)
#' plotAmpl(frenkeLine00, FUN = median)
#' @name plotAmpl
#' @rdname plotAmpl
#' @export
setMethod("plotAmpl", "GPR", function(x, npad = 100, FUN = mean, add = FALSE, 
                                      all = FALSE, plotLog = TRUE, ...){
  #   op <- par(no.readonly=TRUE)
  warning("Deprecated!\nUse 'trPlot(envelope(x))' or ",
          "'trPlot(traceStat(envelope(x)))' instead.")
  FUN <- match.fun(FUN)
  trPlot(traceStat(envelope(x), FUN = FUN), ...)
} 
)

#' DEPRECATED - Processing steps applied to the data
#' 
#' DEPRECATED - use \code{proc} instead!
#' \code{processing} returns all the processing steps applied to the data.
#' 
#' @param x An object of the class GPR.
#' @return A character vector whose elements contain the name of the 
#' processing functions with their arguments applied previously on the
#' GPR data.
#' @examples
#' data(frenkeLine00)
#' A <- dewow(frenkeLine00, type = "Gaussian")
#' proc(A)
#' @name processing
#' @rdname processing
#' @export
setMethod("processing", "GPR", function(x){
  stop("DEPRECATED! Use 'proc()' instead!")
  return(x@proc)
} 
)









#--------------- DATA EDITING FUNCTIONS
#' Shift trace vertically
#'
#' Shift traces vertically by an amount of depth (time) units. New traces 
#' are interpolated.
#' 
#' Modified slots
#' \itemize{
#'   \item \code{data}: trace shifted. The number of rows of data may 
#'         be smaller if \code{crop = TRUE}.
#'   \item \code{proc}: updated with function name and arguments.
#' }
#'
#' @param x      [\code{GPR class}] An object of the class \code{GPR}
#' @param ts     [\code{numeric}] Amount of time (or depth, depending on the
#'               trace unit) to shift the traces. 
#'               \code{ts} is eiter a single value (all the traces are shifted by 
#'               the same amount \code{ts}) or a vector with \eqn{m} elements 
#'               (\eqn{m} is equal to the number of traces).
#' @param method [\code{character(1)}] Interpolation method to be applied
#'               (one of \code{pchip} \code{linear}, \code{nearest}, 
#'               \code{spline}, \code{cubic}, \code{none}, 
#'               see also \code{\link[signal]{interp1}}). 
#'                \code{"none"} means that the trace is shifted by the
#'               amount of trace samples the closest to \code{ts} without
#'               interpolation.
#' @param crop   [\code{logical(1)}] 
#'               If \code{TRUE} (default), remove the rows containing only 
#'               zero's (no data).
#'              
#' @return [\code{GPR class}] An object of the class GPR.
#' 
#' @seealso \code{\link{time0Cor}} to shift the traces such that they start
#'          at time-zero.
#' @name traceShift
#' @rdname traceShift
#' @export
setMethod("traceShift", 
          "GPR", 
          function(x, ts, method = c("pchip", "linear", "nearest", "spline", 
                                      "cubic", "none"), crop = TRUE, 
                   track = TRUE){
            
  # method <- match.arg(method, c("spline", "linear", "nearest", "pchip", 
                                # "cubic", "none"))
  method <- method[1]
  if(length(ts) == 1){
    ts <- rep(ts, ncol(x))
  }
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(ts,     msg, "NUMERIC_LEN", c(1, ncol(x)))
  msg <- checkArg(method, msg, "STRING_CHOICE", 
                 c("pchip", "linear", "nearest", "spline", "cubic", "none"))
  msg <- checkArg(crop,   msg, "LOGICAL_LEN", 1)
  checkArgStop(msg)
  #-----------------------------------
  
  # FIXME: check that ts not too large is!!!!
  
  if(any(ts != 0)){
    x <- .traceShift(x, ts = ts, method = method, crop = crop)
    if(isTRUE(track)) proc(x) <- getArgs()
  }else{
    warning("Nothing shifted because all 'ts' values are equal to zero!")
  }

  return(x)
})

# private function
.traceShift <- function(x, ts, method = c("pchip", "linear", "nearest", "spline", 
                                          "cubic", "none"), crop = TRUE){
  x@data <- .traceShiftMat(x@data, ts = ts, tt = x@depth, 
                           dz = x@dz, method = method)
  x@depth <- (seq_len(nrow(x@data)) - 1) * x@dz
  if(crop == TRUE){
    testCrop <- apply(abs(x@data), 1, sum)
    x <- x[!is.na(testCrop), ]
  }
  return(x)
}

#' Interpolate (vertically) trace at regular interval or given position
#' 
#' Interpolate every trace at regular interval or at given positions
#' 
#' Modified slots
#' \itemize{
#'   \item \code{data}: trace interpolated The number of rows of data may 
#'         be smaller if \code{crop = TRUE}.
#'   \item \code{dz}: new value depending on argument \code{z}.
#'   \item \code{depth}: adapted to code{z}.
#'   \item \code{proc}: updated with function name and arguments.
#' }
#'
#' @param x      [\code{GPR class}] An object of the class \code{GPR}
#' @param z      [\code{numeric}] Either an interval (e.g., time interval)
#'               to interpolate the traces at regular interval or a vector
#'               of \eqn{m} elements (\eqn{m} is equal to the number of traces)
#'               Amount of time (or depth, depending on the
#'               trace unit) to shift the traces. 
#'               \code{ts} is eiter a single value (all the traces are shifted by 
#'               the same amount \code{ts}) or a vector with \eqn{m} elements 
#'               (\eqn{m} is equal to the number of traces).
#' @param method [\code{character(1)}] Interpolation method to be applied
#'               (one of \code{pchip} \code{linear}, \code{nearest}, 
#'               \code{spline}, \code{cubic}, \code{none}, 
#'               see also \code{\link[signal]{interp1}}). 
#'                \code{"none"} means that the trace is shifted by the
#'               amount of trace samples the closest to \code{ts} without
#'               interpolation.
#' @param crop   [\code{logical(1)}] 
#'               If \code{TRUE} (default), remove the rows containing only 
#'               zero's (no data).
#'              
#' @return [\code{GPR class}] An object of the class GPR.
#' 
#' @name interpTrace
#' @rdname interpTrace
#' @export
setMethod("interpTrace", 
          "GPR", 
          function(x, z, method = c("pchip", "linear", "nearest", "spline", 
                                     "cubic"), crop = TRUE, track = TRUE){
            
  method <- method[1]
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(z,     msg, "NUMERIC")
  if(length(z) == 1) msg <- checkArg(z,     msg, "NUMERIC1_SPOS", Inf)
  msg <- checkArg(method, msg, "STRING_CHOICE", 
                  c("pchip", "linear", "nearest", "spline", "cubic"))
  msg <- checkArg(crop,   msg, "LOGICAL_LEN", 1)
  checkArgStop(msg)
  #-----------------------------------  
  extrap <- TRUE
  if(crop) extrap = FALSE
  
  x <- .traceInterpReg(x = x, z = z, method = method, extrap = extrap)
  
  if(crop == TRUE){
    testCrop <- apply(abs(x@data), 1, sum)
    x <- x[!is.na(testCrop), ]
  }
  
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
})


# interpolation at regular interval if x@dz is not unique!!
.traceInterpReg <- function(x, z, method = c("pchip", "linear", "nearest", 
                                             "spline", "cubic"), extrap = TRUE){
  method <- method[1]
  if(length(z) == 1){
    x@dz <- z
    zreg <- seq(from = min(x@depth), to = tail(x@depth, 1), by = x@dz)
  }else if(length(z) > 1){
    zreg <- z
    x@dz <- mean(diff(z))
  }
  funInterp <- function(x, z, zreg){
    signal::interp1(x = z, y = x, xi = zreg, 
                    method = method, extrap = extrap)
  }
  x@data <- apply(x@data, 2, funInterp, 
                  z = x@depth, zreg = zreg)
  x@depth <- zreg
  return(x)
}


#' Time zero correction
#'
#' \code{time0Cor} shift the traces vertically such that they start at
#' time zero (time zero of the data can be modified with the function).
#' New traces are interpolated.
#'
#' This function is a wrapper for the following commands
#' \itemize{
#'   \item \code{ts <- -t0 + keep} (or if \code{t0} is \code{NULL}, 
#'         \code{ts <- -time0(x) + keep})
#'   \item \code{x <- traceShift( x,  ts, method = method, crop = crop)}
#'   \item \code{time0(x) <- time0(x) + ts}
#' }
#' 
#' Modified slots
#' \itemize{
#'   \item \code{data}: trace shifted. The number of rows of data may 
#'         be smaller if \code{crop = TRUE}.
#'   \item \code{time0}: set to 0.
#'   \item \code{proc}: updated with function name and arguments.
#' }
#'  
#' @param x      [\code{GPR class}] An object of the class \code{GPR}
#' @param t0     [\code{DEPRECATED}] DEPRECATED - NO MORE USED.
#'               Instead, set time-zero with either 
#'               \code{time0(x) <- ...} or 
#'               \code{x <- setTime0(x, ...)}.
#' @param method [\code{character(1)}] Interpolation method to be applied
#'               (one of \code{pchip} \code{linear}, \code{nearest}, 
#'               \code{spline}, \code{cubic}, \code{none}, 
#'               see also \code{\link[signal]{interp1}}). 
#'                \code{"none"} means that the trace is shifted by the
#'               amount of trace samples the closest to \code{ts} without
#'               interpolation.
#' @param keep   [\code{DEPRECATED}] DEPRECATED - NO MORE USED.
#' @param crop   [\code{logical(1)}] 
#'               If \code{TRUE} (default), remove the rows containing only 
#'               zero's (no data).
#'               
#' @return [\code{GPR class}] An object of the class \code{GPR}
#' 
#' @examples
#' data(frenkeLine00)
#' tfb <- firstBreak(frenkeLine00)
#' t0 <- firstBreakToTime0(tfb, frenkeLine00, c0 = 0.299)
#' time0(frenkeLine00) <- t0
#' frenkeLine00_2 <- time0Cor(frenkeLine00, method = "pchip")
#' 
#' @seealso \code{\link{firstBreak}} to estimate the first wave break;
#'          \code{\link{firstBreakToTime0}} to convert the first wave break
#'          into time zero.
#'          \code{\link{time0}} and \code{\link{setTime0}} to set time-zero;
#'          \code{\link{traceShift}} to shift the traces

#' @name time0Cor
#' @rdname time0Cor
#' @export
setMethod("time0Cor", "GPR", function(x, t0 = NULL, 
                                      method = c("pchip", "linear", "nearest", 
                                                 "spline", "cubic", "none"), 
                                      crop = TRUE, keep = 0, track = TRUE){
  method <- method[1]
  if(!is.null(t0)){
    warning("'t0' is no more used. Set first time-zero with\n",
            "'time0(x) <- t0' or",
            "'x <- setTime0(x, t0)'.")
  }
  if(keep != 0){
    warning("'keep' is no more used.")
  }
  
  #------------------- check arguments
  msg <- checkArgInit()
  #msg <- checkArg(t0,     msg, "NUMERIC_LEN", c(1, ncol(x)))
  msg <- checkArg(method, msg, "STRING_CHOICE", 
                 c("pchip", "linear", "nearest", "spline", "cubic", "none"))
  msg <- checkArg(crop,   msg, "LOGICAL_LEN", 1)
  checkArgStop(msg)
  #-----------------------------------
  
  # ts <- -x@time0 + keep
  
  # if(is.null(t0)){
  #   ts <- -x@time0 + keep
  # }else{
  #   if(any(is.na(t0))){
  #     stop("Woops, time zero selected have NA values. \n",
  #          "This is not acceptable, time zero must have a numeric value in ",
  #          "order to be corrected. \n",
  #          "If this is because first break could not be picked, \n",
  #          "you might want to consider removing these traces from your ",
  #          "radargram.")
  #   }else{
  #     if(length(t0) == 1){
  #       t0 <- rep(t0, length(x@time0))
  #     }
  #     ts <- -t0 + keep
  #   }
  # }
  ts <- -x@time0 
  if(any(ts != 0)){
    x <- .traceShift(x, ts = ts, method = method, crop = crop)
    x@time0 <- x@time0 + ts
    if(isTRUE(track)) proc(x) <- getArgs()
    return(x)
  }else{
    warning("Nothing shifted because all 'ts' values are equal to zero!")
    return(x)
  }
  # if(crop == TRUE){
  #   testCrop <- apply(abs(x@data), 1, sum)
  #   x <- x[!is.na(testCrop), ]
  # }
  # if( any(ts != 0) ){
  #   x <- traceShift( x,  ts, method = method, crop = TRUE)
  #   x@time0 <- x@time0 + ts
  #   proc(x) <- getArgs()
  # }else{
  #   warning("Nothing shifted because all 't0' or 'time0(x)' values ",
  #           "as well as all 'keep' values are ",
  #           "equal to zero!")
  # }
  # proc(x) <- getArgs()
  # return(x)
})

#' Correct for the elevation of antenna above the ground
#' 
#' For air-lauched GPR or airborn GPR
#' @name corAntElev
#' @rdname corAntElev
#' @export
setMethod("corAntElev", "GPR", function(x, c0 = 0.3){
  ant_z <- max(x@coord[,3]) - x@coord[,3]
  ts <- ant_z * 2 / c0
  x <- traceShift(x, ts, crop = FALSE)
  proc(x) <- getArgs()
  return(x)
})

#' Constant-offset correction (time) of the GPR data
#'
#' \code{timeCorOffset} applies a time correction to every traces 
#' to compensate the offset between transmitter 
#' and receiver antennae (it converts the trace time of the data acquired with
#' a bistatic antenna system into trace time data virtually acquiered with 
#' a monostatic system under the assumption of horizontally layered structure).
#' If all the traces have the same time-zero, this function does not change the 
#' trace but only the time (time scale). If the traces have different
#' time-zero, the traces are first aligned to have the same time-zero 
#' (spline interpolation)
#' 
#' Modified slots
#' \itemize{
#'   \item \code{data}: trace shifted to time-zeor. The number of rows of data 
#'         may be smaller.
#'   \item \code{time0}: set to 0.
#'   \item \code{depth}: adapted to a virtual monostatic system. 
#'         FIXME: not regularly spaced!
#'   \item \code{antsep}: set to 0.
#'   \item \code{proc}: updated with function name and arguments.
#' }
#' 
#' @param x  [\code{GPR class}] An object of the class \code{GPR}
#' @param t0 [\code{DEPRECATED}] DEPRECATED - NO MORE USED.
#'           Instead, set time-zero with either 
#'           \code{time0(x) <- ...} or 
#'           \code{x <- setTime0(x, ...)}.
#'               
#' @return [\code{GPR class}] An object of the class \code{GPR}
#'              
#' @seealso \code{\link{time0}} to set time zero and 
#'          \code{\link{firstBreakToTime0}} to convert the first wave break
#'          into time zero.
#' @name timeCorOffset
#' @rdname timeCorOffset
#' @export
setMethod("timeCorOffset", "GPR", function(x, t0 = NULL, track = TRUE){
  
  if(!is.null(t0)){
    warning("'t0' is no more used. Set first time-zero with\n",
            "'time0(x) <- t0' or",
            "'x <- setTime0(x, t0)'.")
  }
  
  #------------------- check if slots are empty
  msg <- checkSlotInit()
  msg <- checkSlotEmpty(x, "antsep", msg)
  msg <- checkSlotEmpty(x, "vel", msg)
  checkSlotStop(msg)
  #--------------------------------------------
  
  # if( isSlotEmpty(x, "antsep") ){
  #   msg <- c(msg, "@antsep: Antenna separation must be first defined ",
  #          "with `antsep(x)<-...`!")
  # }
  # if( isSlotEmpty(x, "vel") ){
  #   msg <- c(msg, "@vel: wave velocity must be first defined ",
  #        "with `vel(x)<-...`!")
  # }
  # if(length(msg) > 0 )

  # if(!is.null(t0)){
  #   time0(x) <- t0
  # }
  
  # x <- time0Cor(x, method = "spline")
  ts <- -x@time0 
  if(any(ts != 0)){
    x <- .traceShift(x, ts = ts, method = "spline", crop = TRUE)
    x@time0 <- x@time0 + ts
  }
  
  dz <- mean(diff(x@depth))
  # !!! pb if negative values in sqrt()!!!!
  tcor2 <- x@depth^2 - (x@antsep/x@vel[[1]])^2
  test <- tcor2 >= 0
  x <- x[test]
  x@depth <- sqrt(tcor2[test])
  # x@time0 is already = 0
  x@antsep <- 0
  
  x <- .traceInterpReg(x, z = dz)
  # x@proc <- x@proc[-length(x@proc)] # remove proc from traceShift()
  # x@proc <- c(x@proc, "timeCorOffset")
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
  #----------------------------------------------------------------------------#
  # if(is.null(t0)){
  #   tol <- sqrt(.Machine$double.eps)
  #   # all not equal
  #   if(abs(max(x@time0) - min(x@time0)) > tol){
  #     x <- time0Cor(x, method = "spline")
  #   }
  #   t0 <- mean(x@time0)
  # }else{
  #   if(length(t0) > 1){
  #     t0 <- mean(t0)
  #     warning("'length(t0)' should be equal to 1! I take the mean of 't0'!")
  #   }
  # }
  # x <- x[floor(t0/x@dz):nrow(x),]
  # tcor2 <- (x@depth - t0)^2 - (x@antsep/x@vel[[1]])^2
  # #tcor2 <- (x@depth - mean(x@time0) + x@antsep/0.299)^2 - 
  # #                (x@antsep/x@vel[[1]])^2
  # x <- x[tcor2 > 0,]
  # tcor <- sqrt( tcor2[tcor2 > 0] )
  # x@depth <- tcor
  # x@time0 <- rep(0, ncol(x))
  # x@proc <- x@proc[-length(x@proc)] # remove proc from traceShift()
  # x@proc <- c(x@proc, "timeCorOffset")
  # return(x)
})


#----------------- DEWOW
#' Trace dewowing
#' 
#' \code{dewow} remove the low-frequency component (the so-called 'wow') of 
#' every traces.
#' 
#' The low-frequency component is computed by different methods:
#' \itemize{
#'   \item \code{runmed} running median based on \code{\link[stats]{runmed}}
#'   \item \code{runmean} running mean based on \code{\link[stats]{filter}}
#'   \item \code{MAD} DEPRECATED - Median Absolute Deviation filter
#'   \item \code{Gaussian} Gaussian smoothing applied to the trace samples
#'         after time-zero based on \code{\link[mmand]{gaussianSmooth}}
#' }
#' 
#' Modified slots
#' \itemize{
#'   \item \code{data}: trace dewowed.
#'   \item \code{proc}: updated with function name and arguments.
#' }
#' 
#' @param x    [\code{GPR class}] An object of the class GPR.
#' @param type [\code{character(1)}] Dewow method,
#'             one of \code{runmed} (running median),
#'             \code{runmean} (running mean), 
#'             \code{MAD} (DEPRECATED Median Absolute Deviation), 
#'             \code{Gaussian} (Gaussian smoothing).
#' @param w    [\code{numeric(1)}] If \code{type} = \code{runmed}, 
#'             \code{MAD} or \code{runmean}, window length of the filter in
#'             trace unit;
#'             If \code{type} = \code{Gaussian}, standard deviation in trace
#'             unit.
#'             If \code{w = NULL}, \code{w} is estimated as five times the 
#'             wavelength corresponding to the maximum frequency of x 
#'             (estimated with \code{\link{spec}})
#'             
#' @return [\code{GPR class}] An object of the class GPR whose traces are dewowed.
#' @examples
#' data(frenkeLine00)
#' A <- dewow(frenkeLine00, type = "Gaussian")
#' A
#' @name dewow
#' @rdname dewow
#' @export
setMethod("dewow", "GPR", function(x, type = c("runmed", "runmean", 
                                               "mad", "gaussian"), 
                                   w = NULL, track = TRUE){
  # type <- match.arg(type, c("MAD", "Gaussian"))
  type <- tolower(type[1])
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(type, msg, "STRING_CHOICE", c("runmed", "runmean",
                                                "mad", "gaussian"))
  msg <- checkArg(w,    msg, "NUMERIC1_SPOS_NULL", Inf)
  checkArgStop(msg)
  #-----------------------------------
  
  if(is.null(w)){
    # argument initialization
    # pulse width in ns, (x@freq is in MHz)
    a <- RGPR::spec(x, plotSpec = FALSE, unwrapPhase = FALSE)
    freq <- a$freq[which.max(rowMeans(a$pow))]
    # pw <- 1/(x@freq * 10^6)/10^-9
    pw <- 1/(freq * 10^6)/10^-9
    w <- round((5 * pw)/x@dz)
  }else{
    w <- round(w / x@dz)
  }
  if(type == "mad"){  
    warning("Soon deprecated. Use instead:\n",
            "dewow(x, type = 'runmed', w = 2*w)")
    x@data <- x@data - .runmmmMat(x@data, 2*w+1, type = "runmed")
  }else if(type == "runmed"){
    x@data <- x@data - .runmmmMat(x@data, w, type = "runmed")
  }else if(type == "runmean"){
    x@data <- x@data - .runmmmMat(x@data, w, type = "runmean")
  }else if(type == "gaussian"){
    xdata <- x@data
    xDepth <- matrix(x@depth, byrow = FALSE, nrow = nrow(x), ncol = ncol(x))
    xTime0 <- matrix(x@time0, byrow = TRUE, nrow = nrow(x), ncol = ncol(x))
    test <- xDepth <= xTime0
    # before_t0 <- x@depth <= mean(x@time0)
    xdata[test] <- 0
    x@data[!test] <- x@data[!test] - mmand::gaussianSmooth(xdata, w)[!test]
  }
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x) 
})





#----------------- 1D-FILTER
#' One dimensional filters
#' 
#' @name filter1D
#' @rdname filter1D
#' @export
setMethod("filter1D", 
          "GPR", 
          function(x, 
                   type = c("runmed", "runmean", "mad", "gaussian", "hampel"), 
                   w = NULL, track = TRUE){
  # type <- match.arg(type, c("MAD", "Gaussian"))
  type <- tolower(type[1])
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(type, msg, "STRING_CHOICE", c("runmed", "runmean",
                                                "mad", "gaussian", "hampel"))
  msg <- checkArg(w,    msg, "NUMERIC1_SPOS_NULL", Inf)
  checkArgStop(msg)
  #-----------------------------------
  
  if(is.null(w)) w <- 10 * x@dz
  w <- round(w / x@dz)
  
  if(type == "mad"){  
    warning("Soon deprecated. Use instead:\n",
            "filter1D(x, type = 'Hampel', w = ...)")
    # A <- x@data
    # if(length(dim(A)) < 2){
    #   A <- matrix(A,ncol=1,nrow=length(A))
    # }
    # X <- rbind(matrix(0,ncol=ncol(A),nrow=w), A, matrix(0,ncol=ncol(A),nrow=w))
    # n <- nrow(X)
    # Y <- X
    # for (i in (w + 1):(n - w)) {
    #   Y[i,] <-  apply( X[(i - w):(i + w),, drop=FALSE], 2, median)
    #   # x0 = 0.1  # argument initialization
    #   # S0 <- 1.4826 * apply( abs(X[(i - w):(i + w),,drop=FALSE] - Xmed),
    #   #  2, median)
    #   # test <- abs(X[i,] - Xmed) > x0 * S0
    #   # Y[i,test] <- Xmed[test]      
    # }
    # x@data <- A - Y[(w+1):(n-w),]
    x@data <- .runmmmMat(x@data, 2*w, type = "runmed")
  }else if(type == "runmed"){
    x@data <- .runmmmMat(x@data, w, type = "runmed")
  }else if(type == "runmean"){
    x@data <- .runmmmMat(x@data, w, type = "runmean")
  }else if(type == "gaussian"){
    # t0 <- round(mean(x@time0)/x@dz)
    # xdata <- x@data
    # xDepth <- matrix(x@depth, byrow = FALSE, nrow = nrow(x), ncol = ncol(x))
    # xTime0 <- matrix(x@time0, byrow = TRUE, nrow = nrow(x), ncol = ncol(x))
    # test <- xDepth <= xTime0
    # before_t0 <- x@depth <= mean(x@time0)
    # xdata[before_t0,] <- 0
    # # if(length(dim(A))<2){
    # #   A <- matrix(A, ncol = 1, nrow = length(A))
    # # }
    # x@data[!before_t0,] <- xdata[!before_t0,] - 
    #   mmand::gaussianSmooth(xdata, w)[!before_t0,]
    xdata <- x@data
    xDepth <- matrix(x@depth, byrow = FALSE, nrow = nrow(x), ncol = ncol(x))
    xTime0 <- matrix(x@time0, byrow = TRUE, nrow = nrow(x), ncol = ncol(x))
    test <- xDepth <= xTime0
    # before_t0 <- x@depth <= mean(x@time0)
    xdata[test] <- 0
    x@data[!test] <- x@data[!test] - mmand::gaussianSmooth(xdata, w)[!test]
  }else if(type == "hampel"){
    x@data <- .runmmmMat(x@data, w, type = "hampel")
  }
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
} 
)

#----------------- SPATIAL-FILTER
#setMethod("adimproSmooth", "GPR", function(x,hmax=2,...){
#     IMG <- x@data
#     IMG <- (IMG-min(IMG))/(max(IMG)-min(IMG))
#     adimg <- make.image(IMG)
#     # img.smooth <- adimpro::awsimage(adimg, hmax = 2)
#     # img.smooth <- adimpro::awsimage(adimg, hmax = 2)
#     # img.smooth <- adimpro::awsaniso(adimg, hmax = 2,...)
#     img.smooth <- adimpro::awspimage(adimg, hmax = 2,...)
#     AA <- extract.image(img.smooth)
#     AAA <- (AA-mean(AA))/sd(AA)
#     x@data <- AAA
#     proc <- getArgs()
#    x@proc <- c(x@proc, proc)
#     return(x)
#  }
#)


#----------------- CLIP/GAMMA/NORMALIZE



#' Trace scaling
#'
#' @name traceScaling
#' @rdname traceScaling
#' @export
setMethod("traceScaling", 
          "GPR", 
          function(x,type = c("stat", "min-max", "95", "eq", 
                              "sum", "rms", "mad", "invNormal"),
                   track = TRUE){
  x@data <- scaleCol(x@data, type = type)
  if(isTRUE(track)) proc(x) <- getArgs()
  #   x@proc <- c(x@proc, proc)
  return(x)
}
)

#' Trace statistics
#'
#' \code{traceStat} is a generic function used to produce results defined
#' by an user function. The user function is applied accross traces (horizontal) 
#' using a moving window. Note that if the moving window length is not defined, 
#' all traces are averaged into one single trace (the results is similar to
#' \code{apply(x, 1, FUN, ...)}.
#' 
#' @param x An object of the class GPR
#' @param w A length-one integer vector equal to the window length of the 
#'          average window. If \code{w = NULL} a single trace corresponding to
#'          the average trace of the whole profile is returned.
#' @param FUN A function to compute the average (default is \code{mean})
#' @param ... Additional parameters for the FUN functions
#' @return An object of the class GPR. When \code{w = NULL}, this function 
#'         returns a GPR object with a single trace corresponding to the 
#'         average trace of the whole radargram. When \code{w} is equal to a
#'         strictly positive interger this function returns a GPR object with
#'         a size identical to x where each trace corresponds to the average
#'         of the \code{w} neighbouring traces centered on the considered trace.
#' @examples
#' data("frenkeLine00")
#' 
#' f0 <- frenkeLine00
#' 
#' f1 <- traceStat(f0)
#' plot(f1)
#' # substract the average trace
#' plot(f0 - f1)
#' 
#' f2 <- traceStat(f0, w = 20)
#' plot(f2)
#' plot(f0 - f2)
#' 
#' f3 <- traceStat(f0, w = 20, FUN = median)
#' plot(f3)
#' plot(f0 - f3)
#' @name traceStat
#' @rdname traceStat
#' @export
setMethod("traceStat", "GPR", function(x, w = NULL, FUN = mean, ...,
                                       track = TRUE){
  FUN <- match.fun(FUN)
  if(is.null(w)){
    xdata <- x@data
    x <- x[,1]
    x@data <- as.matrix(apply(xdata, 1, FUN, ...))
    x@time0 <- mean(x@time0)
    x@time <- mean(x@time)
    x@coord <- matrix(ncol = 0, nrow = 0)
    x@rec <- matrix(ncol = 0, nrow = 0)
    x@trans <- matrix(ncol = 0, nrow = 0)
  }else{
    x@data <- wapplyMat(x@data, width = w, by = 1, FUN = FUN, MARGIN = 1, ...)
  }
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
}
)

#' Trace average (DEPRECATED, use 'traceStat' instead)
#'
#' Average traces in a radargram along the distance (horizontal) axis using
#' a moving window. This can be used to increase signal to noise ratio. Note that if 
#' the moving window length is not defined, all traces are averaged into one single trace.
#' 
#' @param x An object of the class GPR
#' @param w A length-one integer vector equal to the window length of the 
#'          average window. If \code{w = NULL} a single trace corresponding to
#'          the average trace of the whole profile is returned.
#' @param FUN A function to compute the average (default is \code{mean})
#' @param ... Additional parameters for the FUN functions
#' @return An object of the class GPR. When \code{w = NULL}, this function 
#'         returns a GPR object with a single trace corresponding to the 
#'         average trace of the whole radargram. When \code{w} is equal to a
#'         strictly positive interger this function returns a GPR object with
#'         a size identical to x where each trace corresponds to the average
#'         of the \code{w} neighbouring traces centered on the considered trace.
#' @examples
#' data("frenkeLine00")
#' 
#' f0 <- frenkeLine00
#' 
#' f1 <- traceAverage(f0)
#' plot(f1)
#' # substract the average trace
#' plot(f0 - f1)
#' 
#' f2 <- traceAverage(f0, w = 20)
#' plot(f2)
#' plot(f0 - f2)
#' 
#' f3 <- traceAverage(f0, w = 20, FUN = median)
#' plot(f3)
#' plot(f0 - f3)
#' @name traceAverage
#' @rdname traceAverage
#' @export
setMethod("traceAverage", "GPR", function(x, w = NULL, FUN = mean, ...,
                                          track = TRUE){
  warning("Deprecated!\n Use 'traceStat()' instead. Check the help.")
  FUN <- match.fun(FUN)
  if(is.null(w)){
    xdata <- x@data
    x <- x[,1]
    x@data <- as.matrix(apply(xdata, 1, FUN, ...))
    x@time0 <- mean(x@time0)
    x@time <- mean(x@time)
    x@coord <- matrix(ncol = 0, nrow = 0)
    x@rec <- matrix(ncol = 0, nrow = 0)
    x@trans <- matrix(ncol = 0, nrow = 0)
  }else{
    x@data <- wapplyMat(x@data, width = w, by = 1, FUN = FUN, MARGIN = 1, ...)
  }
  # funName <- getFunName(FUN)
  # proc(x) <- getArgs( addArgs = c('FUN' = funName))
  if(isTRUE(track)) proc(x) <- getArgs()
  #   x@proc <- c(x@proc, proc)
  return(x)
}
)



#----------------- FREQUENCY FILTERS
#' Frequency filter
#' 
#' The frequency filter alters the signal amplitude with respect to frequency.
#' \describe{
#'   \item{Low-pass filter}{low frequencies are passed, high frequencies are attenuated.}
#'   \item{High-pass filter}{high frequencies are passed, low frequencies are attenuated.}
#'   \item{Band-pass filter}{only frequencies in a frequency band are passed.}
#'   \item{Band-pass-reject filter}{a normally narrow band of frequencies is attenuated.}
#' }
#'
#' For the low- and high-pass filter, only one cut-off frequency can be defined
#' while the argument \code{L} will define the filter length of the Hamming
#' window (necessary to reduce ringing artifacts from the Gibbs phenomenon). 
#' If two values are passed to the cut-off frequency argument \code{f}, 
#' the value of \code{L} will be ignored.
#' Example for low-pass filter: \code{f = c(150, 200)}.
#' Example for high-pass filter: \code{f = c(10, 20)}
#' 
#' For the band-pass filter and the band-pass-reject filter, 
#' only two cut-off frequency can be defined
#' while the argument \code{L} will define the filter length of the Hamming
#' window (necessary to reduce ringing artifacts from the Gibbs phenomenon). 
#' If four values (the two first corner frequencies followed by the two last
#' corner frequencies ) are passed to the cut-off frequency argument \code{f}, 
#' the value of \code{L} will be ignored. 
#' Example: \code{f = c(10, 20, 150, 200)}
#' 
#' Check this free book: The Scientist and Engineer's Guide to Digital Signal 
#' Processing By Steven W. Smith, Ph.D.
#'
#' @param x An object of the class GPR
#' @param f numeric vector: cut-off frequencies. Cutoff frequency is the 
#'          frequency beyond which the filter will not pass signals.
#'           See Details.
#' @param type length-one character vector: type of frequency vector. \code{low}
#'             for low-pass filter, \code{high} for high-pass filter and 
#'             \code{bandpass} for bandpass filter.
#' @param L length-one numeric defining the filter length. See Details.
#' @param plotSpec boolean. If \code{TRUE} plot the frequency spectrum as well.
#' @name fFilter
#' @rdname fFilter
#' @export
setMethod("fFilter", "GPR", 
          function(x, f = 100, 
                   type = c('low', 'high', 'bandpass', 'bandpass-reject'),
                   L = 257, plotSpec = FALSE, track = TRUE){
  x@data <- .fFilter1D(x@data, f = f,  type = type, L = L, dT = x@dz, 
                       plotSpec = plotSpec)
  if(isTRUE(track)) proc(x) <- getArgs()
  #   x@proc <- c(x@proc, proc)
  return(x)
} 
)

#' Frequency-wavenumber filter
#'
#' @name fkFilter
#' @rdname fkFilter
#' @export
setMethod("fkFilter", "GPR", function(x, fk = NULL, L = c(5 , 5), npad = 1,
                                      track = TRUE){
  if(is.null(fk)) stop("fk argument has to be specified")
  # if polygon
  if(is.list(fk) && length(fk) == 2){
    areaunclosed <- t(do.call("rbind", fk))
    nk <- npad*(nextpower2(ncol(x@data)))
    nf <- npad*(nextpower2(nrow(x@data)))
    # frequency
    Ts = x@dz*10^(-9)    # [s] Sample time
    fac = 1000000
    fre = (1/Ts)*seq(0,nf/2)/nf/fac
    
    # wavenumber
    Ks <- 1/x@dx      # [1/m] Sampling frequency
    knu <- 1:(nk/2)/(2*(nk/2)) * Ks  #[1/m]
    knutot <- c(-rev(knu),knu)
    fk <- outer(fre, knutot, inPoly,
                vertx = areaunclosed[,2],
                verty = areaunclosed[,1])
  }else if(!is.matrix(fk)){
    stop("fk should be either of type 'list' or 'matrix'\n")
  }else if(is.matrix(fk)){
    cat("# FIXME! function to transform matrix into polygon\n")
  }
  x@data <- .FKFilter(x@data, fk = fk, L = L, npad = npad)
  if(isTRUE(track)) proc(x) <- getArgs()
  #   x@proc <- c(x@proc, proc)
  return(x)
  
} 
)


#--------------- EIGENIMAGE RECONSTRUCTION

#' Eigenimage filter
#'
#' Decompose the GPR data (radargram) using singular value decomposition (SVD) 
#' and return the reconstructed data for the selected singular values 
#' (eigenvalues). This method is sometimes refered to in the litterature as 
#' the Karhunen-Loeve (KL) transformation or the eigenimage decomposition.
#'
#' @param x           An object of the class GPR
#' @param eigenvalue  An integer vector specifying the eigenvalues selected. 
#'                    \code{eigenvalue = 1:5} returns the image reconstructed 
#'                    using the first five eigenvalues while \code{c(2,4,6)}
#'                    will return the image reconstructed for these three 
#'                    specific eigenvalues. 
#'                    If \code{NA}, a plot of the eigenvalues spectrum 
#'                    will be displayed and the user will be prompted to enter 
#'                    the pair of selected eigenvalues separated by a comma. 
#'                    That is for example \code{1,2} will return the image 
#'                    reconstructred using tthe first two eigenvalues. 
#'                    If \code{NULL}, the image is reconstructed using 
#'                    all eigenvalues.
#'                    The number of eigenvalue is equal to \code{min(dim(x))}.
#' @param center      Logical or numeric. If \code{TRUE}, centering is done by 
#'                    subtracting the layer means (omitting \code{NA}'s), and 
#'                    if \code{FALSE}, no centering is done. If center is a 
#'                    numeric vector with length equal to the 
#'                    \code{nlayers(x)}, then each layer of \code{x} has the 
#'                    corresponding value from center subtracted from it.
#' @param scale       Logical or numeric. If \code{TRUE}, scaling is done by 
#'                    dividing the (centered) layers of \code{x} by their 
#'                    standard deviations if center is \code{TRUE}, and the 
#'                    root mean square otherwise. 
#'                    If scale is \code{FALSE}, no scaling is done. If scale is
#'                    a numeric vector with length equal to \code{nlayers(x)}, 
#'                    each layer of \code{x} is divided by the corresponding 
#'                    value. Scaling is done after centering.
#'
#' @return An object of the class GPR.
#' 
#' @references
#' \itemize{
#'   \item{Textbook: Sacchi (2002) Statistical and Transform Methods
#'         in Geophysical Signal Processing}
#'         }
#'         
#' @examples  
#' data(frenkeLine00)
#' x <- frenkeLine00
#' x1 <- eigenFilter(x, eigenvalue = c(1,3))
#' plot(x)
#' plot(x1)
#'
#' @name eigenFilter
#' @rdname eigenFilter
#' @export
setMethod("eigenFilter", "GPR", function(x, eigenvalue = NA, center = TRUE, 
                                         scale = FALSE, track = TRUE){
  
  ev <- unique(eigenvalue)
  X <- scale(x@data, center = center, scale = scale)
  Xsvd <- svd(X)
  lambda <- Xsvd$d^2
  
  if(any(is.na(ev))){
    plot(seq_along(lambda), lambda, type = "b", col = "blue", pch = 16,
         xlab = "Eigenvalue Index", ylab = "Eigenvalue", 
         main = paste0(length(lambda), " eigenvalues"))
    ev <- readline(paste0("What eigenvalues do you want to use to reconstruct ",
                          "the radargram ?"))
    if(grepl(":", ev)){
      ev <- as.integer(eval(parse(text = ev)))
    }else{
      ev <- as.integer(unlist(strsplit(ev, split = ",")))
    }
    if(!is.integer(ev) || is.na(ev)){
      stop("Select the eigenvalues by typing (for example)\n",
           "either '2:12' or '1,2,3,4' or '10'.")
    }
  }
  
  if(is.null(ev)){
    ev <- seq_len(min(dim(X)))
  }else if(!any(is.na(ev))){
    if(max(ev) > min(dim(X))){
      stop("The number of eigenvalues selected cannot exceed ",
           "'min(dim(x))'.")
    }
    if(min(ev) < 0){
      stop("The eigenvalues number must be strictly positive.")
    }
  }
  
  
  d <- numeric(length(Xsvd$d))
  d[ev] <- Xsvd$d[ev]
  Xnew <- Xsvd$u %*% diag(d) %*% t(Xsvd$v)
  
  # Xeigen <- array(NA, dim = c(dim(Xsvd$u), length(ev)))
  # for(i in seq_along(ev)){
  #   Xeigen[,,i] <- Xsvd$d[ev[i]] * Xsvd$u[,ev[i]] %*% t(Xsvd$v[,ev[i]])
  # }
  # 
  # if(length(ev)>1){
  #   Xnew <- rowSums(Xeigen, dims=2)
  # } else{
  #   Xnew <- Xeigen[,,1]
  # }
  
  
  # if(!is.null(attr(X, 'scaled:scale')) && !is.null(attr(X, 'scaled:center'))){
  #   Xnew <- t(apply(Xnew, 1, function(r) r * attr(X, 'scaled:scale') + 
  #                     attr(X, 'scaled:center')))
  # }else if(is.null(attr(X, 'scaled:scale')) && 
  #          !is.null(attr(X, 'scaled:center'))){
  #   Xnew <- t(apply(Xnew , 1, function(r) r + attr(X, 'scaled:center')))
  # }else if(!is.null(attr(X, 'scaled:scale')) && 
  #          is.null(attr(X, 'scaled:center'))){
  #   Xnew <- t(apply(Xnew , 1, function(r) r * attr(X, 'scaled:scale')))
  # }
  
  x@data <- unscale(X, Xnew)
  
  # if(is.null(eigenvalue)){
  #   proc(x) <- getArgs(addArgs = list('eigenvalue' = eigenvalue, 
  #                                     'center' = as.character(center), 
  #                                     'scale' = as.character(scale)))
  # } else{
  #   proc(x) <- getArgs(addArgs = list('eigenvalue' = ev, 
  #                                     'center' = as.character(center), 
  #                                     'scale' = as.character(scale)))
  # }
  if(isTRUE(track)) proc(x) <- getArgs()
  
  return(x)
} 
)




#--------------- CONVOLUTION/DECONVOLUTION


#' Phase rotation
#'
#' Rotate the phase of the GPR data by a given angle \code{phi}.
#' @param x A GPR data
#' @param phi A length-one numeric vector defining the phase rotation in radian.
#' @return The GPR data with rotated phase.
#' @name rotatePhase
#' @rdname rotatePhase
#' @export
setMethod("rotatePhase", "GPR", function(x, phi, track = TRUE){
  x@data <- apply(x@data, 2, phaseRotation, phi)
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
}
)







#-------------------------------------------#
#---------------- SETMETHOD ----------------#
# Print methods
# setMethod("print", "GPR", function(x) print.GPR(x))
# > 1. helper function:
.GPR.print   <-  function(x, digits=5){
  topaste <- c(paste("***","Class GPR", "***\n"))
  topaste <- c(topaste,   paste0("name        = ", x@name, "\n"))
  if(length(x@filepath) > 0){
    topaste <- c(topaste, paste0("filepath    = ", x@filepath, "\n"))
  }
  nbfid <- sum(trimStr(x@fid)!= "")
  if(nbfid > 0){
    topaste <- c(topaste, paste0(nbfid, " fiducial(s)\n"))
  }
  if(length(x@description) > 0){
    topaste <- c(topaste, paste0("description = ", x@description, "\n"))
  }
  if(length(x@date) > 0){
    topaste <- c(topaste, paste0("survey date = ", x@date,"\n"))
  }
  topaste <- c(topaste, paste0(x@surveymode,", ",x@freq, " MHz, ", 
                               "Window length = ",(nrow(x@data)-1)*x@dz, " ", x@depthunit,
                               ", dz = ",x@dz, " ", x@depthunit, "\n"))
  topaste <- c(topaste, paste0(ncol(x@data), " traces, ", 
                               diff(range(x@pos))," ",x@posunit,"\n"))
  if(length(x@proc)>0){
    topaste <- c(topaste, paste("> PROCESSING\n"))
    for(i in seq_along(x@proc)){
      topaste <- c(topaste, paste0("  ",i,". ", x@proc[i],"\n"))
    }      
  }
  topaste <- c(topaste, paste("****************\n"))
  return(topaste)      
}    

# #' @rdname show
#' Print GPR
#'
#' @method print GPR 
#' @name print
#' @rdname show
# > 2. S3 function:
print.GPR <- function(x, ...){
  jj <- .GPR.print(x, ...)
  cat(jj)
  return(invisible(jj))
}
# > 3. And finally a call to setMethod():
# #' @rdname show
#' Show some information on the GPR object
#'
#' Identical to print().
#' @name show
#' @aliases show-method
setMethod("show", "GPR", function(object){
  print.GPR(object)
})   

#' Add a GPR trace on a plot
#'
#' @method lines GPR 
#' @name lines
#' @rdname lines
#' @export
lines.GPR <- function(x, relTime0 = FALSE, ...){
  if(length(x@vel) > 0){  
    v <- x@vel[[1]]
  }else{
    v <- 0
  }
  if(any(dim(x) == 1)){
    z <- x@depth
    t0 <- x@time0
    if(isTRUE(relTime0)){
      z <- x@depth - x@time0
      t0 <- 0
    }
    # z <- seq(-x@time0, by = x@dz, length.out = length(x@data))
    #z <- x@depth - x@time0
    dots <- list(...)
    if(!is.null(dots[["log"]]) && dots[["log"]] == "y"){
      dots[["log"]] <- NULL
      x@data <- log(x@data)
    }
    dots[["x"]] <- z
    dots[["y"]] <- x@data
    invisible( do.call(lines, dots) )
    #lines(z, x@data,...)
  }else{
    stop("x must a vector!")
  }
}

#' Add a GPR trace points on a plot
#'
#' @method points GPR 
#' @name points
#' @rdname points
#' @export
points.GPR <- function(x, ...){
  if(length(x@vel)>0){  
    v <- x@vel[[1]]
  }else{
    v <- 0
  }
  if(any(dim(x) == 1)){
    #     z <- seq(-x@time0, by = x@dz, length.out = length(x@data))
    z <- x@depth - x@time0
    points(z, x@data,...)
  }else{
    stop("x must a vector!")
  }
}

#' Plot all the traces in one 1D plot
#' 
#' @param x Object of the class GPR
#' @param ... Arguments to be passed to \code{plot}/\code{line}
#' @name trPlot
#' @rdname trPlot
#' @export
setMethod(
  f = "trPlot",
  signature = "GPR",
  definition = function(x, ...){
    if(ncol(x) == 1){
      plot(x, ...)
    }else{
      dots <- list(...)
      if(!is.null(dots[["log"]]) && dots[["log"]] == "y"){
        dots[["log"]] <- ""
        x <- log(x)
        if(is.null(dots[["ylab"]])){
          dots[["ylab"]] <- "log(amplitude envelope mV)"
        }
      }
      if(is.null(dots[["ylab"]])) dots[["ylab"]] <- "amplitude envelope (mV)"
      
      dotsLine <- list()
      dotsLine[["X"]] <- x
      dotsLine[["MARGIN"]] <- 2
      dotsLine[["FUN"]] <- lines
      dotsLine[["x"]] <- depth(x)
      
      dotsLine[["lty"]] <- dots[["lty"]]
      dotsLine[["lwd"]] <- dots[["lwd"]]
      dotsLine[["col"]] <- dots[["col"]]
      # be carefull (dots$type redefined below)
      dotsLine[["type"]] <- dots[["type"]]  
      dotsLine[["pch"]] <- dots[["pch"]]
      
      if(is.null(dots[["ylim"]])){
        dots[["ylim"]] <- range(x, na.rm = TRUE)
        if(dots[["ylim"]][1] > 0){
          dots[["ylim"]][1] <- 0
        }else{
          dots[["ylim"]] <- max(abs(dots[["ylim"]])) * c(-1, 1)
        }
      }
      # if(is.null(dots[["ylim"]])){
      #   dots[["ylim"]] <- range(depth(x))
      # }

      dots[["x"]] <- x[,1]
      dots[["y"]] <- NULL
      dots[["type"]] <- "n"
      invisible( do.call(plot, dots) )
      #plot(x[,1], type = "n", ylim = ylim, xlim = range(depth(x)), ...)
      
      # lty, line width, lwd, color, col and for type = "b", pch.
      invisible(do.call(apply, dotsLine))
    }
  }
)












#--- REMOVE DUPLICATED TRACES (TRACES WITH (ALMOST) SAME POSITIONS) ---#
#' Remove traces with duplicated trace positions
#' 
#' checks for duplicates trace positions (up to precision defined by 'tol') 
#' and remove them from 'x' (object of the class GPR or GPRsurvey). 
#' If there is a series of consecutive traces with an inter-distance smaller
#' than the tolerance threshold defined by the computer precision, 
#' the function starts by removing traces every two traces and repeat 
#' the procedure until the trace inter-distances are larger that the threshold.
#' Example with 5 traces:
#' \itemize{
#'   \item distance between trace 1 and 2 < tol
#'   \item distance between trace 2 and 3 < tol
#'   \item distance between trace 3 and 4 < tol
#'   \item distance between trace 4 and 5 < tol
#' }
#' The algorithm first remove trace 2 and 4 and recompute 
#' the inter-trace distances:
#' \itemize{
#'   \item distance between trace 1 and 3 < tol
#'   \item distance between trace 3 and 5 > tol
#' }  
#' The algorithm remove trace 3. END!
#' @param x   An object of the class GPR
#' @param tol Length-one numeric vector: if the horizontal distance between two 
#'            consecutive traces is smaller than \code{tol}, then
#'            the second trace is removed.
#'            If \code{tol = NULL}, \code{tol} is set equal to
#'            \code{sqrt(.Machine$double.eps)}.
#' @param verbose Logical. \code{TRUE}: a message will be thrown, 
#'                \code{FALSE}: no message will be thrown.
#' @name trRmDuplicates 
#' @rdname trRmDuplicates            
#' @export
setMethod("trRmDuplicates", "GPR", function(x, tol = NULL, verbose = TRUE){
  if(length(x@coord) == 0 ){
    warning("No trace coordinates!")
    return(x)
  }
  #dist2D <- posLine(x@coord[, 1:2], last = FALSE)
  dist2D <- relTrPos(x)
  # in 'x' and 'topo'
  if(is.null(tol))  tol <- sqrt(.Machine$double.eps)
  tdbl <- which(abs(diff(dist2D)) < tol)
  # x <- x[, -(tdbl + 1)]
  diff(tdbl)
  check <- 0L
  while(length(tdbl) > 0){
    rmTr <- c()
    skip <- FALSE
    for(i in seq_along(tdbl)){
      if(i > 1 && (tdbl[i] - 1 == tdbl[i - 1])){
        tdbl[i] <- -999
        next
      }
      rmTr <- c(rmTr, tdbl[i] + 1)
      check <- check + 1L
    }
    x <- x[, -rmTr]  # remove trace in x
    # dist2D <- posLine(x@coord[, 1:2], last = FALSE)
    dist2D <- relTrPos(x)
    tdbl <- which(abs(diff(dist2D)) < tol)
  }
  if(verbose){
    message(check, " duplicated trace(s) removed from 'x'!")
  }
  #FIXME: use getArgs()!!!!
  #x@proc <- c(x@proc, "trRmDuplicates")
  proc(x) <- getArgs()
  return(x)
})

#' Interpolate trace positions from measurement (e.g., GPS).
#'
#' @param x      An object of the class GPR.
#' @param topo   A \eqn{m \times 4} numeric matrix, with \code{m} the number of 
#'               recorded trace positions.
#'               The first 3 columns are the 
#'               coordinates (x, y, z) and the last column the trace number 
#'               (the column names can now be freely chosen).
#' @param plot   A length-one boolean vector. If \code{TRUE} some 
#'               control/diagnostic plots are displayed.
#' @param r      A 'RasterLayer' object from the package 'raster' from which
#'               trace elevation \code{z} will be extracted based on the 
#'               trace position \code{(x, y)} on the raster. The extracted 
#'               trace elevation will overwrite the values from the third
#'               column of \code{topo}.
#' @param tol    Length-one numeric vector: if the horizontal distance between 
#'               two consecutive trace positions is smaller than \code{tol}, 
#'               then the traces in between as well as the second trace
#'               position are removed.
#'               If \code{tol = NULL}, \code{tol} is set equal to
#'               \code{sqrt(.Machine$double.eps)}.
#' @param method A length-three character vector defining the interpolation
#'               methods (same methods as in \code{signal::interp1}:
#'               "linear", "nearest", "pchip", "cubic", and "spline"). 
#'               First element for the interpolation of the 
#'               inter-trace distances, 
#'               second element for the interpolation of the horizontal 
#'               trace positions, and third element for the interpolation
#'               of the vertical trace positions.
#' @name interpPos 
#' @rdname interpPos
#' @export
### handle crs -> if geogrphic -> projection
setMethod("interpPos", "GPR", 
          function(x, topo, plot = FALSE, r = NULL, tol = NULL,
                   method = c("linear", "linear", "linear"), crs = NULL
                   ,...){
    if(is.list(topo) && !is.data.frame(topo)) topo <- topo[[1]]
    if(all(is.na(topo[,4]))){
      stop(x@name, ": no link between the measured points",
                  " and the GPR traces!")
    }
    if(ncol(topo) < 4){
      stop("'topo' has less than 4 columns!")
    }
    # if we have measured some points before the line start or 
    # after the end of the GPR Line, we delete them
    test <- which(!is.na(topo[, 4]))
    topo <- topo[min(test):max(test), ]
    topo[, 4] <- as.integer(topo[,4])
    # keep only the markers corresponding to existing traces
    topo <- topo[topo[, 4] > 0 & topo[, 4] <= ncol(x), ]
    # convert to UTM (useful for GPS data)
    # if(toUTM == TRUE){
    #   topoUTM <-  llToUTM(lat = topo[,2], 
    #                       lon = topo[,1], 
    #                       zone = NULL, south = NULL)
    #   topo[, 1:2] <- topoUTM$xy
    # }
    #--- 3D topo Distance ---#
    if(!is.null(r)){
      topo[,3] <- raster::extract(r, topo[, 1:2], method = "bilinear")
      if(sum(is.na(topo[, 3])) > 0){
        stop("'extract(r, topo[, c(\"E\",\"N\")], method = \"bilinear\")' ",
             "returns 'NA' values!\n", 
             "Not all GPR positions fall within raster extent or\n",
             "there are 'NA' values in raster 'r'!")
      }
    }
    # check for duplicates in TRACE ID and remove them!
    topo <- rmRowDuplicates(topo, topo[, 4])
    # order topo by increasing traceNb
    topo <- topo[order(topo[, 4]), ]
    #----- REMOVE DUPLICATED TRACES (TRACES WITH (ALMOST) SAME POSITIONS) -----#
    if(is.null(tol))  tol <- (.Machine$double.eps)
    # topo <- mrk
    dist2D <- posLine(topo[, 1:2], last = FALSE)
    tdbl <- which(abs(diff(dist2D)) < tol)
    #if(length(tdbl) > 0){
    nb_dbl_tr_topo <- 0
    nb_dbl_tr_x <- 0
    while(length(tdbl) > 0){    # add
      for(i in seq_along(tdbl)){
        # i <- 1
        # number of trace to remove
        dtr <- topo[tdbl[i] + 1 , 4] - topo[tdbl[i], 4]
        # traces to remove (ID)
        w <- topo[tdbl[i], 4] + seq_len(dtr)
        # remove trace in x
        x <- x[, -w]
        # remove traces in topo
        topo <- topo[-(tdbl[i] + 1), ]
        # and actualise the trace numbers of the trace above the delete trace
        if(tdbl[i] + 1 <= nrow(topo)){
          v <- (tdbl[i] + 1):nrow(topo)
          # if(any(is.na(topo[v,]))) stop( "lkjlkj")
          topo[v, 4] <- topo[v, 4] -  dtr
        }
        tdbl <- tdbl - 1
        nb_dbl_tr_x <- nb_dbl_tr_x + dtr
        nb_dbl_tr_topo <- 1 + nb_dbl_tr_topo
      }
      dist2D <- posLine(topo[, 1:2], last = FALSE)
      tdbl <- which(abs(diff(dist2D)) < tol)
    }
    if(nb_dbl_tr_topo > 0){
      message(nb_dbl_tr_x, " trace(s) removed from 'x' ",
              "(", nb_dbl_tr_x, " duplicate trace position(s) in 'topo'" )
    }
    #--------------------------------------------------------------------------#
    # if there are points measured with the total station
    # that do not have an fiducial (FID) > interpolate them!
    myWarning <- ""
    if(anyNA(topo[,4])){
      # dist3D[topo$PNAME %in% FID$PNAME] > distance for the points 
      # also recorded in FID
      myWarning <- "\npoints total station without fiducials"
      dist3D <- posLine(topo[, 1:3], last = FALSE)
      test <- !is.na(topo[,4])
      intMeth <- ifelse(sum(test) > 2, method[1], "linear")
      traceNb <- signal::interp1(x      = dist3D[test], 
                                 y      = topo[test, 4], 
                                 xi     = dist3D,
                                 method = intMeth,
                                 extrap = TRUE)
      topo[, 4] <- as.integer(round(traceNb))
      # check for duplicates in TRACE ID and remove them!
      topo <- rmRowDuplicates(topo, topo[, 4])
    }
    if(all(seq_along(x@pos) %in% topo[, 4])){
      A <- as.matrix(topo[topo[, 4] %in% seq_along(x@pos), 1:3])
      message("No interpolation required because the trace positions\n",
              "of all GPR traces is already available (in object 'topo')!")
    }else{
      #--- INTERPOLATION ---#
      A <- interp3DPath(x = topo[, 1:3], pos = topo[, 4], 
                        posi = seq_along(x@pos), r = r,
                        method = method)
      colnames(A) <- c("x", "y", "z")
      # add fiducial to indicate which points/traces were used for interpolation
      # x@fid[topo[,4]] <- paste0(x@fid[topo[,4]], " *")
      # fid(x) <- trimStr(fid(x))
    }
    # diagnostic plots
    dist3Dint <- posLine(A, last = FALSE)
    message(x@name, ": mean dx = ", round( mean(diff(dist3Dint)), 3 ), 
            ", range dx = [", round( min(diff(dist3Dint)), 3 ),", ", 
            round( max(diff(dist3Dint)), 3 ),"]", myWarning)
    if(plot == TRUE){
      op <- par(no.readonly=TRUE)
      par(mfrow=c(1, 3))
      plot(topo[, 4], dist3D, pch = 20, col = "red", cex = 2, asp = 1,
           xlab = "trace number", ylab = "trace spacing (3D)",
           xlim = range(seq_along(x@pos)), ylim = range(dist3Dint), 
           main = paste( x@name))
      points(seq_along(x@pos), dist3Dint, pch = 20, col = "blue", cex = 0.6)
      plot(dist3Dint, A[, 3], type = "l", asp = 10, 
           xlab = "interpolated trace spacing", 
           ylab = "interpolated elevation",
           main = paste0(x@name, " min dx = ", round(min(diff(dist3Dint)), 2), 
                         "  max dx = ", round(max(diff(dist3Dint)), 2)))
      points(dist3D, topo[,3], pch = 20, col = "red")
      plot(topo[, c(1,2)], col = 1, type = "l", lwd = 2, asp = 1,
           ylim = range(A[, 2]), xlim = range(A[, 1]), 
           main = paste0(x@name, " mean dx=", round(mean(diff(dist3Dint)),2)))
      points(topo[, c(1,2)], col = 1, pch = 20, cex = 2)
      lines(A[, 1], A[, 2], col = 2, lwd = 1)
      Sys.sleep(1)
      par(op)
    }
    # if(toUTM == TRUE){
    #   crs(x) <- topoUTM$crs
    # }
    x@coord <- A
    x@proc <- c(x@proc, "interpPos")
    return(x)
  }
)

# remove rows of x with duplicated element in v
# length(v) == nrow(x)
# use the R-base function 'duplicated()'
rmRowDuplicates <- function(x, v){
  iRM <- which(duplicated(v))
  if(length(iRM) > 0){
    return( x[-iRM, ])
  }else{
    return(x)
  }
}

spRmDuplicates <- function(x, tol = NULL, verbose = TRUE){
  if(length(x@coord) == 0 ){
    warning("No trace coordinates!")
    return(x)
  }
  if(is.null(tol))  tol <- .Machine$double.eps
  dist2D <- posLine(x@coord[, 1:2], last = FALSE)
  tdbl <- which(abs(diff(dist2D)) < tol)
  check <- 0L
  while(length(tdbl) > 0){
    rmTr <- c()
    for(i in seq_along(tdbl)){
      if(i > 1 && tdbl[i-1] == tdbl[i] - 1){
        tdbl[i] <- -999
      }else{
        rmTr <- c(rmTr, tdbl[i])
        check <- check + 1L
      }
    }
    x <- x[, -rmTr]  # remove trace in x
    dist2D <- posLine(x@coord[, 1:2], last = FALSE)
    tdbl <- which(abs(diff(dist2D)) < tol)
  }
  if(isTRUE(verbose)){
    message(check, " duplicated trace(s) removed from 'x'!")
  }
  return(x)
}

# x = topo[, c("E", "N", "Z")] or C("x", "y", "z")
# dist3D <- posLine(x, last = FALSE)
# posi <- seq_along(x@pos)
# pos <- topo[, "TRACE"]
# r
interp3DPath <- function(x, pos, posi, r = NULL,
                         method = c("linear", "spline", "pchip")){
  if(ncol(x) > 3) x <- x[, 1:3]
  dist3D <- posLine(x, last = FALSE)
  #--- INTERPOLATION dist3D ---#
  intMeth <- ifelse(length(dist3D) > 2, method[1], "linear")
  dist3DInt   <- signal::interp1(pos, dist3D, posi, 
                                 method = intMeth , extrap=TRUE)
  #--- INTERPOLATION N,E,Z ---#
  ENZ <- matrix(nrow = length(posi), ncol = 3)
  intMeth <- ifelse(length(dist3D) > 2, method[2], "linear")
  ENZ[, 1] <- signal::interp1(dist3D, x[, 1], dist3DInt, 
                              method = intMeth, extrap = NA)
  ENA <- is.na(ENZ[, 1])
  ENZ[ENA, 1] <- signal::interp1(dist3D, x[, 1], dist3DInt[ENA], 
                                 method = "linear", extrap = TRUE)
  ENZ[, 2] <- signal::interp1(dist3D, x[, 2], dist3DInt, 
                              method = intMeth , extrap = NA)
  ZNA <- is.na(ENZ[, 2])
  ENZ[ZNA, 2] <- signal::interp1(dist3D, x[, 2], dist3DInt[ZNA], 
                                 method = "linear", extrap = TRUE)
  if(is.null(r) && ncol(x) == 3){
    intMeth <- ifelse(length(dist3D) > 2, method[3], "linear")
    ENZ[, 3] <- signal::interp1(dist3D, x[, 3], dist3DInt, 
                                method = intMeth, extrap = NA)
  }else if(!is.null(r)){
    ENZ[, 3] <- raster::extract(r, cbind(ENZ[, 1], ENZ[, 2]), 
                                method = "bilinear")
  } 
  lastNA  <- max(which(!is.na(ENZ[, 3])))
  firstNA <- min(which(!is.na(ENZ[, 3])))
  if(firstNA > 1){
    ENZ[1:(firstNA - 1), 3] <- ENZ[firstNA, 3]
  } 
  if(lastNA < length(ENZ[, 3])){
    ENZ[(lastNA + 1):length(ENZ[, 3]), 3]<- ENZ[lastNA, 3]
  }
  # message(x@name, ": mean dx = ", round(mean(diff(dist3DInt)), 3), 
  #         "  range dx = ",round(min(diff(dist3DInt)), 3)," - ", 
  #         round(max(diff(dist3DInt)), 3))
  return(ENZ)
}

#' Interpolate GPR coordinates from GPS data (GPGGA string) data
#'
#' @param x Object of the class GPR
#' @param gpgga output of the function "readGPGGA()"
#' @name interpPosFromGPGGA 
#' @rdname interpPosFromGPGGA
#' @export
interpPosFromGPGGA <- function(ntr, GPGGA, tol = NULL, backproject = TRUE){
  
  mrk0 <- GPGGA
  
  #--- Convert to UTM
  # fixme: consider S (South) and W (West)
  # tr_crs <-  llToUTM(lat = median(sp::coordinates(mrk0)[,2]), 
  #                     lon = median(sp::coordinates(mrk0)[,1]), 
  #                     zone = NULL, south = NULL)$crs
  # mrk <- as.data.frame(sp::spTransform(mrk0, tr_crs))
  xy_crs <-  llToUTM(lat = sf::st_coordinates(mrk0)[,2], 
                      lon = sf::st_coordinates(mrk0)[,1], 
                      zone = NULL, south = NULL)
  
  tr_crs <- xy_crs$crs
  
  mrk <- as.data.frame(xy_crs$xy)
  
  
  #---- 4. remove duplicates
  dist2D <- posLine(mrk[, c("x", "y")], last = FALSE)
  # in 'x' and 'mrk'
  if(is.null(tol))  tol <- sqrt(.Machine$double.eps)
  tdbl <- which(abs(diff(dist2D)) < tol)
  while(length(tdbl) > 0){
    mrk <- mrk[ -(tdbl + 1), ]
    dist2D <- posLine(mrk[, c("x", "y", "z")], last = FALSE) # mod
    tdbl <- which(abs(diff(dist2D)) < tol)
  }
  
  #--- Interpolate trace position
  mrk_time <- as.numeric(as.POSIXct(mrk$time))
  mrk_pos <- posLine(mrk[,c("x", "y")])
  tr_time <- seq(from = mrk_time[1], to = tail(mrk_time, 1), 
                 length.out = ntr)
  tr_pos <- signal::interp1(x = mrk_time, y = mrk_pos, xi = tr_time, 
                            method = "spline", extrap = NA)
  
  
  # plot(mrk_pos, mrk_time)
  # points(tr_pos, tr_time, pch = 20)
  # check: compare with GPR data -> OK
  # plot(tr_pos, posLine(coord(x[[1]])[,1:2]))
  
  #--- Interpolate trace coordinates
  tr_xyz <- matrix(0, nrow = ntr, ncol = 3)
  tr_xyz[, 1] <- signal::interp1(x = mrk_pos, y = mrk$x, xi = tr_pos, 
                          method = "spline", extrap = NA)
  tr_xyz[, 2] <- signal::interp1(x = mrk_pos, y = mrk$y, xi = tr_pos, 
                          method = "spline", extrap = NA)
  tr_xyz[, 3] <- signal::interp1(x = mrk_pos, y = mrk$z, xi = tr_pos, 
                          method = "spline", extrap = NA)
  
  # plot(tr_x, tr_y, pch = 20)
  # 
  # 
  # plot(tr_pos, tr_z)
  # points(mrk_pos, mrk$z, pch = 20, col = "red")
  

  if(backproject == TRUE){
    tr_xyz[,1:2] <- UTMToll(xy = tr_xyz[,1:2], xy_crs = tr_crs)
    tr_crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
  }
  
  # utr <- sf::st_as_sf(as.data.frame(uu), coords = c(1,2))
  # plot(sf::st_coordinates(xyz), asp = 1, type = "l")
  # points(sf::st_coordinates(utr), pch = 20)
  # plot(utr, add = TRUE, pch = 20)

  # coord(x) <- tr_xyz
  # crs(x) <- mrk_crs
  # x@proc <- c(x@proc, "interpPosFromGPGGA")
  return(list(x = tr_xyz, crs = tr_crs))
}


#' Interpolate GPR coordinates from geoJSON data
#'
#' @param x Object of the class GPR
#' @param geojson Either a geojson string or a filepath pointing to a 
#'                geojson file
#' @export
interpPosFromGeoJSON <- function(x, geojson, tol = NULL, backproject = TRUE){
  #---- 1. Read geoJSON
  xyz <- geojsonsf::geojson_sf(geojson)
  # sf::st_crs(xyz)
  
  #---- 2. convert to UTM
  XY <- sf::st_coordinates(xyz)
  XY <- XY[XY[, "L1"] == XY[1, "L1"], ]       # take the first structure
  u <- llToUTM(lat = XY[,2], lon = XY[,1])
  #plot(u$xy, type = "l", asp = 1)
  
  #---- 3. create "topo" file
  mrk <- cbind(u$xy, 0, NA)
  colnames(mrk) <- c("x", "y", "z", "tn")
  
  #---- 4. remove duplicates
  dist2D <- posLine(mrk[, c("x", "y")], last = FALSE)
  # in 'x' and 'mrk'
  if(is.null(tol))  tol <- sqrt(.Machine$double.eps)
  tdbl <- which(abs(diff(dist2D)) < tol)
  while(length(tdbl) > 0){
    mrk <- mrk[ -(tdbl + 1), ]
    dist2D <- posLine(mrk[, c("x", "y", "z")], last = FALSE) # mod
    tdbl <- which(abs(diff(dist2D)) < tol)
  }
  
  #---- 5. trace interpolation
  # a) interpolate shape points to traces
  trFIDPos <- approx(x = c(0, tail(dist2D, 1)),
                     y = c(1, length(x)),
                     xout = dist2D)
  
  # b) interpolate coordinates
  tr_xyz <- matrix(0, nrow = ncol(x), ncol = 3)
  tx_x <- approx(x = trFIDPos$y,
                 y = mrk[,"x"],
                 xout = seq_along(x))
  tx_y <- approx(x = trFIDPos$y,
                 y = mrk[,"y"],
                 xout = seq_along(x))
  tx_z <- approx(x = trFIDPos$y,
                 y = mrk[,"z"],
                 xout = seq_along(x))
  tr_xyz[,1] <- tx_x$y
  tr_xyz[,2] <- tx_y$y
  tr_xyz[,3] <- tx_z$y
  # plot(u$xy, type = "l", asp = 1)
  # lines(tx_x$y, tx_y$y, type = "p")
  
  if(backproject == TRUE){
    tr_xyz[,1:2] <- UTMToll(xy = tr_xyz[,1:2], xy_crs = u$crs)
  }
  
  # utr <- sf::st_as_sf(as.data.frame(uu), coords = c(1,2))
  # plot(sf::st_coordinates(xyz), asp = 1, type = "l")
  # points(sf::st_coordinates(utr), pch = 20)
  # plot(utr, add = TRUE, pch = 20)
  
  return(tr_xyz)
}

#' Interpolate GPR coordinates from xyz coordinates
#'
#' This function interpolate the trace positions from a series of coordinates
#' assuming no link between the traces and the points (e.g., it is not know
#' which trace belong to which point). Therefore, it is assumed, that the
#' trace spacing is constant over the GPR line. 
#'
#' @param x Object of the class GPR
#' @param xyz [\code{matrix(n, 2|3)}] A two-columns (or three-column matrix) 
#'            containing the x and y (or x, y, and z) coordinates.
#' @param tol [\code{numeric(1)}] A tolerance value used to remove duplicated
#'            coordinates in \code{xyz}. When left equal to \code{NULL}
#'            \code{tol} is internally set equal to 
#'            \code{sqrt(.Machine$double.eps)}.
#' @return [\code{GPR}] object.    
#' @name interpPosFromXYZ 
#' @rdname interpPosFromXYZ   
#' @export
setMethod("interpPosFromXYZ", "GPR", 
          function(x, xyz,  tol = NULL){
  
  #---- 3. create "topo" file
  mrk <- as.matrix(xyz)
  if(ncol(mrk) == 2){
    mrk <- cbind(mrk, 0)
  }
  colnames(mrk) <- c("x", "y", "z")
  
  #---- 4. remove duplicates
  dist2D <- posLine(mrk[, c("x", "y")], last = FALSE)
  # in 'x' and 'mrk'
  if(is.null(tol))  tol <- sqrt(.Machine$double.eps)
  tdbl <- which(abs(diff(dist2D)) < tol)
  while(length(tdbl) > 0){
    mrk <- mrk[ -(tdbl + 1), ]
    dist2D <- posLine(mrk[, c("x", "y", "z")], last = FALSE) # mod
    tdbl <- which(abs(diff(dist2D)) < tol)
  }
  
  #---- 5. trace interpolation
  # a) interpolate shape points to traces
  trFIDPos <- approx(x = c(0, tail(dist2D, 1)),
                     y = c(1, length(x)),
                     xout = dist2D)
  
  # b) interpolate coordinates
  tr_xyz <- matrix(0, nrow = ncol(x), ncol = 3)
  tx_x <- approx(x = trFIDPos$y,
                 y = mrk[,"x"],
                 xout = seq_along(x))
  tx_y <- approx(x = trFIDPos$y,
                 y = mrk[,"y"],
                 xout = seq_along(x))
  tx_z <- approx(x = trFIDPos$y,
                 y = mrk[,"z"],
                 xout = seq_along(x))
  tr_xyz[,1] <- tx_x$y
  tr_xyz[,2] <- tx_y$y
  tr_xyz[,3] <- tx_z$y
  
  coord(x) <- tr_xyz
  x@pos <- posLine(tr_xyz[, 1:2])
  return(x)
})


#' Relative trace position on the GPR profile.
#'
#' Trace position computed from (x, y)
#' @name relTrPos
#' @rdname relTrPos
#' @export
setMethod("relTrPos", "GPR", function(x, last = FALSE){
  if(length(x@coord) > 0){
    return(posLine(x@coord[ ,1:2], last = last))
  }else{
    stop("No coordinates")
  }
} 
)

#' Relative trace position acounting for elevation.
#'
#' Trace position computed from (x, y, z)
#' @name relTrPos3D
#' @rdname relTrPos
#' @export
setMethod("relTrPos3D", "GPR", function(x, last = FALSE){
  return(posLine(x@coord, last = last))
} 
)

#' @export
relPos <- function(x){
  stop("Use 'relTrPos()' instead!")
}

#' Reverse the trace position.
#'
#' @param x Object of the class \code{GPR} or \code{GPRsurvey}
#' @param id Only if \code{x} is an object of the class \code{GPRsurvey}. If 
#'           \code{id = NULL} and \code{x} has coordinates, \code{reverse()} 
#'           will cluster the GPR data according to their names (e.g., 
#'           cluster 1 = XLINE01, XLINE02, XLINE03; cluster 2 = YLINE01, 
#'           YLINE02; cluster 3 = XYLINE1, XYLINE2, XYLINE3) and reverse the
#'           data such that all GPR lines within the same cluster have the
#'           same orientation (up to a tolerance value \code{tol}).
#' @param to Length-one numeric vector. Tolerance angle in radian to determine
#'           if the data have the same orientation. The first data of the 
#'           cluster is set as reference angle \eqn{\alpha_0}, then for data 
#'           \eqn{i} in the same cluster, if \eqn{\alpha_i} is not between
#'           \eqn{\alpha_0 - \frac{tol}{2}} and 
#'           \eqn{\alpha_0 + \frac{tol}{2}}, then the data is reversed.
#' @name reverse
#' @rdname reverse
#' @examples 
#' \dontrun{
#' # SU class GPRsurvey
#' SU <- reverse(SU, id = "zigzag")
#' # identical to above
#' SU <- reverse(SU, id = seq(from = 2, to = length(SU), by = 2))
#' }
#' @export
setMethod("reverse", "GPR", function(x, id = NULL,  tol = 0.3){
  xnew <- x
  xnew@data <- x@data[,length(x):1]
  xnew@time0 <- rev(x@time0)
  xnew@time <- rev(x@time)
  xnew@fid <- rev(x@fid)
  xnew@ann <- rev(x@ann)
  if(length(x@coord)>0){
    xnew@coord <- x@coord[nrow(x@coord):1,]
  }
  if(length(x@rec)>0){
    xnew@rec <- x@rec[nrow(x@rec):1,]
  }
  if(length(x@trans)>0){
    xnew@trans <- x@trans[nrow(x@trans):1,]
  }
  if(length(x@delineations) > 0){
    
    xnew@delineations <- rapply(x@delineations, .revMat, 
                                how = "replace", n = ncol(x))
  }
  proc(xnew) <- getArgs()
  return(xnew)
}
)

.revMat <- function(x, n){
  x[, 1] <- n - x[, 1] + 1
  x <- apply(x, 2, rev)
  return(x)
}

# Oriented Bounding Box
#' @name tpOBB2D
#' @rdname tpOBB2D
#' @export
setMethod("tpOBB2D", "GPR", function(x){
  if(length(x@coord) > 0){
    return(OBB(x@coord[,1:2]))
  }else{
    stop("x has no coordinates.")
  }
})

#' @name svAngle
#' @rdname svAngle
#' @export
setMethod("svAngle", "GPR", function(x){
  if(length(x@coord) > 0){
    dEN <- x@coord[1,1:2] - tail(x@coord[,1:2],1)
    angl_EN <- atan2(dEN[2], dEN[1])
    # angl_EN/pi * 180
    orb <- tpOBB2D(x)
    dEN <- orb[1,] - orb[2,]
    i <- which.max(diff(posLine(orb)))[1]
    dOBB <- orb[i + 1,] - orb[i,]
    angl_OBB <- atan2(dOBB[2], dOBB[1])
    # angl_OBB/pi * 180
    # abs(angl_EN - angl_OBB) / pi * 180
    if(pi * 6/5 > abs(angl_EN - angl_OBB) && abs(angl_EN - angl_OBB)  > pi* 4 /5){
      angl_OBB <- angl_OBB + pi
      if(angl_OBB > pi) angl_OBB <- angl_OBB - 2*pi
    }
    return(angl_OBB)
  }else{
    stop("x has no coordinates.")
  }
})

#' Shift estimation between two GPR profiles.
#'
#' @name shiftEst
#' @rdname shiftEst
#' @export
setMethod("shiftEst", "GPR", function(x, y = NULL, 
                                      method=c("phase", "WSSD"), dxy = NULL, ...){
  return( displacement(x@data, y@data, method=method, dxy = dxy) )
})





#' Trace interpolation at regularly spaced positions 
#'
#' @name regInterpPos
#' @rdname regInterpPos
#' @export
setMethod("regInterpPos", "GPR", function(x, type = c("linear", "cosine"), 
                                          dx = NULL){
  type <- match.arg(type, c("linear", "cosine"))
  if(length(x@coord)>0){
    #xpos <- posLine(x@coord)
    xpos <- relTrPos(x)
  }else{
    xpos <- x@pos
  }
  if(is.null(dx)){
    dx <- min(abs(diff(xpos)))
  }
  xo <- seq(min(xpos), max(xpos), by = dx)
  xnew <- x[,rep(1, length(xo))]
  for(k in seq_along(xo)){
    if( any(di <- abs(xo[k] - xpos) < 10^-3) ){
      ki <- which(di)[1]
      xnew@data[, k] <- x@data[,ki]
      xnew@time[k] <- x@time[ki]
      xnew@time0[k] <- x@time0[ki]
      if(length(x@coord) > 0){
        xnew@coord[k,] <- x@coord[ki,]
      }
      if(length(x@rec) > 0){
        xnew@rec[k,] <- x@rec[ki,]
      }
      if(length(x@trans) > 0){
        xnew@trans[k,] <- x@trans[ki,]
      }
    }else{
      testm <- xpos < xo[k]
      testp <- xpos > xo[k]
      km <- ifelse(any(testm), tail(which(testm),1), 0)
      kp <- ifelse(any(testm), which(testp)[1], 0)
      w <- (xpos[kp] - xo[k])/(xpos[kp] - xpos[km])
      #       wm <- 1/(xo[k] - xpos[km]) 
      #       wp <- 1/(xpos[kp] - xo[k])
      #       xnew@data[, k] <- (wm*x@data[,km] + wp*x@data[,kp])/(wm + wp)
      if(type == "linear"){
      }else if(type == "cosine"){
        w <- (1-cos(w*pi))/2
      }
      xnew@data[,k] <- w * x@data[,km] + (1 - w) * x@data[,kp]
      xnew@time[k] <- round( w * x@time[km] + (1 - w) * x@time[kp] )
      xnew@time0[k] <- w * x@time0[km] + (1 - w) * x@time0[kp]
      if(length(x@coord) > 0){
        xnew@coord[k,] <- w * x@coord[km,] + (1 - w) * x@coord[kp,]
      }
      if(length(x@rec) > 0){
        xnew@rec[k,] <- w * x@rec[km,] + (1 - w) * x@rec[kp,]
      }
      if(length(x@trans) > 0){
        xnew@trans[k,] <- w * x@trans[km,] + (1 - w) * x@trans[kp,]
      }
    }
  }
  xnew@traces <- seq_along(xo)
  xnew@pos <- xo
  xnew@fid <- interpFid(xpos, xo, x@fid)
  xnew@ann <- interpFid(xpos, xo, x@ann)
  xnew@dx <- round(dx,3)
  proc(xnew) <- getArgs()
  #     xnew@proc <- c(xnew@proc, proc)
  return(xnew)
}
)

# interpolate position fiducial/annotation
interpFid <- function(xposold, xposnew, fidOld){
  fidNew <- rep("", length(xposnew))
  fids <- which(fidOld != "")
  for(i in seq_along(fids)){
    fidpos <- which.min(abs(xposold[fids[i]] - xposnew))
    fidNew[fidpos] <- fidOld[fids[i]]
  }
  return(fidNew)
}










