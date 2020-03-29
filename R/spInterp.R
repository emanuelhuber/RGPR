#' Interpolate trace positions from measurement (e.g., GPS).
#'
#' @param x      [\code{GPR|GRPsurvey}]
#' @param topo [\code{matrix|list}] A numeric matrix or a list of matrix 
#'             (if \code{x} is a \code{GPRsurvey} object) with at least
#'             3 columns (the (x,y,z)-coordinates) and optionally a fourth
#'             column containing the trace number (id).
#' @param tt [\code{numeric|list}] A numeric vector or a list of vectors  
#'           (if \code{x} is a \code{GPRsurvey} object) corresponding to the
#'           trace recording time (e.g., output GPS device). If provided,
#'           it will be assumed that the traces were recorded at regular
#'           time interval (the 4th column of \code{topo} will be ignored).
#' @param r [\code{raster::RasterLayer}]  A 'RasterLayer' 
#'          object from the package 
#'          'raster' from which trace elevation \code{z} will be extracted 
#'          based on the trace position \code{(x, y)} on the raster. 
#'          The extracted trace elevation will overwrite the values from the 
#'          third column of \code{topo}.
#' @param lonlat [\code{logical(1)}] Are the coordinates in geographic WGS84 
#'               lat/long coordinate reference system (e.g., GPS data)?
#' @param projToUTM [\code{logical(1)}] Should the geographic coordinates be
#'                  projected into the corresponding UTM WGS84 coordinate 
#'                  reference system (only if \code{lonlat = TRUE}).
#'                  RGPR will guess the correct UTM WGS84 project.
#' @param CRSin [\code{character(1)|integer(1)}] CRS of \code{topo}, only used
#'              if \code{lonlat = FALSE}. Either a valid proj4string character
#'              or a valid EPSG integer value 
#'              (see \code{\link[rgdal]{make_EPSG}}).
#' @param CRSout [\code{character(1)|integer(1)}] CRS to which the trace 
#'               coordinates must be projected. Either a valid proj4string 
#'               character or a valid EPSG integer value 
#'               (see \code{\link[rgdal]{make_EPSG}}).
#' @param odometer [\code{logical(1)}] Was the data collection triggered by
#'                                     an odometer? If yes, then 
#'                                     \code{odometer = TRUE}.
#' @param tol  [\code{numeric(1)}]     Length-one numeric vector: if the horizontal distance between 
#'               two consecutive trace positions is smaller than \code{tol}, 
#'               then the traces in between as well as the second trace
#'               position are removed.
#'               If \code{tol = NULL}, \code{tol} is set equal to
#'               \code{sqrt(.Machine$double.eps)}.
#' @param verbose [\code{logical(1)}] If \code{FALSE}, all messages and warnings are
#'                suppressed (use with care).
#' @param plot [\code{logical(1)}] If \code{TRUE} some control/diagnostic 
#'             plots are displayed.
#' @param method [\code{character(3)}] The interpolation
#'               methods:
#'               First element for the interpolation of the 
#'               inter-trace distances, 
#'               second element for the interpolation of the horizontal 
#'               trace positions, and third element for the interpolation
#'               of the vertical trace positions.
#'               The methods that can be used are 
#'               \code{linear}, \code{nearest}, \code{pchip}, \code{cubic}, 
#'               and \code{spline}
#'                (same methods as in \code{\link{signal}{interp1}}.
#' @return x with interpolated trace positions.
#' @name spInterp
setGeneric("spInterp", function(x, topo, tt = NULL, r = NULL, 
                                lonlat = FALSE, projToUTM = FALSE, 
                                CRSin = NULL, CRSout = NULL, odometer = FALSE,
                                tol = NULL, verbose = TRUE, plot = TRUE,
                                method = c("linear", "linear", "linear"))
  standardGeneric("spInterp"))


#' @rdname spInterp
#' @export
setMethod("spInterp", "GPR", 
 function(x, topo, tt = NULL, r = NULL, 
          lonlat = FALSE, projToUTM = FALSE, 
          CRSin = NULL,
          CRSout = NULL,
          odometer = FALSE,
          tol = NULL, verbose = TRUE, plot = TRUE,
          method = c("linear", "linear", "linear")){
  
   # if lonlat = TRUE or geo = TRUE or projected = FALSE
   # lonlatToUTM <- function(lon, lat, zone = NULL, south = NULL)
   # topo_crs <- NULL
   if(inherits(topo, "sf")){
     crs(x) <- sf::st_crs(topo)
     x_c <- sf::st_coordinates(topo)
     # handle "LINESTRING" type (for geojson for example)
     test <- grepl("L", colnames(x_c))
     if(any(test)){
       x_c <- x_c[x_c[, test] == x_c[1, test],]
     }
     id_c <- c(colnames(x_c) %in% c("X", "Y", "Z"))
     id_c0 <- c(c("X", "Y", "Z") %in% colnames(x_c))
     x_c0 <- matrix(0, nrow = nrow(x_c), ncol = 3)
     x_c0[,id_c0] <- x_c[, id_c]
     x_g <- sf::st_drop_geometry(topo)
     if(length(x_g) > 0){
      topo <- cbind(x_c0, x_g)
     }else{
       topo <- x_c0
     }
   }
   if(isTRUE(lonlat)){
     if(is.na(x@crs) || trimStr(sf::st_crs(x@crs)$proj4string) == "+proj=longlat +datum=WGS84 +no_defs" ){
        crs(x) <- 4326
        topo_utm <- lonlatToUTM(lon = topo[, 1], lat = topo[, 2])
        tryCatch({crs(x) <- topo_utm$crs
                  topo[, 1:2] <- topo_utm$xy
                  if(isFALSE(projToUTM) && is.null(CRSout)){
                    CRSout <- 4326
                  }}
        )
     }else{
       lonlat <- FALSE
     }
     # topo_crs <- topo_utm$crs
     # topo[, 1:2] <- topo_utm$xy
   }else if(!is.null(CRSin)){
     crs(x) <- CRSin
     # topo_crs <- crs(x)
   }
   
   
   # 3 Cases:
   #   - (x, y, z, tr) trace markers
   #   - (x, y, z, t) time (signal recording at regular time interval)
   #   - (x, y, z) only coordinates
   
   # Duplicated trace positions -> remove trace?
   
   # What to do with geographic coordinates?
   
   if(ncol(topo) >= 4 && all(is.na(topo[,4]))){
     warning(x@name, ": no link between the measured points",
             " and the GPR traces!")
     topo <- topo[, 1:4]
   }
   if(ncol(topo) < 3){
     stop("'topo' must have at least 3 columns (x, y, z)!")
   }
   if(is.null(tol)) tol <- .Machine$double.eps
   #----- REMOVE UNUSABLE TRACE MARKERS ----------------------------------------#
   if(ncol(topo) >=  4 && is.null(tt)){
     # if we have measured some points before the line start or 
     # after the end of the GPR Line, we delete them
     test <- which(!is.na(topo[, 4]))
     topo <- topo[min(test):max(test), ]
     topo[, 4] <- as.integer(topo[,4])
     # keep only the markers corresponding to existing traces
     topo <- topo[topo[, 4] > 0 & topo[, 4] <= ncol(x), ]
     #----- REMOVE DUPLICATED TRACE ID IN TOPO ---------------------------------#
     topo <- .rmRowDuplicates(topo, topo[, 4])
     # order topo by increasing traceNb
     topo <- topo[order(topo[, 4]), ]
     #----- REMOVE DUPLICATED TRACES (TRACES WITH (ALMOST) SAME POSITIONS) -----#
     xx <- .rmTraceIDDuplicates(x, topo, tol = tol, verbose = verbose)
     x <- xx$x
     topo <- xx$topo
   }else{
     dist2D <- pathRelPos(topo[, 1:2])
     tbdltd <- which(abs(diff(dist2D)) < tol) # to be deleted
     while(length(tbdltd) > 0){
       topo <- topo[ -(tbdltd + 1), ]
       dist2D <- pathRelPos(topo[, 1:2]) # mod
       tbdltd <- which(abs(diff(dist2D)) < tol)
     }
   }
   #----- IF RASTER -> EXTRACT POSITION ELEVATION FROM RASTER ------------------#
   if(!is.null(r)){
     topo[,3] <- raster::extract(r, topo[, 1:2], method = "bilinear")
     if(sum(is.na(topo[, 3])) > 0){
       stop("'extract(r, topo[, c(\"E\",\"N\")], method = \"bilinear\")' ",
            "returns 'NA' values!\n", 
            "Not all GPR positions fall within raster extent or\n",
            "there are 'NA' values in raster 'r'!")
     }
   }
   ntr <- ncol(x@data)
   myWarning <- ""
   #----- INTERPOLATE MARKER POSITION THAT ARE NOT LINKED TO A TRACE -----------#
   if(ncol(topo) >= 4 && is.null(tt)){
     # if there are points measured with the total station
     # that do not have an fiducial (FID) > interpolate them!
     if(anyNA(topo[,4])){
       # dist3D[topo$PNAME %in% FID$PNAME] > distance for the points 
       # also recorded in FID
       myWarning <- "\npoints total station without fiducials"
       dist3D <- pathRelPos(topo[, 1:3])
       test <- !is.na(topo[,4])
       intMeth <- ifelse(sum(test) > 2, method[1], "linear")
       traceNb <- signal::interp1(x      = dist3D[test], 
                                  y      = topo[test, 4], 
                                  xi     = dist3D,
                                  method = intMeth,
                                  extrap = TRUE)
       topo[, 4] <- as.integer(round(traceNb))
       # check for duplicates in TRACE ID and remove them!
       topo <- .rmRowDuplicates(topo, topo[, 4])
     }
   }
   #----- IF THERE IS NOTHING TO INTERPOLATE (= 1 MARKER PER TRACE) ------------#
   if( ncol(topo) >= 4 && all(seq_len(ntr) %in% topo[, 4]) ){
     ENZ <- as.matrix(topo[topo[, 4] %in% seq_along(x@pos), 1:3])
     message("No interpolation required because the trace positions\n",
             "of all GPR traces is already available (in object 'topo')!")
   }else{
     #--- PRE-INTERPOLATION ---#
     if(ncol(topo) >=  4 && is.null(tt)){
       mrk <- topo[, 4]                    # pos
       mrki <- seq_len(ntr)       # posi
       v <- 1:2
       if(isTRUE(odometer))    v <- 1:3
       pos <- pathRelPos(topo[, v])   # dist3D
       # FIXME: 1:3 or 1:2 ?????
       # pos <- pathRelPos(topo[, 1:3])   # dist3D
     }else if(!is.null(tt)){
       # mrk_time <- as.numeric(as.POSIXct(mrk$time))
       # mrk_pos <- pathRelPos(mrk[, c("x", "y")])
       # if(is.character(tt)){
       #   mrk <- as.numeric(as.POSIXct(tt))
       #   # mrk <- as.numeric(tt)
       # }
       if(is.character(tt)){
         mrk <- verboseF(as.numeric(tt), verbose = FALSE)
         if(all(is.na(mrk))){
          mrk <- as.POSIXct(tt)
         } 
         mrk <- as.numeric(mrk)
       }else{
          mrk <- as.numeric(tt)
       }
       
       pos <- pathRelPos(topo[, 1:2])
       # tr_time <- seq(from = mrk_time[1], to = tail(mrk_time, 1), 
       #                length.out = ntr)
       mrki <- seq(from = mrk[1], to = tail(mrk, 1), length.out = ntr)
       # intMeth <- ifelse(length(mrk_pos) > 2, method[1], "linear")
       # tr_pos <- signal::interp1(x = mrk_time, y = mrk_pos, xi = tr_time, 
       #                          method = "spline", extrap = NA)
     }else{
       id <- c(1, length(x))
       dstid <- c(0, pathLength(topo[, 1:2]))
       dst <- pathRelPos(topo[,1:2])
       pp <- approx(x    = c(0, pathLength(topo[, 1:2])),
                    y    = c(1, length(x)),
                    xout = pathRelPos(topo[,1:2]))$y
       mrki <- seq_along(x)
       mrk <- pp
       pos <- pathRelPos(topo[,1:2])
     }
     intMeth <- ifelse(length(pos) > 2, method[1], "linear")
     # dist3DInt
     posi   <- signal::interp1(x = mrk, 
                               y = pos, 
                               xi = mrki, 
                               method = intMeth , 
                               extrap = TRUE)
     # plot(mrki, posi)
     # points(mrk, pos, pch = 20)
     #--- INTERPOLATION dist3D ---#
     
     #--- INTERPOLATION N,E,Z ---#
     # A <- interp3DPath(x = topo[, 1:3], pos = topo[, 4], 
     #                   posi = seq_along(x@pos), r = r,
     #                   method = method)
     # colnames(A) <- c("x", "y", "z")
     ENZ <- matrix(nrow = length(posi), ncol = 3)
     intMeth <- ifelse(length(pos) > 2, method[2], "linear")
     for(i in 1:2){
       ENZ[, i] <- signal::interp1(pos, topo[, i], posi, 
                                   method = intMeth, extrap = NA)
       ENZ_NA <- is.na(ENZ[, i])
       ENZ[ENZ_NA, i] <- signal::interp1(pos, topo[, i],  posi[ENZ_NA], 
                                         method = "linear", extrap = TRUE)
     }
     if(is.null(r)){
       intMeth <- ifelse(length(pos) > 2, method[3], "linear")
       ENZ[, 3] <- signal::interp1(pos, topo[, 3], posi, 
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
     
     # add fiducial to indicate which points/traces were used for interpolation
     # x@fid[topo[,4]] <- paste0(x@fid[topo[,4]], " *")
     # fid(x) <- trimStr(fid(x))
     # diagnostic plots
     # dist3Dint <- pathRelPos(ENZ[, 1:2]) == posi
     if(verbose){
       message(x@name, ": mean dx = ", round( mean(diff(posi)), 3 ), 
               ", range dx = [", round( min(diff(posi)), 3 ),", ", 
               round( max(diff(posi)), 3 ),"]", myWarning)
     }
     if(plot == TRUE){
       op <- par(no.readonly=TRUE)
       par(mfrow=c(1, 3))
       plot(mrki, posi, pch = 20, col = "red", cex = 2, asp = NA,
            xlab = "trace number", ylab = "trace spacing (3D)",
            #xlim = range(seq_len(ntr)), ylim = range(posi), 
            main = paste( x@name))
       points(mrk, pos, pch = 20, col = "blue", cex = 0.6)
       plot(posi, ENZ[, 3], type = "l", asp = 10, 
            xlab = "interpolated trace spacing", 
            ylab = "interpolated elevation",
            main = paste0("range dx = [", round(min(diff(posi)), 2), 
                          ", ", round(max(diff(posi)), 2), "]"))
       points(pos, topo[,3], pch = 20, col = "red")
       plot(topo[, c(1,2)], col = 1, type = "l", lwd = 2, asp = 1,
            ylim = range(ENZ[, 2]), xlim = range(ENZ[, 1]), 
            main = paste0("mean dx=", round(mean(diff(posi)),2)))
       points(topo[, c(1,2)], col = 1, pch = 20, cex = 2)
       lines(ENZ[, 1], ENZ[, 2], col = 2, lwd = 1)
       Sys.sleep(1)
       par(op)
     }
   }
   # x@x <- posi - posi[1]
   # x@coord <- ENZ
   coord(x) <- ENZ
   # if(isFALSE(projToUTM) && !is.null(topo_crs) && is.null(CRSout)){
   #   #ENZ[,1:2] <- UTMTolonlat(ENZ[,1:2], CRSobj = topo_crs)
   #   # topo_crs <- sp::CRS("+init=epsg:4326")
   #   CRSout <- sp::CRS("+init=epsg:4326")
   # }
   # if(!is.null(topo_crs)){
   #   crs(x) <- topo_crs
   #   # x@crs    <- .checkCRS(topo_crs)
   #   # x@spunit <- crsUnit(x@crs)
   #   # x@proc   <- c(x@proc, "crs<-")
   #   # x@crs <- topo_crs
   #   # x@spunit <- crsUnit(topo_crs)
   # }
   if(!is.null(CRSout)){
     x <- spProjectToCRS(x, CRSout)
   }
   # else{
   #   # FIXME -> update x@x
   #   x@x <- pathRelPos(ENZ[,1:2])
   # }
   x@proc <- c(x@proc, "spInterp")
  return(x)
})

# remove rows of x with duplicated element in v
# length(v) == nrow(x)
# use the R-base function 'duplicated()'
.rmRowDuplicates <- function(x, v){
  iRM <- which(duplicated(v))
  if(length(iRM) > 0){
    return( x[-iRM, ])
  }else{
    return(x)
  }
}

# dim(topo) n x 4
.rmTraceIDDuplicates <- function(x, topo, tol = NULL,
                                 verbose = TRUE){
  if(is.null(tol)) tol <- .Machine$double.eps
  # topo <- mrk
  dist2D <- pathRelPos(topo[, 1:2])
  tbdltd <- which(abs(diff(dist2D)) < tol)   # to be deleted
  #if(length(tbdltd) > 0){
  # nb_tr_topo <- 0
  nb_tr_x <- 0
  dtr_id <- c()  # deleted trace ID
  while(length(tbdltd) > 0){    # add
    for(i in seq_along(tbdltd)){
      # i <- 1
      # number of trace to remove
      dtr <- topo[tbdltd[i] + 1 , 4] - topo[tbdltd[i], 4]
      # traces to remove (ID)
      w <- topo[tbdltd[i], 4] + seq_len(dtr)
      # remove trace in x
      x <- x[, -w]
      # remove traces in topo
      topo <- topo[-(tbdltd[i] + 1), ]
      # and actualise the trace numbers of the trace above the delete trace
      if(tbdltd[i] + 1 <= nrow(topo)){
        v <- (tbdltd[i] + 1):nrow(topo)
        # if(any(is.na(topo[v,]))) stop( "lkjlkj")
        topo[v, 4] <- topo[v, 4] -  dtr
      }
      tbdltd <- tbdltd - 1
      nb_tr_x <- nb_tr_x + dtr
      # nb_tr_topo <- 1 + nb_tr_topo
    }
    dist2D <- pathRelPos(topo[, 1:2])
    tbdltd <- which(abs(diff(dist2D)) < tol)
  }
  # if(nb_tr_topo > 0 && isTRUE(verbose)){
  if(nb_tr_x > 0 && isTRUE(verbose)){
    message(nb_tr_x, " trace(s) removed (duplicated trace position(s))" )
  }
  return(list(x = x, topo = topo))
}


#' @rdname spInterp
#' @export
setMethod("spInterp", "GPRsurvey", 
          function(x, topo, tt = NULL, r = NULL, 
                   lonlat = FALSE, projToUTM = FALSE, 
                   CRSin = NULL, CRSout = NULL,
                   odometer = FALSE,
                   tol = NULL, verbose = TRUE, plot = TRUE,
                   method = c("linear", "linear", "linear")){
            
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(topo,  msg, "LENGTH", length(x@names))
  checkArgStop(msg)
  if( any(sapply(topo, nrow) != x@nx)) stop("Elements in list must have correct dim")
  #------------------- end check
  
  for(i in seq_along(x)){
    z <- readGPR(x@paths[[i]])
    z <- spInterp(z, topo[[i]], 
                  tt        = tt[[i]], 
                  r         = r, 
                  lonlat    = lonlat, 
                  projToUTM = projToUTM, 
                  CRSout    = CRSout,
                  odometer  = odometer,
                  tol       = tol,
                  verbose   = verbose,
                  plot      = plot,
                  method    = method)
    x@coords[[i]] <- z@coord
    x@xlengths[i] <- pathLength(z@coord[ ,1:2])
    x@nx[i]       <- nrow(z@coord)
    x@paths[i]    <- .saveTempFile(z)
  }
  x@intersections <- list()
  # x <- coordref(x)
  x <- spIntersection(x)
  return(x)
})


#   
# # x = topo[, c("E", "N", "Z")] or C("x", "y", "z")
# # dist3D <- pathRelPos(x, last = FALSE)
# # posi <- seq_along(x@pos)
# # pos <- topo[, "TRACE"]
# # r
# interp3DPath <- function(x, pos, posi, r = NULL,
#                          method = c("linear", "spline", "pchip")){
#   if(ncol(x) > 3) x <- x[, 1:3]
#   dist3D <- pathRelPos(x)
#   #--- INTERPOLATION dist3D ---#
#   intMeth <- ifelse(length(dist3D) > 2, method[1], "linear")
#   dist3DInt   <- signal::interp1(pos, dist3D, posi, 
#                                  method = intMeth , extrap=TRUE)
#   #--- INTERPOLATION N,E,Z ---#
#   ENZ <- matrix(nrow = length(posi), ncol = 3)
#   intMeth <- ifelse(length(dist3D) > 2, method[2], "linear")
#   ENZ[, 1] <- signal::interp1(dist3D, x[, 1], dist3DInt, 
#                               method = intMeth, extrap = NA)
#   ENA <- is.na(ENZ[, 1])
#   ENZ[ENA, 1] <- signal::interp1(dist3D, x[, 1], dist3DInt[ENA], 
#                                  method = "linear", extrap = TRUE)
#   ENZ[, 2] <- signal::interp1(dist3D, x[, 2], dist3DInt, 
#                               method = intMeth , extrap = NA)
#   ZNA <- is.na(ENZ[, 2])
#   ENZ[ZNA, 2] <- signal::interp1(dist3D, x[, 2], dist3DInt[ZNA], 
#                                  method = "linear", extrap = TRUE)
#   if(is.null(r) && ncol(x) == 3){
#     intMeth <- ifelse(length(dist3D) > 2, method[3], "linear")
#     ENZ[, 3] <- signal::interp1(dist3D, x[, 3], dist3DInt, 
#                                 method = intMeth, extrap = NA)
#   }else if(!is.null(r)){
#     ENZ[, 3] <- raster::extract(r, cbind(ENZ[, 1], ENZ[, 2]), 
#                                 method = "bilinear")
#   } 
#   lastNA  <- max(which(!is.na(ENZ[, 3])))
#   firstNA <- min(which(!is.na(ENZ[, 3])))
#   if(firstNA > 1){
#     ENZ[1:(firstNA - 1), 3] <- ENZ[firstNA, 3]
#   } 
#   if(lastNA < length(ENZ[, 3])){
#     ENZ[(lastNA + 1):length(ENZ[, 3]), 3]<- ENZ[lastNA, 3]
#   }
#   # message(x@name, ": mean dx = ", round(mean(diff(dist3DInt)), 3), 
#   #         "  range dx = ",round(min(diff(dist3DInt)), 3)," - ", 
#   #         round(max(diff(dist3DInt)), 3))
#   return(ENZ)
# }

# # Interpolate GPR coordinates from GPS data (GPGGA string) data
# #
# # @param x Object of the class GPR
# # @param gpgga output of the function "readGPGGA()"
# #@name interpPosFromGPGGA 
# # @rdname interpPosFromGPGGA
# # @export
# interpPosFromGPGGA <- function(ntr, GPGGA, tol = NULL, backproject = TRUE){
#   
#   mrk0 <- GPGGA
#   
#   #--- Convert to UTM
#   tr_crs <-  llToUTM(lat = median(sp::coordinates(mrk0)[,2]), 
#                      lon = median(sp::coordinates(mrk0)[,1]), 
#                      zone = NULL, south = NULL)$crs
#   mrk <- as.data.frame(sp::spTransform(mrk0, tr_crs))
#   
#   
#   #---- 4. remove duplicates
#   dist2D <- pathRelPos(mrk[, c("x", "y")])
#   # in 'x' and 'mrk'
#   if(is.null(tol))  tol <- sqrt(.Machine$double.eps)
#   tbdltd <- which(abs(diff(dist2D)) < tol)
#   while(length(tbdltd) > 0){
#     mrk <- mrk[ -(tbdltd + 1), ]
#     dist2D <- pathRelPos(mrk[, c("x", "y", "z")]) # mod
#     tbdltd <- which(abs(diff(dist2D)) < tol)
#   }
#   
#   #--- Interpolate trace position
#   mrk_time <- as.numeric(as.POSIXct(mrk$time))
#   mrk_pos <- pathRelPos(mrk[,c("x", "y")])
#   tr_time <- seq(from = mrk_time[1], to = tail(mrk_time, 1), 
#                  length.out = ntr)
#   tr_pos <- signal::interp1(x = mrk_time, y = mrk_pos, xi = tr_time, 
#                             method = "spline", extrap = NA)
#   
#   
#   # plot(mrk_pos, mrk_time)
#   # points(tr_pos, tr_time, pch = 20)
#   # check: compare with GPR data -> OK
#   # plot(tr_pos, pathRelPos(coord(x[[1]])[,1:2]))
#   
#   #--- Interpolate trace coordinates
#   tr_xyz <- matrix(0, nrow = ntr, ncol = 3)
#   tr_xyz[, 1] <- signal::interp1(x = mrk_pos, y = mrk$x, xi = tr_pos, 
#                                  method = "spline", extrap = NA)
#   tr_xyz[, 2] <- signal::interp1(x = mrk_pos, y = mrk$y, xi = tr_pos, 
#                                  method = "spline", extrap = NA)
#   tr_xyz[, 3] <- signal::interp1(x = mrk_pos, y = mrk$z, xi = tr_pos, 
#                                  method = "spline", extrap = NA)
#   
#   # plot(tr_x, tr_y, pch = 20)
#   # 
#   # 
#   # plot(tr_pos, tr_z)
#   # points(mrk_pos, mrk$z, pch = 20, col = "red")
#   
#   
#   if(backproject == TRUE){
#     tr_xyz[,1:2] <- UTMToll(xy = tr_xyz[,1:2], xy_crs = tr_crs)
#     tr_crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
#   }
#   
#   # utr <- sf::st_as_sf(as.data.frame(uu), coords = c(1,2))
#   # plot(sf::st_coordinates(xyz), asp = 1, type = "l")
#   # points(sf::st_coordinates(utr), pch = 20)
#   # plot(utr, add = TRUE, pch = 20)
#   
#   # coord(x) <- tr_xyz
#   # crs(x) <- mrk_crs
#   # x@proc <- c(x@proc, "interpPosFromGPGGA")
#   return(list(x = tr_xyz, crs = tr_crs))
# }


# Interpolate GPR coordinates from geoJSON data
#
# @param x Object of the class GPR
# @param geojson Either a geojson string or a filepath pointing to a 
#                geojson file
# @export
# interpPosFromGeoJSON <- function(x, geojson, tol = NULL, backproject = TRUE){
#   #---- 1. Read geoJSON
#   xyz <- geojsonsf::geojson_sf(geojson)
#   # sf::st_crs(xyz)
#   
#   #---- 2. convert to UTM
#   XY <- sf::st_coordinates(xyz)
#   XY <- XY[XY[, "L1"] == XY[1, "L1"], ]       # take the first structure
#   u <- llToUTM(lat = XY[,2], lon = XY[,1])
#   #plot(u$xy, type = "l", asp = 1)
#   
#   #---- 3. create "topo" file
#   mrk <- cbind(u$xy, 0, NA)
#   colnames(mrk) <- c("x", "y", "z", "tn")
#   
#   #---- 4. remove duplicates
#   dist2D <- pathRelPos(mrk[, c("x", "y")])
#   # in 'x' and 'mrk'
#   if(is.null(tol))  tol <- sqrt(.Machine$double.eps)
#   tbdltd <- which(abs(diff(dist2D)) < tol)
#   while(length(tbdltd) > 0){
#     mrk <- mrk[ -(tbdltd + 1), ]
#     dist2D <- pathRelPos(mrk[, c("x", "y", "z")]) # mod
#     tbdltd <- which(abs(diff(dist2D)) < tol)
#   }
#   
#   #---- 5. trace interpolation
#   # a) interpolate shape points to traces
#   trFIDPos <- approx(x = c(0, tail(dist2D, 1)),
#                      y = c(1, length(x)),
#                      xout = dist2D)
#   
#   # b) interpolate coordinates
#   tr_xyz <- matrix(0, nrow = ncol(x), ncol = 3)
#   tx_x <- approx(x = trFIDPos$y,
#                  y = mrk[,"x"],
#                  xout = seq_along(x))
#   tx_y <- approx(x = trFIDPos$y,
#                  y = mrk[,"y"],
#                  xout = seq_along(x))
#   tx_z <- approx(x = trFIDPos$y,
#                  y = mrk[,"z"],
#                  xout = seq_along(x))
#   tr_xyz[,1] <- tx_x$y
#   tr_xyz[,2] <- tx_y$y
#   tr_xyz[,3] <- tx_z$y
#   # plot(u$xy, type = "l", asp = 1)
#   # lines(tx_x$y, tx_y$y, type = "p")
#   
#   if(backproject == TRUE){
#     tr_xyz[,1:2] <- UTMToll(xy = tr_xyz[,1:2], xy_crs = u$crs)
#   }
#   
#   # utr <- sf::st_as_sf(as.data.frame(uu), coords = c(1,2))
#   # plot(sf::st_coordinates(xyz), asp = 1, type = "l")
#   # points(sf::st_coordinates(utr), pch = 20)
#   # plot(utr, add = TRUE, pch = 20)
#   
#   return(tr_xyz)
# }
# 
