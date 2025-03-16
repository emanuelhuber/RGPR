## FIXME: change parameter name "coords"


#' Interpolate trace positions from measurement (e.g., GPS).
#' 
#' Update slot `x@x`!
#'
#' @param x      (`GPR|GRPsurvey`)
#' @param coords `matrix|sf::sf|list`: A numeric matrix with at least
#'             3 columns (the (x,y,z)-coordinates) and optionally a fourth
#'             column containing the trace number (id) or `NA` if
#'             for some coordinates there is no link to the trace number;
#'             or a simple feature from the `sf` package of `POINT`
#'             geometry type (one point per coordinates). If there are some
#'             attributes, it is assumed that the first one corresponds to the
#'             trace number (id). 
#'             Use `sf::st_cast(..., "POINT")` to convert a 
#'             `LINESTRING` simple feature to a 
#'             `point` simple feature.
#'             If `x` is a `GPRsurvey` `coords` is a list of either
#'             matrix or simple featue as previously defined.
#' @param tt (`numeric|list`) A numeric vector or a list of vectors  
#'           (if `x` is a `GPRsurvey` object) corresponding to the
#'           trace recording time (e.g., output GPS device). If provided,
#'           it will be assumed that the traces were recorded at regular
#'           time interval (the 4th column of `coords` will be ignored).
#' @param r (`terra::SpatRaster`)  A `SpatRaster` 
#'          object from the package 
#'          `terra` from which trace elevation `z` will be extracted 
#'          based on the trace position `(x, y)` on the raster. 
#'          The extracted trace elevation will overwrite the values from the 
#'          third column of `coords`.
#' @param UTM (`logical[1]|character[1]`) If `TRUE` it is assumed 
#'            that the coordinates are in the in geographic (longitude/latitude) 
#'            coordinate reference system (WGS84) and the coordinates are
#'            projected into the guessed UTM WGS84 coordinate reference 
#'            system (RGPR guesses the UTM zone based on the coordinates).
#'            If `UTM` is a character string corresponding to a UTM zone
#'            (for example `UTM = "32N"`), it is assumed 
#'            that the coordinates are in the in geographic (longitude/latitude) 
#'            coordinate reference system (WGS84) and the coordinates are
#'            projected into the provided UTM zone. Note that only `N` 
#'            and `S` are allowed after the zone number 
#'            (for example, `"31X"` will be not recognized).
#' @param interp3D (`logical[1]`) If `interp3D = TRUE` it is 
#'                 assumed that data recording was triggered by an interp3D.
#'                 In this case, the coordinate interpolation is based on the
#'                 3D distance between the coordinates (x,y,z). 
#'                 If `interp3D = FALSE` (e.g. GPS data) a 2D distance
#'                 (x, y) is used.
#' @param tol  [`numeric[1]`]     Length-one numeric vector: if the horizontal distance between 
#'               two consecutive trace positions is smaller than `tol`, 
#'               then the traces in between as well as the second trace
#'               position are removed.
#'               If `tol = NULL`, `tol` is set equal to
#'               `sqrt(.Machine$double.eps)`.
#' @param verbose (`logical[1]`) If `FALSE`, all messages and warnings are
#'                suppressed (use with care).
#' @param plot (`logical[1]`) If `TRUE` some control/diagnostic 
#'             plots are displayed.
#' @param method (`character[3]`) The interpolation
#'               methods:
#'               First element for the interpolation of the 
#'               inter-trace distances, 
#'               second element for the interpolation of the horizontal 
#'               trace positions, and third element for the interpolation
#'               of the vertical trace positions.
#'               The methods that can be used are 
#'               `linear`, `nearest`, `pchip`, `cubic`, 
#'               and `spline`
#'                (same methods as in \code{[signal]{interp1}}.
#' @return x with interpolated trace positions.
#' @name interpCoords
#' @concept spatial computing
setGeneric("interpCoords", function(x, coords, 
                                    tt = NULL, r = NULL, 
                                    UTM = FALSE, 
                                    interp3D = FALSE,
                                    tol = NULL, verbose = TRUE, plot = TRUE,
                                    method = c("linear", "linear", "linear"))
  standardGeneric("interpCoords"))


#' @rdname interpCoords
#' @export
setMethod("interpCoords", "GPR", 
 function(x, coords, 
          tt = NULL, r = NULL, 
          UTM = FALSE, 
          interp3D = FALSE,
          tol = NULL, verbose = TRUE, plot = TRUE,
          method = c("linear", "linear", "linear")){
  
   # FIXME > do some checks on args
   if(is.null(tol)) tol <- sqrt(.Machine$double.eps)

   if(inherits(coords, "sf")){
     crs(x) <- sf::st_crs(coords)
     if(sf::st_crs(coords)$epsg != 4326) UTM <- FALSE
     coordssf <- sf::st_coordinates(coords)
     # if 2D coordinates
     if(ncol(coordssf) == 2) coordssf <- cbind(coordssf, 0)
     coords <- cbind(coordssf, 
                   sf::st_drop_geometry(coords))
     # FIXME apply here the UTM transform
   }
   #--------------------- CRS TRANSFORM ----------------------------------------
   if(isTRUE(UTM) ){
     coords_utm <- lonLatToUTM(lon = coords[, 1], lat = coords[, 2])
     if(any(is.na(coords_utm$xy))){
       stop("The conversion to UTM did not work!\n",
            "try setting 'UTM = FALSE'.")
     }
     crs(x) <- coords_utm$crs
     coords[, 1:2] <- coords_utm$xy
   }else if(is.character(UTM)){
     crs(x) <- paste0("EPSG:", UMTStringToEPSG(UTM))
     coords[, 1:2] <- sf::sf_project(from = "EPSG:4326", 
                                   to   = crs(x), 
                                   pts  = coords[, 1:2])
   }
   
   # 3 Cases:
   #   - case 1: (x, y, z, tr) trace markers
   #   - case 2: (x, y, z, t) time (signal recording at regular time interval)
   #   - case 3: (x, y, z) only coordinates
   
   # Duplicated trace positions -> remove trace?
   
   if(ncol(coords) >= 4 && all(is.na(coords[,4]))){
     # -> case 3
     warning(x@name, ": no link between the measured points",
             " and the GPR traces!")
     coords <- coords[, 1:3]
   }
   if(ncol(coords) < 3){
     stop("'coords' must have at least 3 columns (x, y, z)!")
   }
   
   #----- REMOVE UNUSABLE TRACE MARKERS ----------------------------------------#
   # case 1
   if(ncol(coords) >=  4 && is.null(tt)){
     # if we have measured some points before the line start or 
     # after the end of the GPR Line, we delete them
     test <- which(!is.na(coords[, 4]))
     coords <- coords[min(test):max(test), ]
     coords[, 4] <- as.integer(coords[,4])
     # keep only the markers corresponding to existing traces
     coords <- coords[coords[, 4] > 0 & coords[, 4] <= ncol(x), ]
     
     #----- REMOVE DUPLICATED TRACE ID IN coords ---------------------------------#
     coords <- .rmRowDuplicates(coords, coords[, 4])
     # order coords by increasing trace id
     coords <- coords[order(coords[, 4]), ]
     #----- REMOVE DUPLICATED TRACES (TRACES WITH (ALMOST) SAME POSITIONS) -----#
     xx <- .rmTraceIDDuplicates(x, coords, tol = tol, verbose = verbose)
     x <- xx$x
     coords <- xx$coords
   }else{
     coords <- dropDuplicatedCoords(coords, tol = tol, z = interp3D, verbose = TRUE)
     # dist2D <- pathRelPos(coords[, 1:2])
     # tbdltd <- which(abs(diff(dist2D)) < tol) # to be deleted
     # while(length(tbdltd) > 0){
     #   coords <- coords[ -(tbdltd + 1), ]
     #   dist2D <- pathRelPos(coords[, 1:2]) # mod
     #   tbdltd <- which(abs(diff(dist2D)) < tol)
     # }
   }
   #----- IF RASTER -> EXTRACT POSITION ELEVATION FROM RASTER ------------------#
   if(!is.null(r)){
     # coords[,3] <- raster::extract(r, coords[, 1:2], method = "bilinear")
     coords[,3] <- terra::extract(r, coords[, 1:2], method = "bilinear")
     if(sum(is.na(coords[, 3])) > 0){
       stop("'extract(r, coords[, c(\"E\",\"N\")], method = \"bilinear\")' ",
            "returns 'NA' values!\n", 
            "Not all GPR positions fall within raster extent or\n",
            "there are 'NA' values in raster 'r'!")
     }
   }
   ntr <- ncol(x)
   myWarning <- ""
   
   #----- INTERPOLATE MARKER POSITION THAT ARE NOT LINKED TO A TRACE -----------#
   if(ncol(coords) >= 4 && is.null(tt)){
     # if there are points measured with the total station
     # that do not have an fiducial (FID) > interpolate them!
     if(anyNA(coords[,4])){
       # dist3D[coords$PNAME %in% FID$PNAME] > distance for the points 
       # also recorded in FID
       myWarning <- "\npoints total station without fiducials"
       dist3D <- pathRelPos(coords[, 1:3])
       test <- !is.na(coords[,4])
       intMeth <- ifelse(sum(test) > 2, method[1], "linear")
       traceNb <- signal::interp1(x      = dist3D[test], 
                                  y      = coords[test, 4], 
                                  xi     = dist3D,
                                  method = intMeth,
                                  extrap = TRUE)
       coords[, 4] <- as.integer(round(traceNb))
       # check for duplicates in TRACE ID and remove them!
       coords <- .rmRowDuplicates(coords, coords[, 4])
     }
   }
   #----- IF THERE IS NOTHING TO INTERPOLATE (= 1 MARKER PER TRACE) ------------#
   if( ncol(coords) >= 4 && all(seq_len(ntr) %in% coords[, 4]) ){
     ENZ <- as.matrix(coords[coords[, 4] %in% seq_along(x@pos), 1:3])
     message("No interpolation required because the trace positions\n",
             "of all GPR traces is already available (in object 'coords')!")
   }else{
     #--- PRE-INTERPOLATION ---#
     if(ncol(coords) >=  4 && is.null(tt)){
       mrk <- coords[, 4]           # pos
       mrki <- seq_len(ntr)       # posi
       v <- 1:2
       if(isTRUE(interp3D))    v <- 1:3
       pos <- pathRelPos(coords[, v])   # dist3D
     }else if(!is.null(tt)){
       if(is.character(tt)){
         mrk <- verboseF(as.numeric(tt), verbose = FALSE)
         if(all(is.na(mrk))){
          mrk <- as.POSIXct(tt)
         } 
         mrk <- as.numeric(mrk)
       }else{
          mrk <- as.numeric(tt)
       }
       
       pos <- pathRelPos(coords[, 1:2])
       mrki <- seq(from = mrk[1], to = tail(mrk, 1), length.out = ntr)
     }else{
       id <- c(1, length(x))
       dstid <- c(0, pathLength(coords[, 1:2]))
       dst <- pathRelPos(coords[,1:2])
       pp <- approx(x    = c(0, pathLength(coords[, 1:2])),
                    y    = c(1, length(x)),
                    xout = pathRelPos(coords[,1:2]))$y
       mrki <- seq_along(x)
       mrk <- pp
       pos <- pathRelPos(coords[,1:2])
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
     # A <- interp3DPath(x = coords[, 1:3], pos = coords[, 4], 
     #                   posi = seq_along(x@pos), r = r,
     #                   method = method)
     # colnames(A) <- c("x", "y", "z")
     ENZ <- matrix(nrow = length(posi), ncol = 3)
     intMeth <- ifelse(length(pos) > 2, method[2], "linear")
     for(i in 1:2){
       ENZ[, i] <- signal::interp1(pos, coords[, i], posi, 
                                   method = intMeth, extrap = NA)
       ENZ_NA <- is.na(ENZ[, i])
       ENZ[ENZ_NA, i] <- signal::interp1(pos, coords[, i],  posi[ENZ_NA], 
                                         method = "linear", extrap = TRUE)
     }
     if(is.null(r)){
       intMeth <- ifelse(length(pos) > 2, method[3], "linear")
       ENZ[, 3] <- signal::interp1(pos, coords[, 3], posi, 
                                   method = intMeth, extrap = NA)
     }else if(!is.null(r)){
       # ENZ[, 3] <- raster::extract(r, cbind(ENZ[, 1], ENZ[, 2]), 
       #                             method = "bilinear")
       ENZ[, 3] <- terra::extract(r, 
                                  cbind(ENZ[, 1], ENZ[, 2]), 
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
     
     # FIXME add fiducial to indicate which points/traces were used for interpolation
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
       points(pos, coords[,3], pch = 20, col = "red")
       plot(coords[, c(1,2)], col = 1, type = "l", lwd = 2, asp = 1,
            ylim = range(ENZ[, 2]), xlim = range(ENZ[, 1]), 
            main = paste0("mean dx=", round(mean(diff(posi)),2)))
       points(coords[, c(1,2)], col = 1, pch = 20, cex = 2)
       lines(ENZ[, 1], ENZ[, 2], col = 2, lwd = 1)
       Sys.sleep(1)
       par(op)
     }
   }
   # coord(x) <- ENZ
   colnames(ENZ) <- c("x", "y", "z")
   x@coord <- ENZ
   
   x@x <- relPos(x)

   # if(!is.null(CRSout)){
   #   x <- verboseF(project(x, CRSout), FALSE)
   # }
   x@proc <- c(x@proc, "interpCoords")
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

# dim(coords) n x 4
# check that: https://stackoverflow.com/a/34924247
# round the coordinates to the tol value and apply duplicated?
.rmTraceIDDuplicates <- function(x, coords, tol = NULL, z = FALSE,
                                 verbose = TRUE){
  j <- seq_len(ncol(x))
  j2 <- rep(FALSE, ncol(x))
  i <- .dropDuplicatedCoords(coords, tol = tol, z = z, verbose = verbose)
  if(all(sapply(i, isFALSE))){
    return(list(x = x, coords = coords))
  }
  ids_notD <- as.integer(coords[!i, 4])  # id of not duplicates
  j2[j %in% ids_notD] <- TRUE
  # j2[!i]
  # coords[!i, 4]
  
  ids <- as.integer(coords[i, 4])  # id of duplicates
  
  new_ids <- which(j2[-ids])   # id of not duplicates
  
  x <- x[, -ids]
  coords <- coords[!i, ]
  coords[, 4] <- new_ids
  if(verbose){
    message(sum(i), " duplicated trace(s) removed from 'x'!")
  }
  # if(is.null(tol)) tol <- .Machine$double.eps
  # dist2D <- pathRelPos(coords[, 1:2])
  # tbdltd <- which(abs(diff(dist2D)) < tol)   # to be deleted
  # #if(length(tbdltd) > 0){
  # # nb_tr_coords <- 0
  # nb_tr_x <- 0
  # dtr_id <- c()  # deleted trace ID
  # while(length(tbdltd) > 0){    # add
  #   for(i in seq_along(tbdltd)){
  #     # i <- 1
  #     # number of trace to remove
  #     dtr <- coords[tbdltd[i] + 1 , 4] - coords[tbdltd[i], 4]
  #     # traces to remove (ID)
  #     w <- coords[tbdltd[i], 4] + seq_len(dtr)
  #     # remove trace in x
  #     x <- x[, -w]
  #     # remove traces in coords
  #     coords <- coords[-(tbdltd[i] + 1), ]
  #     # and actualise the trace numbers of the trace above the delete trace
  #     if(tbdltd[i] + 1 <= nrow(coords)){
  #       v <- (tbdltd[i] + 1):nrow(coords)
  #       # if(any(is.na(coords[v,]))) stop( "lkjlkj")
  #       coords[v, 4] <- coords[v, 4] -  dtr
  #     }
  #     tbdltd <- tbdltd - 1
  #     nb_tr_x <- nb_tr_x + dtr
  #     # nb_tr_coords <- 1 + nb_tr_coords
  #   }
  #   dist2D <- pathRelPos(coords[, 1:2])
  #   tbdltd <- which(abs(diff(dist2D)) < tol)
  # }
  # # if(nb_tr_coords > 0 && isTRUE(verbose)){
  # if(nb_tr_x > 0 && isTRUE(verbose)){
  #   message(nb_tr_x, " trace(s) removed (duplicated trace position(s))" )
  # }
  return(list(x = x, coords = coords))
}


#' @rdname interpCoords
#' @export
setMethod("interpCoords", "GPRsurvey", 
          function(x, coords, tt = NULL, r = NULL, 
                   UTM = FALSE, 
                   interp3D = FALSE,
                   tol = NULL, verbose = TRUE, plot = TRUE,
                   method = c("linear", "linear", "linear")){
            
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(coords,  msg, "LENGTH", length(x@names))
  checkArgStop(msg)
  if( any(sapply(coords, nrow) != x@nx)) stop("Elements in list must have correct dim")
  #------------------- end check
  
  for(i in seq_along(x)){
    z <- readGPR(x@paths[[i]])
    z <- interpCoords(z, coords[[i]], 
                  tt        = tt[[i]], 
                  r         = r, 
                  UTM = UTM, 
                  interp3D  = interp3D,
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
  x <- intersect(x)
  return(x)
})


#   
# # x = coords[, c("E", "N", "Z")] or C("x", "y", "z")
# # dist3D <- pathRelPos(x, last = FALSE)
# # posi <- seq_along(x@pos)
# # pos <- coords[, "TRACE"]
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
#   #---- 3. create "coords" file
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
