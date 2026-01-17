
#' Interpolate horizontal slices
#' 
#' 
#' Interpolate horizontal slices
#' @param obj (`GPRsurvey`)
#' @param dx (`numeric[1]`) x-resolution
#' @param dy (`numeric[1]`) y-resolution
#' @param dz (`numeric[1]`) z-resolution
#' @param h (`numeric[1]`) FIXME: Number of levels in MBA hierarchy (see function...)
#' @param extend (`character[1]`) FIXME: Method to define interpolation extent.
#' @param buffer (`numeric[1]`) FIXME: Buffer distance around survey lines.
#' @param shp (`matrix[n,2]|list[2]|sf`) FIXME: Shape/polygon defining interpolation bounds.
#' @param rot (`logical[1]|numeric[1]`) If `TRUE` the GPR lines are 
#'            fist rotated such to minimise their axis-aligned bounding box. 
#'            If `rot` is numeric, the GPR lines is rotated first
#'            rotated by `rot` (in radian).
#' @param verbose (`logical[1]`) If TRUE, verbose.
#' @return (`GPRcube|GPRslice`)
#' @name interpSlices
#' @rdname interpSlices
#' @export
#' @concept 3D
setGeneric("interpSlices", function(obj, 
                                    dx = NULL, 
                                    dy = NULL, 
                                    dz = NULL, 
                                    h = 6,
                                    extend = c("bbox", "obbox", "chull", "buffer"),
                                    buffer = NULL,
                                    shp = NULL,
                                    rot = FALSE, verbose = TRUE) 
  standardGeneric("interpSlices"))

#' @rdname interpSlices
#' @export
setMethod("interpSlices", "GPRsurvey", function(obj, 
                                                dx = NULL, 
                                                dy = NULL, 
                                                dz = NULL, 
                                                h = 6,
                                                extend = c("bbox", "obbox", "chull", "buffer"),
                                                buffer = NULL,
                                                shp = NULL,
                                                rot = FALSE,
                                                verbose = TRUE){
  
  
  extend = match.arg(extend, c("bbox", "obbox", "chull", "buffer"))
  test <- sapply(obj@coords, function(x) length(x) > 0)
  
  if(all(!test)){
    stop("Some of the data have no coordinates.\n", 
         " Please set first coordinates to all data,\n",
         " or remove these data!")
  }
  if( length(unique(obj@spunit)) > 1 ){
    stop("Position units are not identical: \n",
         paste0(unique(obj@spunit), collapse = ", "), "!")
  }
  if(length(unique(obj@zunits)) > 1){
    stop("Depth units are not identical: \n",
         paste0(unique(obj@zunits), collapse = ", "), "!\n")
  }
  stopifnot(length(obj@coords) == length(obj@zlengths))
  
  if(is.null(dx)){
    dx <- mean(obj@xlengths/(obj@nx - 1))
  }
  if(is.null(dy)){
    dy <- dx
  }
  if(is.null(dz)){
    dz <- mean(obj@zlengths/(obj@nz - 1))
  }
  
  
  if(isTRUE(rot)){
    obj <- georef(obj)
    x_rot <- obj@transf[5]
  }else if(!isFALSE(rot)){
    if(is.numeric(rot)){
      x_rot <- rot
      obj <- georef(obj, alpha = x_rot)
    }else{
      stop("'rot' must be either TRUE/FALSE or numeric")
    }
  }else{
    x_rot <- 0
  }
  
  SXY <- .sliceInterp(obj = obj[test], dx = dx, dy = dy, dz = dz, h = h,
                      extend = extend,
                      buffer = buffer,
                      shp = shp, verbose = verbose)
  
  xyref <- c(min(SXY$x), min(SXY$y), SXY$vz[1])
  # xpos <- SXY$x #- min(SXY$x)
  # ypos <- SXY$y #- min(SXY$y)
  
  xfreq <- ifelse(length(unique(obj@freqs[test])) == 1, obj@freqs[test][1], numeric(0))
  
  if(dim(SXY$z)[3] == 1){
    class_name <- "GPRslice"
    ddata <- SXY$z[,,1]
  }else{
    class_name <- "GPRcube"
    ddata <- SXY$z
  }
  xtrsf <- numeric(0)
  if(length(obj@transf)>0) xtrsf <- c(obj@transf[1:2], x_rot)
  
  y <- new(class_name,
           #----------------- GPRvirtual --------------------------------------#
           version      = "0.3",
           name         = "",
           path         = obj@paths[test][1],
           desc         = "GPR cube",  # data description
           mode         = obj@modes[test][1],  # reflection/CMP/WARR (CMPAnalysis/spectrum/...)?
           date         = Sys.Date(),       # survey date (format %Y-%m-%d)
           freq         = xfreq,    # antenna frequency
           
           data         = ddata,      # data
           dunit        = "",         # data unit
           dlab         = "", 
           
           spunit       = obj@spunit[test][1],  # spatial unit
           crs          = obj@crs[test][1],  # coordinate reference system of @coord
           # ? coordref = "numeric",    # coordinates references or "center" or "centroid"
           
           xunit        = obj@spunit[test][1],  # horizontal unit
           xlab         = "",
           
           zunit        = obj@zunits[test][1],  # time/depth unit
           zlab         = "",
           
           # vel          = "list",   # velocity model (function of x, y, z)
           
           # proc         = "list",       # processing steps
           # delineations = "list",       # delineations
           # md           = "list",        # data from header file/meta-data
           #----------------- GPRcube -----------------------------------------#
           dx     = dx,   # xpos,
           dy     = dy,   # ypos,
           # FIXME: dz sign handling is confusing and fragile. 
           # In setMethod() you take dz * sign(mean(diff(SXY$vz))) — that stores 
           # a signed dz in the object. This is confusing for consumers of the 
           # class. Better to keep dz positive (the sampling spacing) and store 
           # orientation/direction by storing z0 (top) and vz explicitly (which 
           # you already do).
           dz     = dz * sign(mean(diff(SXY$vz))),   # SXY$vz,
           ylab   = "",   #,  # set names, length = 1|p
           
           center = xyref,    # coordinates grid corner bottom left (0,0)
           rot    = xtrsf #x_rot     # rotation angle
  )
  if(class_name == "GPRslice") y@z <- SXY$vz[1]
  return(y)
})


# # define z
# defVz <- function(obj){
#   if(all(isZDepth(obj)) && all(sapply(obj@coords, length) > 0)){
#     # elevation coordinates
#     zmax <- sapply(obj@coords, function(x) max(x[,3]))
#     zmin <- sapply(obj@coords, function(x) min(x[,3])) - max(obj@zlengths)
#     d_z  <- obj@zlengths/(obj@nz - 1)
#     vz   <- seq(from = min(zmin), to = max(zmax), by = min(d_z))
#   }else{
#     # time/depth
#     d_z <- obj@zlengths/(obj@nz - 1)
#     vz <- seq(from = 0, by = min(d_z), length.out = max(obj@nz))
#   }
#   return(vz)
# }



#' Interpolate trace to target depths/times
#' 
#' @param x Amplitude values
#' @param z Depth/time values
#' @param zi Target depth/time values for interpolation
#' @return Interpolated amplitudes at zi
#' @noRd
# x = amplitude
# z = time/depth
# zi = time/depth at which to interpolate
trInterp <- function(x, z, zi){
  # isNA <- is.na(x)
  # xi <- signal::interp1(x = z[!isNA], y = x[!isNA], xi = zi, method = "spline", 
  #                       extrap = 0)
  # NOT: with method = "pchip" troubles because of Non-Monotonic Z Values
  xi <- signal::interp1(x = z, y = x, xi = zi, method = "spline", extrap = 0)
  return(xi)
}

# 
# .sliceInterp <- function(obj, dx = NULL, dy = NULL, dz = NULL, h = 6,
#                          extend = "bbox", buffer = NULL, shp =  NULL, m = 1, n = 1){
#   
#   
#   
#   # target depth vector
#   if(all(isZDepth(obj))){
#     zmax <- max(sapply(obj@coords, function(x) max(x[,3])))
#     zmin <- min(mapply(function(x, y) min(x[,3] - y), obj@coords, obj@zlengths))
#     vz <- seq(zmax, to = zmin, by = -dz)
#   }else{
#     vz <- seq(from = 0, by = dz, to = max(obj@zlengths))
#   }
#   
#   
#   V <- matrix(0, nrow = length(vz), ncol = sum(obj@nx))
#   posi <- 0
#   for(i in seq_along(obj)){
#     OBJI <- obj[[i]]
#     OBJI@data[is.na(OBJI@data)] <- 0
#     idx <- posi + (1:ncol(OBJI))
#     if(isZDepth(OBJI)){
#       if(length(unique(OBJI@coord[,3])) > 1){
#         # z values accounting for topography
#         Z <- matrix(OBJI@coord[, 3], nrow = nrow(OBJI), ncol = ncol(OBJI), byrow = TRUE) - 
#           matrix(OBJI@z, nrow = nrow(OBJI), ncol = ncol(OBJI))
#         V[, idx] <- vapply(seq_len(ncol(OBJI@data)),
#                function(j) trInterp(OBJI@data[, j], Z[, j], vz),
#                numeric(length(vz)))
#       }else{
#         V[, idx] <- apply(OBJI@data, 2, trInterp, 
#                           z = OBJI@coord[1,3] - OBJI@z, 
#                           zi = vz )
#       } 
#     }else{
#       V[, idx] <- apply(OBJI@data, 2, trInterp, 
#                         z = OBJI@z, 
#                         zi = vz )
#     }
#     posi <- posi + ncol(OBJI)
#   }
#   
# 
#   # positions obervations
#   xypos <- do.call(rbind, obj@coords)
#   # xpos <- unlist(lapply(obj@coords, function(x) x[,1]))
#   # ypos <- unlist(lapply(obj@coords, function(x) x[,2]))
#   
#   x_shp <- obj
#   if(!is.null(shp)){
#     if(inherits(shp, "sf") || inherits(shp, "sfc") || inherits(shp, "sfg")){
#       x_shp <- sf::st_coordinates(shp)[,1:2]
#     }else if(is.list(shp)){
#       x_shp <- cbind(shp[[1]], shp[[2]])
#     }else{
#       x_shp <- shp
#     }
#   }
#   
#   # FIXME: Buffer logic issue:
#   # When extend = "bbox" and shp = NULL, buffer is not explicitly set, causing 
#   # getbbox_nx_ny to use its default (5% extension). 
#   # This may not match user expectations
#   xy_clip <- NULL
#   if(extend == "chull"){
#     xsf_chull <- convexhull(x_shp)
#     if(is.null(buffer)){
#       if(is.null(shp)){
#         xsf_chull_xy <- sf::st_coordinates(xsf_chull)
#         buffer <- min(diff(range(xsf_chull_xy[, 1])) * 0.05,  
#                       diff(range(xsf_chull_xy[, 2])) * 0.05)
#       }else{
#         buffer <- 0
#       }
#     }
#     if(buffer > 0){
#       xsf_chull <- sf::st_buffer(xsf_chull, buffer)
#     }
#     xy_clip <- sf::st_coordinates(xsf_chull)
#     para <- getbbox_nx_ny(xy_clip[,1], xy_clip[,2], dx, dy, buffer = 0)
#   }else if(extend == "bbox"){
#     if(!is.null(shp)){
#       if(is.null(buffer)) buffer <- 0
#       para <- getbbox_nx_ny(x_shp[,1], x_shp[,2], dx, dy, buffer)
#     }else{
#       para <- getbbox_nx_ny(xypos[,1], xypos[,2], dx, dy, buffer)
#     }
#   }else if(extend == "obbox"){
#     sf_obb <- obbox(x_shp)
#     if(is.null(buffer)){
#       if(is.null(shp)){
#         xsf_chull_xy <- sf::st_coordinates(sf_obb)
#         buffer <- min(diff(range(xsf_chull_xy[, 1])) * 0.05,  
#                       diff(range(xsf_chull_xy[, 2])) * 0.05)
#       }else{
#         buffer <- 0
#       }
#     }
#     if(buffer > 0){
#       sf_obb <- sf::st_buffer(sf_obb, buffer)
#       sf_obb <- obbox(sf_obb)
#     }
#     xy_clip <- sf::st_coordinates(sf_obb)
#     
#     para <- getbbox_nx_ny(xy_clip[,1], xy_clip[,2], dx, dy, buffer = 0)
#   }else if(extend == "buffer"){
#     if(is.null(buffer) || !(buffer > 0)){
#       stop("When 'extend = buffer', 'buffer' must be larger than 0!")
#     }else{
#       x_shp <- buffer(obj, buffer)
#       xy_clip <- sf::st_coordinates(x_shp)
#       para <- getbbox_nx_ny(xy_clip[,1], xy_clip[,2], dx, dy, buffer = 0)
#     }
#   }
#   
#   fk <- NULL
#   
#   SL <- array(dim = c(para$nx, para$ny, length(vz)))
#   
# 
#   # ratio_x_y <- bbox_dy / bbox_dx
#   ratio_x_y <- (para$bbox[4] - para$bbox[3]) / (para$bbox[2] - para$bbox[1])
#   if(ratio_x_y < 1){
#     if(is.null(m)) m <- round(1/ratio_x_y)
#   }else{
#     if(is.null(n)) n <- round(ratio_x_y)
#   }
#   if(m < 1) m <- 1L
#   if(n < 1) n <- 1L
#   
#   fk <- NULL
#   if(!is.null(xy_clip)){
#     if(is.null(fk)){
#       fk <- outer(S$x, S$y, inPoly,
#                   vertx = xy_clip[,1],
#                   verty = xy_clip[,2])
#       fk <- !as.logical(fk)
#     }
#   }
#   for(j in  seq_along(vz)){
#     # val[[j]] <- unlist(lapply(V, function(v, k = j) v[k,]))
#     # val[[j]] <- V[,j]
#     # MBA::mba.surf echoes a warning when 
#     # all(c(range(xpos), range(ypos)) == para$bbox) == TRUE!!
#     S <- suppressWarnings(MBA::mba.surf(cbind(xypos, V[j,]), para$nx , para$ny, n = n, m = m, 
#                                         extend = TRUE, h = h, b.box = para$bbox)$xyz.est)
#     if(!is.null(fk)) S$z[fk] <- NA_real_
#     SL[,,j] <- S$z
#   }
#   return(list(x = S$x, y = S$y, z = SL, vz = vz, x0 = xypos[,1], y0 = xypos[,2], z0 = V))
#   
# }  

#' Interpolate GPR slices (refactored version)
#' 
#' This function interpolates GPR survey data onto a regular 3D grid using
#' multilevel B-spline approximation (MBA).
#' 
#' @param obj GPRsurvey object
#' @param dx x-resolution
#' @param dy y-resolution
#' @param dz z-resolution (depth or time spacing)
#' @param h MBA hierarchy levels (controls smoothness, default 6)
#' @param extend Extent method: "bbox", "obbox", "chull", or "buffer"
#' @param buffer Buffer distance around survey lines
#' @param shp Shape specification (sf object, list, or matrix)
#' @param m MBA row refinement (usually leave as default)
#' @param n MBA column refinement (usually leave as default)
#' @param verbose (`logical[1]`) If TRUE, verbose.
#' @return list with interpolation results:
#'   \item{x}{x-coordinates of grid}
#'   \item{y}{y-coordinates of grid}
#'   \item{z}{3D array of interpolated values [nx x ny x nz]}
#'   \item{vz}{depth/time vector}
#'   \item{x0}{original x-coordinates of observations}
#'   \item{y0}{original y-coordinates of observations}
#'   \item{z0}{interpolated data matrix [nz x n_traces]}
#' @noRd
.sliceInterp <- function(obj, dx = NULL, dy = NULL, dz = NULL, h = 6,
                         extend = "bbox", buffer = NULL, shp = NULL, 
                         m = 1, n = 1, verbose = TRUE) {
  
  # Step 1: Compute target depth vector
  vz <- .computeTargetDepths(obj, dz)
  
  # Step 2: Interpolate all profiles to target depths
  V <- .interpolateAllProfiles(obj, vz)
  
  # Step 3: Extract spatial coordinates (returns matrix [n x 2])
  # xypos <- .extractCoordinates(obj)
  xypos <- do.call(rbind, obj@coords)
  
  # Step 4: Process shape input
  x_shp <- .processShapeInput(shp, obj)
  shp_provided <- !is.null(shp)
  
  # Step 5: Compute interpolation extent
  extent <- .computeInterpolationExtent(
    extend, x_shp, xypos, dx, dy, buffer, shp_provided, obj
  )
  bbox_params <- extent$bbox_params
  xy_clip <- extent$clip_polygon
  
  # Step 6: Compute MBA refinement parameters
  mba_params <- .computeMBARefinement(bbox_params$bbox, m, n)
  
  # Step 7: Initialize output array
  SL <- array(dim = c(bbox_params$nx, bbox_params$ny, length(vz)))
  
  # Step 8: Create clipping mask (computed once after first interpolation)
  clip_mask <- NULL
  
  # Step 9: Interpolate each depth slice
  for (j in seq_along(vz)) {
    if (verbose) {
      message(sprintf("Interpolating slice %d/%d", j, length(vz)))
    }
    S <- .interpolateSlice(
      xypos[, 1:2], V[j, ], 
      bbox_params, h, mba_params$m, mba_params$n
    )
    
    # Create mask on first iteration if clipping is needed
    if (is.null(clip_mask) && !is.null(xy_clip)) {
      clip_mask <- .createClippingMask(S$x, S$y, xy_clip)
    }
    
    # Apply clipping if mask exists
    if (!is.null(clip_mask)) {
      S$z[clip_mask] <- NA_real_
    }
    
    SL[, , j] <- S$z
  }
  
  # Return results (note: z0 now returns the full interpolated matrix V)
  list(
    x = S$x,              # x-coordinates of grid
    y = S$y,              # y-coordinates of grid
    z = SL,               # 3D array of interpolated values
    vz = vz,              # depth/time vector
    x0 = xypos[, 1],      # original x-coordinates
    y0 = xypos[, 2],      # original y-coordinates
    z0 = V                # interpolated data at all depths [nz x n_traces]
  )
}

#' Compute target depth/time vector
#' 
#' @param obj GPRsurvey object
#' @param dz depth/time resolution
#' @return numeric vector of z values
#' @noRd
.computeTargetDepths <- function(obj, dz) {
  if (all(isZDepth(obj))) {
    # Depth mode: work from surface down
    zmax <- max(sapply(obj@coords, function(x) max(x[, 3])))
    zmin <- min(mapply(function(x, y) min(x[, 3] - y), 
                       obj@coords, obj@zlengths))
    vz <- seq(zmax, to = zmin, by = -dz)
  } else {
    # Time mode: work from zero up
    vz <- seq(from = 0, by = dz, to = max(obj@zlengths))
  }
  return(vz)
}

#' Interpolate single GPR profile to target depths
#' 
#' @param gpr_obj Single GPR object
#' @param vz Target depth/time vector
#' @return matrix of interpolated values [length(vz) x ncol(gpr_obj)]
#' @noRd
.interpolateProfile <- function(gpr_obj, vz) {
  # Replace NA with zeros
  gpr_obj@data[is.na(gpr_obj@data)] <- 0
  
  n_traces <- ncol(gpr_obj)
  
  if (isZDepth(gpr_obj)) {
    if (length(unique(gpr_obj@coord[, 3])) > 1) {
      # Variable topography: each trace has different z-values
      Z <- matrix(gpr_obj@coord[, 3], 
                  nrow = nrow(gpr_obj), 
                  ncol = n_traces, 
                  byrow = TRUE) - 
        matrix(gpr_obj@z, 
               nrow = nrow(gpr_obj), 
               ncol = n_traces)
      
      result <- vapply(seq_len(n_traces),
                       function(j) trInterp(gpr_obj@data[, j], Z[, j], vz),
                       numeric(length(vz)))
      
      # V[, idx] <- vapply(seq_len(ncol(OBJI@data)),
      #                    #                function(j) trInterp(OBJI@data[, j], Z[, j], vz),
      #                    #                numeric(length(vz)))
    } else {
      # Constant topography: all traces share z-values
      x_z <- gpr_obj@coord[1, 3] - gpr_obj@z
      result <- apply(gpr_obj@data, 2, trInterp, z = x_z, zi = vz)
    }
  } else {
    # Time/depth mode (no topography)
    x_z <- gpr_obj@z
    result <- apply(gpr_obj@data, 2, trInterp, z = x_z, zi = vz)
  }
  
  return(result)
}


#' Interpolate all profiles to common depth grid
#' 
#' @param obj GPRsurvey object
#' @param vz Target depth/time vector
#' @return matrix [length(vz) x total_traces]
#' @noRd
.interpolateAllProfiles <- function(obj, vz) {
  total_traces <- sum(obj@nx)
  V <- matrix(0, nrow = length(vz), ncol = total_traces)
  
  pos_start <- 0
  for (i in seq_along(obj)) {
    n_traces <- ncol(obj[[i]])
    idx <- pos_start  + seq_len(n_traces)
    
    V[, idx] <- .interpolateProfile(obj[[i]], vz)
    
    pos_start <- pos_start + n_traces
  }
  
  return(V)
}

#' Process shape input into standard matrix format
#' 
#' @param shp Shape specification (sf, list, matrix, or NULL)
#' @param obj GPRsurvey object (fallback if shp is NULL)
#' @return matrix [n x 2] of coordinates or GPRsurvey object
#' @noRd
.processShapeInput <- function(shp, obj) {
  if (is.null(shp)) {
    return(obj)
  }
  
  if (inherits(shp, c("sf", "sfc", "sfg"))) {
    return(sf::st_coordinates(shp)[, 1:2])
  } else if (is.list(shp)) {
    return(cbind(shp[[1]], shp[[2]]))
  } else {
    return(shp)
  }
}

#' Compute default buffer size
#' 
#' @param xy_coords Coordinate matrix [n x 2]
#' @param fraction Fraction of extent to use as buffer (default 0.05)
#' @return numeric buffer distance
#' @noRd
.computeDefaultBuffer <- function(xy_coords, fraction = 0.05) {
  min(
    diff(range(xy_coords[, 1])) * fraction,
    diff(range(xy_coords[, 2])) * fraction
  )
}

#' Compute interpolation extent for convex hull method
#' 
#' @param x_shp Shape coordinates
#' @param buffer Buffer distance (will compute default if NULL and shp not provided)
#' @param shp_provided Was shape explicitly provided?
#' @param dx x-resolution
#' @param dy y-resolution
#' @return list with bbox_params and clip_polygon
#' @noRd
.computeExtentConvexHull <- function(x_shp, buffer, shp_provided, dx, dy) {
  xsf_chull <- convexhull(x_shp)
  
  if (is.null(buffer)) {
    buffer <- if (shp_provided) {
      0
    } else {
      xsf_chull_xy <- sf::st_coordinates(xsf_chull)
      .computeDefaultBuffer(xsf_chull_xy)
    }
  }
  
  if (buffer > 0) {
    xsf_chull <- sf::st_buffer(xsf_chull, buffer)
  }
  
  xy_clip <- sf::st_coordinates(xsf_chull)
  para <- getbbox_nx_ny(xy_clip[, 1], xy_clip[, 2], dx, dy, buffer = 0)
  
  list(bbox_params = para, clip_polygon = xy_clip)
}

#' Compute interpolation extent for bounding box method
#' 
#' @param x_shp Shape coordinates (matrix or GPRsurvey)
#' @param xypos Observation coordinates matrix
#' @param buffer Buffer distance
#' @param shp_provided Was shape explicitly provided?
#' @param dx x-resolution
#' @param dy y-resolution
#' @return list with bbox_params and clip_polygon (NULL)
#' @noRd
.computeExtentBBox <- function(x_shp, xypos, buffer, shp_provided, dx, dy) {
  if (shp_provided) {
    if (is.null(buffer)) buffer <- 0
    para <- getbbox_nx_ny(x_shp[, 1], x_shp[, 2], dx, dy, buffer)
  } else {
    # When shp is NULL, buffer=NULL means getbbox_nx_ny uses 5% default
    para <- getbbox_nx_ny(xypos[, 1], xypos[, 2], dx, dy, buffer)
  }
  
  list(bbox_params = para, clip_polygon = NULL)
}


#' Compute interpolation extent for oriented bounding box method
#' 
#' @param x_shp Shape coordinates
#' @param buffer Buffer distance (will compute default if NULL and shp not provided)
#' @param shp_provided Was shape explicitly provided?
#' @param dx x-resolution
#' @param dy y-resolution
#' @return list with bbox_params and clip_polygon
#' @noRd
.computeExtentOBBox <- function(x_shp, buffer, shp_provided, dx, dy) {
  sf_obb <- obbox(x_shp)
  
  if (is.null(buffer)) {
    buffer <- if (shp_provided) {
      0
    } else {
      xsf_obb_xy <- sf::st_coordinates(sf_obb)
      .computeDefaultBuffer(xsf_obb_xy)
    }
  }
  
  if (buffer > 0) {
    sf_obb <- sf::st_buffer(sf_obb, buffer)
    sf_obb <- obbox(sf_obb)
  }
  
  xy_clip <- sf::st_coordinates(sf_obb)
  para <- getbbox_nx_ny(xy_clip[, 1], xy_clip[, 2], dx, dy, buffer = 0)
  
  list(bbox_params = para, clip_polygon = xy_clip)
}

#' Compute interpolation extent for buffer method
#' 
#' @param obj GPRsurvey object
#' @param buffer Buffer distance (must be > 0)
#' @param dx x-resolution
#' @param dy y-resolution
#' @return list with bbox_params and clip_polygon
#' @noRd
.computeExtentBuffer <- function(obj, buffer, dx, dy) {
  if (is.null(buffer) || !(buffer > 0)) {
    stop("When 'extend = buffer', 'buffer' must be larger than 0!")
  }
  
  x_shp <- buffer(obj, buffer)
  xy_clip <- sf::st_coordinates(x_shp)
  para <- getbbox_nx_ny(xy_clip[, 1], xy_clip[, 2], dx, dy, buffer = 0)
  
  list(bbox_params = para, clip_polygon = xy_clip)
}


#' Compute interpolation extent based on method
#' 
#' @param extend Method: "bbox", "obbox", "chull", or "buffer"
#' @param x_shp Shape coordinates (matrix or GPRsurvey)
#' @param xypos Observation coordinates matrix
#' @param dx x-resolution
#' @param dy y-resolution
#' @param buffer Buffer distance
#' @param shp_provided Was shape explicitly provided?
#' @param obj GPRsurvey object (for buffer method)
#' @return list with bbox_params and clip_polygon
#' @noRd
.computeInterpolationExtent <- function(extend, x_shp, xypos, 
                                        dx, dy, buffer, shp_provided, obj) {
  switch(extend,
         "chull" = .computeExtentConvexHull(x_shp, buffer, shp_provided, dx, dy),
         "bbox"  = .computeExtentBBox(x_shp, xypos, buffer, shp_provided, dx, dy),
         "obbox" = .computeExtentOBBox(x_shp, buffer, shp_provided, dx, dy),
         "buffer" = .computeExtentBuffer(obj, buffer, dx, dy),
         stop("Invalid extend method: ", extend)
  )
}


#' Compute MBA refinement parameters based on aspect ratio
#' 
#' @param bbox Bounding box vector c(xmin, xmax, ymin, ymax)
#' @param m Row refinement (will compute from aspect ratio if NULL)
#' @param n Column refinement (will compute from aspect ratio if NULL)
#' @return list with m and n
#' @noRd
.computeMBARefinement <- function(bbox, m = NULL, n = NULL) {
  ratio_x_y <- (bbox[4] - bbox[3]) / (bbox[2] - bbox[1])
  
  # Adjust refinement based on aspect ratio
  if (ratio_x_y < 1) {
    if (is.null(m)) m <- round(1 / ratio_x_y)
  } else {
    if (is.null(n)) n <- round(ratio_x_y)
  }
  
  # Ensure minimum of 1
  if (is.null(m)) m <- 1L
  if (is.null(n)) n <- 1L
  m <- max(1L, as.integer(m))
  n <- max(1L, as.integer(n))
  
  list(m = m, n = n)
}

#' Create clipping mask for interpolation grid
#' 
#' @param x Grid x-coordinates
#' @param y Grid y-coordinates
#' @param clip_polygon Polygon vertices [n x 2] or NULL
#' @return logical matrix or NULL
#' @noRd
.createClippingMask <- function(x, y, clip_polygon) {
  if (is.null(clip_polygon)) {
    return(NULL)
  }
  
  mask <- outer(x, y, inPoly,
                vertx = clip_polygon[, 1],
                verty = clip_polygon[, 2])
  
  return(!as.logical(mask))
}

#' Perform MBA interpolation for single depth slice
#' 
#' @param xypos Matrix [n x 2] of observation coordinates
#' @param values Vector of values at observation points
#' @param bbox_params Bounding box parameters (list with nx, ny, bbox)
#' @param h MBA hierarchy levels
#' @param m Row refinement
#' @param n Column refinement
#' @return list with x, y grid coordinates and z interpolated matrix
#' @noRd
.interpolateSlice <- function(xypos, values, bbox_params, h, m, n) {
  result <- tryCatch({
    suppressWarnings(
      MBA::mba.surf(
        cbind(xypos, values),
        bbox_params$nx,
        bbox_params$ny,
        n = n,
        m = m,
        extend = TRUE,
        h = h,
        b.box = bbox_params$bbox
      )$xyz.est
    )
  }, error = function(e) {
    stop("MBA interpolation failed at depth slice: ", e$message, call. = FALSE)
  })
  
  return(result)
}

#' Compute bounding box and grid dimensions
#' 
#' @param xpos x-coordinates
#' @param ypos y-coordinates
#' @param dx x-resolution
#' @param dy y-resolution
#' @param buffer Buffer distance (NULL for auto 5%)
#' @return list with bbox, nx, ny
#' @noRd
getbbox_nx_ny <- function(xpos, ypos, dx, dy, buffer = NULL) {
  xpos_rg <- range(xpos, na.rm = TRUE)
  ypos_rg <- range(ypos, na.rm = TRUE)
  
  if (is.null(buffer)) {
    buffer <- min(diff(xpos_rg) * 0.05, diff(ypos_rg) * 0.05)
  }
  
  if (buffer > 0) {
    xpos_rg <- xpos_rg + c(-1, 1) * buffer
    ypos_rg <- ypos_rg + c(-1, 1) * buffer
  }
  
  bbox <- c(xpos_rg, ypos_rg)
  
  # Define the number of cells
  bbox_dx <- bbox[2] - bbox[1]
  bbox_dy <- bbox[4] - bbox[3]
  nx <- ceiling(bbox_dx / dx)
  ny <- ceiling(bbox_dy / dy)
  
  # Correct bbox so that dx, dy are exact
  Dx <- (nx * dx - bbox_dx) / 2
  Dy <- (ny * dy - bbox_dy) / 2
  bbox[1:2] <- bbox[1:2] + c(-1, 1) * Dx
  bbox[3:4] <- bbox[3:4] + c(-1, 1) * Dy
  
  nx <- nx + 1L
  ny <- ny + 1L
  
  list(bbox = bbox, nx = nx, ny = ny)
}



