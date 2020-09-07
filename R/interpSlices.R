
#' Interpolate horizontal slices
#' 
#' 
#' Interpolate horizontal slices
#' @param x [\code{GPRsurvey}]
#' @param dx [\code{numeric(1)}] x-resolution
#' @param dy [\code{numeric(1)}] y-resolution
#' @param dz [\code{numeric(1)}] z-resolution
#' @param h [\code{numeric(1)}] FIXME: see function...
#' @param extend [\code{character(1)}] FIXME: see function...
#' @param buffer [\code{numeric(1)}] FIXME: see function...
#' @param shp [\code{matrix(n,2)|list(2)|sf}] FIXME: see function...
#' @param rot [\code{logical(1)|numeric(1)}] If \code{TRUE} the GPR lines are 
#'            fist rotated such to minimise their axis-aligned bounding box. 
#'            If \code{rot} is numeric, the GPR lines is rotated first
#'            rotated by \code{rot} (in radian).
#' @name interpSlices
setGeneric("interpSlices", function(x, 
                                    dx = NULL, 
                                    dy = NULL, 
                                    dz = NULL, 
                                    h = 6,
                                    extend = c("bbox", "obbox", "chull", "buffer"),
                                    buffer = NULL,
                                    shp = NULL,
                                    rot = FALSE) 
  standardGeneric("interpSlices"))

#' @rdname interpSlices
#' @export
setMethod("interpSlices", "GPRsurvey", function(x, 
                                                dx = NULL, 
                                                dy = NULL, 
                                                dz = NULL, 
                                                h = 6,
                                                extend = c("bbox", "obbox", "chull", "buffer"),
                                                buffer = NULL,
                                                shp = NULL,
                                                rot = FALSE){
  
  if(is.null(dx) || is.null(dy) || is.null(dz)){
    stop("'dx', 'dy' and 'dz' must all be defined!")
  }
  if( dx <= 0 || dy <= 0 || dz <= 0 ){
    stop("'dx', 'dy' and 'dz' must all be strickly positive!")
  }
  
  extend = match.arg(extend, c("bbox", "obbox", "chull", "buffer"))
  if(isTRUE(rot)){
    # x_rot <-  spAngle(x)
    x <- spGeoref(x)
    x_rot <- x@transf[5]
  }else if(!isFALSE(rot)){
    if(is.numeric(rot)){
      x_rot <- rot
      x <- spGeoref(x, alpha = x_rot)
    }else{
      stop("'rot' must be either TRUE/FALSE or numeric")
    }
  }else{
    x_rot <- 0
  }
  test <- sapply(x@coords, function(x) length(x) > 0)
  
  SXY <- .sliceInterp(x = x[test], dx = dx, dy = dy, dz = dz, h = h,
                      extend = extend,
                      buffer = buffer,
                      shp = shp)
  
  xyref <- c(min(SXY$x), min(SXY$y), SXY$vz[1])
  # xpos <- SXY$x #- min(SXY$x)
  # ypos <- SXY$y #- min(SXY$y)
  
  xfreq <- ifelse(length(unique(x@freqs[test])) == 1, x@freqs[test][1], numeric())
  
  if(dim(SXY$z)[3] == 1){
    class_name <- "GPRslice"
    ddata <- SXY$z[,,1]
  }else{
    class_name <- "GPRcube"
    ddata <- SXY$z
  }
  
  y <- new(class_name,
           #----------------- GPRvirtual --------------------------------------#
           version      = "0.3",
           name         = "",
           path         = x@paths[test][1],
           desc         = "GPR cube",  # data description
           mode         = x@modes[test][1],  # reflection/CMP/WARR (CMPAnalysis/spectrum/...)?
           date         = Sys.Date(),       # survey date (format %Y-%m-%d)
           freq         = xfreq,    # antenna frequency
           
           data         = ddata,      # data
           dunit        = "",         # data unit
           dlab         = "", 
           
           spunit       = x@spunit[test][1],  # spatial unit
           crs          = x@crs[test][1],  # coordinate reference system of @coord
           # ? coordref = "numeric",    # coordinates references or "center" or "centroid"
           
           xunit        = x@spunit[test][1],  # horizontal unit
           xlab         = "",
           
           zunit        = x@zunits[test][1],  # time/depth unit
           zlab         = "",
           
           # vel          = "list",   # velocity model (function of x, y, z)
           
           # proc         = "list",       # processing steps
           # delineations = "list",       # delineations
           # md           = "list",        # data from header file/meta-data
           #----------------- GPRcube -----------------------------------------#
           dx     = dx,   # xpos,
           dy     = dy,   # ypos,
           dz     = dz * sign(mean(diff(SXY$vz))),   # SXY$vz,
           ylab   = "",   #,  # set names, length = 1|p
           
           center = xyref,    # coordinates grid corner bottom left (0,0)
           rot    = c(x@transf[1:2], x_rot)#x_rot     # rotation angle
  )
  if(class_name == "GPRslice") y@z <- SXY$vz[1]
  return(y)
})


# define z
defVz <- function(x){
  #if(x@zunit == "ns"){
  # time
  # if unit are not time and if there are coordinates for each GPR data
  if( length(unique(x@spunit)) > 1 ){
    stop("Position units are not identical: \n",
         paste0(unique(x@spunit), collaspe = ", "), "!")
  }
  if(length(unique(x@zunits)) > 1){
    stop("Depth units are not identical: \n",
         paste0(unique(x@zunits), collaspe = ", "), "!\n")
  }
  # if(!all(grepl("[s]$", x@zunits))
  # x@zunits != "ns"  
  if(all(isZDepth(x)) && all(sapply(x@coords, length) > 0)){
    # elevation coordinates
    zmax <- sapply(x@coords, function(x) max(x[,3]))
    zmin <- sapply(x@coords, function(x) min(x[,3])) - max(x@zlengths)
    d_z <- x@zlengths/x@nz
    vz <- seq(from = min(zmin), to = max(zmax), by = min(d_z))
  }else{
    # time/depth
    d_z <- x@zlengths/x@nz
    vz <- seq(from = 0, by = min(d_z), length.out = max(x@nz))
  }
  return(vz)
}



# x = amplitude
# z = time/depth
# zi = time/depth at which to interpolate
trInterp <- function(x, z, zi){
  # isNA <- is.na(x)
  isNA <- is.na(x)
  # xi <- signal::interp1(z[!isNA], x[!isNA], zi, method = "pchip", extrap = 0)
  xi <- signal::interp1(x = z[!isNA], y = x[!isNA], xi = zi, method = "spline", 
                        extrap = 0)
  return(xi)
}


.sliceInterp <- function(x, dx = NULL, dy = NULL, dz = NULL, h = 6,
                         extend = "bbox",
                         buffer = NULL,
                         shp = shp){
  if(!all(sapply(x@coords, length) > 0) ){
    stop("Some of the data have no coordinates. Please set first coordinates to all data.")
  }
  X <- x
  # default z-values (get elevation range)
  x_zi0 <- defVz(X)
  x_zi <- seq(min(x_zi0), by = dz, to = max(x_zi0))
  if(all(isZDepth(X)) ){
    x_zi <- sort(x_zi, decreasing = TRUE)
  }
  
  if(is.null(dx)){
    dx <- mean(sapply(x@coords, function(x) mean(diff(pathRelPos(x)))))
  }
  if(is.null(dy)){
    dy <- dx
  }
  if(is.null(dz)){
    dz <- abs(diff(x_zi))
  }
  #Z <- list()
  V <- list()
  for(i in seq_along(X)){
    X_i <- X[[i]]
    if(isZDepth(X_i)){
      if(length(unique(X_i@coord[,3])) > 1){
        stop("The traces have different elevation!")
      } 
      x_z   <- X_i@coord[1,3] - X_i@z
    }else{
      x_z   <- X_i@z
    }
    x_data <- X_i@data
    x_data[is.na(x_data)] <- 0
    # interpolation
    V[[i]] <- apply(x_data, 2, trInterp, z = x_z, zi = x_zi )
    # Z[[i]] <- x_zi   # X_i@depth
  }
  
  # vj <- seq(dz, by = dz, to = length(x_zi))
  
  val <- list()
  # positions obervations
  xpos <- unlist(lapply(X@coords, function(x) x[,1]))
  ypos <- unlist(lapply(X@coords, function(x) x[,2]))
  
  # # define bounding box + number of points for interpolation
  # obb <- spOBB(x)
  # bbox <- c(min(obb[,1]), max(obb[,1]), min(obb[,2]), max(obb[,2]))
  # 
  # 
  # bbox_dx <- bbox[2] - bbox[1]
  # bbox_dy <- bbox[4] - bbox[3]
  # nx <- ceiling(bbox_dx / dx )
  # ny <- ceiling(bbox_dy / dy )
  # 
  # # correct bbox (such that dx, dy are correct)
  # Dx <- (nx * dx - bbox_dx)/2
  # Dy <- (ny * dy - bbox_dy)/2
  # bbox[1:2] <- bbox[1:2] + c(-1, 1) * Dx
  # bbox[3:4] <- bbox[3:4] + c(-1, 1) * Dy 
  # bbox_dx <- bbox[2] - bbox[1]
  # bbox_dy <- bbox[4] - bbox[3]
  # SL <- array(dim = c(nx, ny, length(x_zi)))
  # 
  
  x_shp <- x
  if(!is.null(shp)){
    if(inherits(shp, "sf") || inherits(shp, "sfc") || inherits(shp, "sfg")){
      x_shp <- sf::st_coordinates(shp)[,1:2]
    }else if(is.list(shp)){
      x_shp <- cbind(shp[[1]], shp[[2]])
    }else{
      x_shp <- shp
    }
  }

  xy_clip <- NULL
  if(extend == "chull"){
    xsf_chull <- spConvexHull(x_shp)
    if(is.null(buffer)){
      if(is.null(shp)){
        xsf_chull_xy <- sf::st_coordinates(xsf_chull)
        buffer <- min(diff(range(xsf_chull_xy[, 1])) * 0.05,  
                      diff(range(xsf_chull_xy[, 2])) * 0.05)
      }else{
        buffer <- 0
      }
    }
    if(buffer > 0){
      xsf_chull <- sf::st_buffer(xsf_chull, buffer)
    }
    xy_clip <- sf::st_coordinates(xsf_chull)
    para <- getbbox_nx_ny(xy_clip[,1], xy_clip[,2], dx, dy, buffer = 0)
  }else if(extend == "bbox"){
    if(!is.null(shp)){
      if(is.null(buffer)) buffer <- 0
      para <- getbbox_nx_ny(x_shp[,1], x_shp[,2], dx, dy, buffer)
    }else{
      para <- getbbox_nx_ny(xpos, ypos, dx, dy, buffer)
    }
  }else if(extend == "obbox"){
    sf_obb <- spOBB(x_shp)
    if(is.null(buffer)){
      if(is.null(shp)){
        xsf_chull_xy <- sf::st_coordinates(sf_obb)
        buffer <- min(diff(range(xsf_chull_xy[, 1])) * 0.05,  
                      diff(range(xsf_chull_xy[, 2])) * 0.05)
      }else{
        buffer <- 0
      }
    }
    if(buffer > 0){
      sf_obb <- sf::st_buffer(sf_obb, buffer)
      sf_obb <- spOBB(sf_obb)
    }
    xy_clip <- sf::st_coordinates(sf_obb)
    
    para <- getbbox_nx_ny(xy_clip[,1], xy_clip[,2], dx, dy, buffer = 0)
  }else if(extend == "buffer"){
    if(is.null(buffer) || !(buffer > 0)){
      stop("When 'extend = buffer', 'buffer' must be larger than 0!")
    }else{
      x_shp <- spBuffer(x, buffer)
      xy_clip <- sf::st_coordinates(x_shp)
      para <- getbbox_nx_ny(xy_clip[,1], xy_clip[,2], dx, dy, buffer = 0)
    }
  }
  
  fk <- NULL

  SL <- array(dim = c(para$nx, para$ny, length(x_zi)))

  n <- 1
  m <- 1
  # ratio_x_y <- bbox_dy / bbox_dx
  ratio_x_y <- (para$bbox[4] - para$bbox[3]) / (para$bbox[2] - para$bbox[1])
  if(ratio_x_y < 1){
    m <- round(1/ratio_x_y)
  }else{
    n <- round(ratio_x_y)
  }
  if(m < 1) m <- 1L
  if(n < 1) n <- 1L
  for(j in  seq_along(x_zi)){
    val[[j]] <- unlist(lapply(V, function(v, k = j) v[k,]))
    S <- MBA::mba.surf(cbind(xpos, ypos, val[[j]]), para$nx , para$ny, n = n, m = m, 
                       extend = TRUE, h = h, b.box = para$bbox)$xyz.est
    if(!is.null(xy_clip)){
      if(is.null(fk)){
        fk <- outer(S$x, S$y, inPoly,
                    vertx = xy_clip[,1],
                    verty = xy_clip[,2])
        fk <- !as.logical(fk)
      }
      S$z[fk] <- NA
    }
    SL[,,j] <- S$z
  }
  return(list(x = S$x, y = S$y, z = SL, vz = x_zi, x0 = xpos, y0 = ypos, z0 = val))
  
}  



getbbox_nx_ny <- function(xpos, ypos, dx, dy, buffer = NULL){
  xpos_rg <- range(xpos)
  ypos_rg <- range(ypos)
  
  if(is.null(buffer)){
    buffer <- min(diff(xpos_rg) * 0.05,  diff(ypos_rg) * 0.05)
    print(buffer)
  }
  if(buffer > 0 ){
    print(buffer)
    xpos_rg <- xpos_rg + c(-1, 1) * buffer
    ypos_rg <- ypos_rg + c(-1, 1) * buffer
    print(c(-1, 1) * buffer)
  }
  # if(isTRUE(prettyExtend)){
  #   xpos_rg <- pretty( xpos_rg)
  #   ypos_rg <- pretty(ypos_rg )
  # }
  bbox <- c(xpos_rg, ypos_rg)
  
  # define the number of cells
  bbox_dx <- bbox[2] - bbox[1]
  bbox_dy <- bbox[4] - bbox[3]
  nx <- ceiling(bbox_dx / dx )
  ny <- ceiling(bbox_dy / dy )
  
  # correct bbox (such that dx, dy are correct)
  Dx <- (nx * dx - bbox_dx)/2
  Dy <- (ny * dy - bbox_dy)/2
  bbox[1:2] <- bbox[1:2] + c(-1, 1) * Dx
  bbox[3:4] <- bbox[3:4] + c(-1, 1) * Dy 
  # bbox_dx <- bbox[2] - bbox[1]
  # bbox_dy <- bbox[4] - bbox[3]
  nx <- nx + 1
  ny <- ny + 1
  return(list(bbox = bbox, nx = nx, ny = ny))
}

