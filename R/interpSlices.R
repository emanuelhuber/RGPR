
#' Interpolate horizontal slices
#' 
#' 
#' Interpolate horizontal slices
#' @param x [\code{GPRsurvey}]
#' @param dx [\code{numeric(1)}] x-resolution
#' @param dy [\code{numeric(1)}] y-resolution
#' @param dz [\code{numeric(1)}] z-resolution
#' @param h [\code{numeric(1)}] FIXME: see function...
#' @param extend [\code{numeric(1)}] FIXME: see function...
#' @param buffer [\code{numeric(2)}] FIXME: see function...
#' @name interpSlices
setGeneric("interpSlices", function(x, 
                                    dx = NULL, 
                                    dy = NULL, 
                                    dz = NULL, 
                                    h = 6,
                                    extend = TRUE,
                                    buffer = NULL) 
  standardGeneric("interpSlices"))

#' @rdname interpSlices
#' @export
setMethod("interpSlices", "GPRsurvey", function(x, 
                                                dx = NULL, 
                                                dy = NULL, 
                                                dz = NULL, 
                                                h = 6,
                                                extend = TRUE,
                                                buffer = NULL){
  
  if(is.null(dx) || is.null(dy) || is.null(dz)){
    stop("'dx', 'dy' and 'dz' must all be defined!")
  }
  if( dx <= 0 || dy <= 0 || dz <= 0 ){
    stop("'dx', 'dy' and 'dz' must all be strickly positive!")
  }
  
  SXY <- .sliceInterp(x = x, dx = dx, dy = dy, dz = dz, h = h,
                      extend = extend,
                      buffer = buffer)
  
  xyref <- c(min(SXY$x), min(SXY$y))
  # xpos <- SXY$x - min(SXY$x)
  # ypos <- SXY$y - min(SXY$y)
  
  # print(xyref)
  # print(xpos)
  
  xfreq <- ifelse(length(unique(x@freqs)) == 1, x@freqs[1], numeric())
  
  # plot3D::image2D(x = SXY$x, y = SXY$y, z = SXY$z[,,k],
  #                 main = paste0("elevation = ", SXY$vz[k], " m"),
  #                 zlim = zlim, col = palGPR("slice"))
  # 
  # plot3D::points2D(x = SXY$x0, y = SXY$y0, colvar = SXY$z0[[k]],
  #                  add = TRUE, pch = 20, clim = zlim, col = palGPR("slice"))
  
  # FIXME : if only one slice -> create slice object !!!!
  if(dim(SXY$z)[3] == 1){
    className <- "GPRslice"
    ddata <- SXY$z[,,1]
  }else{
    className <- "GPRcube"
    ddata <- SXY$z
  }
  
  y <- new(className,
           # version      = "0.1",
           # name         = "",
           # date         = as.character(Sys.Date()),  
           # freq         = xfreq,
           # filepaths    = x@filepaths,
           # x            = xpos,
           # y            = ypos,
           # data         = ddata,
           # coord        = xyref,
           # posunit      = x@posunits[1],
           # crs          = x@crs,
           # depth        = SXY$vz,
           # depthunit    = x@zunits[1],
           # #vel         = "list",               
           # #delineations = "list",
           # obs          = list(x = SXY$x0,
           #                     y = SXY$y0,
           #                     z = SXY$z0)
           #transf       = "numeric"
           #----------------- GPRvirtual --------------------------------------#
           version      = "0.3",
           name         = "",
           path         = x@paths,
           desc         = x@descs,  # data description
           mode         = x@modes,  # reflection/CMP/WARR (CMPAnalysis/spectrum/...)?
           date         = Sys.Date(),       # survey date (format %Y-%m-%d)
           freq         = xfreq,    # antenna frequency
           
           data         = ddata,      # data
           dunit        = "",         # data unit
           dlab         = "", 
           
           spunit       = x@spunit[1],  # spatial unit
           crs          = x@crs[1],  # coordinate reference system of @coord
           # ? coordref = "numeric",    # coordinates references or "center" or "centroid"
           
           xunit        = x@spunit[1],  # horizontal unit
           xlab         = "",
           
           zunit        = x@zunits[1],  # time/depth unit
           zlab         = "",
           
           # vel          = "list",   # velocity model (function of x, y, z)
           
           # proc         = "list",       # processing steps
           # delineations = "list",       # delineations
           # md           = "list",        # data from header file/meta-data
           #----------------- GPRcube -----------------------------------------#
           dx     = dx,
           dy     = dy,
           dz     = dz,
           ylab   = "",  # set names, length = 1|p
           
           center = xyref,    # coordinates grid corner bottom left (0,0)
           rot    = 0     # affine transformation
  )
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
  if(all(isZunitLength(x)) && all(sapply(x@coords, length) > 0)){
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
                         extend = extend,
                         buffer = NULL){
  if(!all(sapply(x@coords, length) > 0) ){
    stop("Some of the data have no coordinates. Please set first coordinates to all data.")
  }
  X <- x
  # default z-values (get elevation range)
  x_zi0 <- defVz(X)
  x_zi <- seq(min(x_zi0), by = dz, to = max(x_zi0))
  if(all(isZunitLength(X)) ){
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
    if(isZunitLength(X[[i]])){
      if(length(unique(X[[i]]@coord[,3])) > 1){
        stop("The traces have different elevation!")
      } 
      x_z   <- X[[i]]@coord[1,3] - X[[i]]@z
    }else{
      x_z   <- X[[i]]@z
    }
    x_data <- X[[i]]@data
    x_data[is.na(x_data)] <- 0
    # interpolation
    V[[i]] <- apply(x_data, 2, trInterp, z = x_z, zi = x_zi )
    # Z[[i]] <- x_zi   # X[[i]]@depth
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
  
  
  para <- getbbox_nx_ny(xpos, ypos, dx, dy, buffer)
  
  SL <- array(dim = c(para$nx, para$ny, length(x_zi)))
  
  # h = 6
  
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
    # j <- vj[u]
    #z <- rep(sapply(Z, function(x, i = j) x[i]), sapply(V, ncol))
    val[[j]] <- unlist(lapply(V, function(v, k = j) v[k,]))
    S <- MBA::mba.surf(cbind(xpos, ypos, val[[j]]), para$nx , para$ny, n = n, m = m, 
                       extend = extend, h = h, b.box = para$bbox)$xyz.est
    SL[,,j] <- S$z
  }
  return(list(x = S$x, y = S$y, z = SL, vz = x_zi, x0 = xpos, y0 = ypos, z0 = val))
  
}  



getbbox_nx_ny <- function(xpos, ypos, dx, dy, buffer = NULL){
  xpos_rg <- range(xpos)
  ypos_rg <- range(ypos)
  
  if(is.null(buffer)){
    buffer <- rep(min(diff(xpos_rg) * 0.05,
                diff(ypos_rg) * 0.05), 2)
  }
  xpos_pretty <- pretty( xpos_rg + c(-1, 1) * buffer[1])
  ypos_pretty <- pretty(ypos_rg + c(-1, 1) * buffer[2])
  bbox <- c(xpos_pretty[1], tail(xpos_pretty, 1), 
            ypos_pretty[1], tail(ypos_pretty, 1))
  
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
  bbox_dx <- bbox[2] - bbox[1]
  bbox_dy <- bbox[4] - bbox[3]
  nx <- nx + 1
  ny <- ny + 1
  return(list(bbox = bbox, nx = nx, ny = ny))
}

