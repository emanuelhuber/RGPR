
#' Class GPRcube
#' 
#' An S4 class to represent 3D ground-penetrating radar (GPR) data.
#' 
#' @name GPRcube-class
#' @rdname GPRcube-class
#' @export
setClass(
  Class="GPRcube",  
  slots=c(
    version = "character",      # version of the class
    date = "character",          # date of creation, format %Y-%m-%d
    
    filepaths = "character",     # filepath of the profile
    
    freq = "numeric",            # antenna frequency (if unique)
    name = "character",         # name of the cube

    
    x = "numeric",      # trace position along x-axes (local crs)
    y = "numeric",      # trace position along y-axes (local crs)
    data = "array",     # 3D [x, y, z] one column per trace
    
    coord = "numeric",      # coordinates grid corner bottom left (0,0)
    posunit = "character",  # spatial unit
    crs = "character",      # coordinate reference system of coord
    
    depth = "numeric",         # depth position
    depthunit = "character",   # time/depth unit
    
    vel = "list",                # velocity model
    delineations = "list",       # delineated lines
    
    obs = "list",                # observation points used for interpolation 
    
    transf = "numeric"          # affine transformation
  )
)



#' Class GPRslice
#' 
#' An S4 class to represent time/depth slices of 
#' ground-penetrating radar (GPR) data.
#' 
#' @name GPRslice-class
#' @rdname GPRslice-class
#' @export
setClass(
  Class = "GPRslice",  
  contains = "GPRcube"
)

#' Return TRUE if the data are a function of length
#' 
#' @name isLengthUnit
#' @rdname isLengthUnit
#' @export
setMethod("isLengthUnit", "GPRslice", function(x){
  !isTimeUnit(x)
} 
)

#' Return TRUE if the data are a function of time
#' 
#' @name isTimeUnit
#' @rdname isTimeUnit
#' @export
setMethod("isTimeUnit", "GPRslice", function(x){
  grepl("[s]$", x@depthunit)
})

#' @export
newFUnction <- function(x){
 x
}

#------------------------------
# "["
#' extract parts of GPRsurvey
#'
#' Return an object of class GPR slice
#' @name GPRcube-subset
#' @docType methods
#' @rdname GPRcube-subset
setMethod(
  f = "[",
  signature = "GPRcube",
  definition = function(x, i, j, k, drop = TRUE){
    if(missing(i) || length(i) == 0){
      i <- 1:dim(x@data)[1]
    } 
    if(missing(j) || length(j) == 0){
      j <- 1:dim(x@data)[2]
    }
    # dots <- list(...)
    # if(length(dots) > 0){
    #   k <- as.integer(dots[[1]])
    # }
    # print(dots)
    if(missing(k) || length(k) == 0){
      k <- 1:dim(x@data)[3]
    }
    # extract slice k
    if(length(k) == 1){
      y <- new("GPRslice",
               version      = "0.1",
               name         = x@name,
               date         = x@date,  
               freq         = x@freq,
               filepaths    = x@filepaths,
               x            = x@x[i],
               y            = x@y[j],
               data         = x@data[i, j, k, drop = TRUE],
               coord        = x@coord,
               posunit      = x@posunit,
               crs          = x@crs,
               depth        = x@depth[k],
               depthunit    = x@depthunit
              )
    # extract GPR alons x or y axis
    }else if(length(i) == 1 || length(j) == 1){
      u <- which(c(length(i), length(j)) == 1)[1]
      if(u == 1){
        dx <- mean(abs(diff(x@y)))
        xpos <- x@y[j]
      }else{
        dx <- mean(abs(diff(x@x)))
        xpos <- x@x[i]
      }
      xdata <- x@data[i, j, k]
      if(is.null(dim(xdata))){
        n <- 1L
        dim(xdata) <- c(length(xdata), 1)
      }else{
        xdata <- t(xdata)
        n <- ncol(xdata)
      }
      y <- new("GPR",   
            version     = "0.2",
            data        = xdata,
            traces      = seq_len(n),
            fid         = rep("", n),
            #coord      = coord,            FIXME!
            pos         = xpos,        
            depth       = x@depth,
            #rec        = rec_coord,         
            #trans      = trans_coord,
            time0       = rep(0, n),          
            #time       = traceTime,        
            #proc       = character(0),     
            vel         = list(0.1),         
            name        = x@name,
            #description = "",
            #filepath    = "",
            dz          = abs(diff(x@depth)), 
            dx          = dx,              
            depthunit   = x@depthunit,
            posunit     = x@posunit,
            freq        = x@freq, 
            #antsep      = antsep[1], 
            surveymode  = "reflection",
            date        = x@date,
            crs         = character(0)
            #hd          = sup_hd                      # header
        )
    # extract sub-cube
    }else{
      y <- new("GPRcube",
               version      = x@version,
               name         = x@name,
               date         = x@date,  
               freq         = x@freq,
               filepaths    = x@filepaths,
               x            = x@x[i],
               y            = x@y[j],
               data         = x@data[i, j, k, drop = FALSE],
               coord        = x@coord,
               posunit      = x@posunit,
               crs          = x@crs,
               depth        = x@depth[k],
               depthunit    = x@depthunit,
               vel          = x@vel,               
               delineations = x@delineations,
               obs          = x@obs,
               transf       = x@transf
      )
    }
    
    return(y)
  }
)


#' Plot a GPR cube
#'
#' Plot GPR data cube.
#' 
#' i, j and k define the sections of the cube that are plotted. 
#' With the default value (e.g., i = NULL, the first and the last sections 
#' of all dimensions are plotted). The additional arguments (...) correspond 
#' to the arguments of the function 'plot3D::surf3D()' (with argument 'theta' 
#' and 'phi' you can define the orientation of the cube).
#' 
#' @param x Object of class \code{GPRcube}
#' @param add logical. If \code{TRUE}, add to current plot
#' @param ratio logical. Add fiducial markes
#' @param barscale logical. Add a colorbar scale
#' @param main character. Plot title.
#' @method plot GPRcube 
#' @name plot
#' @rdname plot
#' @export
plot.GPRcube <- function(x, 
                         i = NULL, 
                         j = NULL,
                         k = NULL,
                         xlim = NULL,
                         ylim = NULL,
                         zlim = NULL,
                         clim = NULL,
                         colkey = NULL,
                         add = FALSE,
                         col = NULL,
                         inttype = 2,
                          ...){

  rnxyz <- dim(x@data)
  nx <- rnxyz[1]
  ny <- rnxyz[2]
  nz <- rnxyz[3]
  
  if(is.null(i)) i <- c(1, nx)
  if(is.null(j)) j <- c(1, ny)
  if(is.null(k)) k <- c(1, nz)
  
  if(is.null(xlim)) xlim <- range(x@x) 
  if(is.null(ylim)) ylim <- range(x@y)
  if(is.null(zlim)) zlim <- range(x@depth)
  
  if( min(x@data, na.rm = TRUE) >= 0 ){
      # to plot amplitudes for example...
      if(is.null(clim)) clim <- c(0, max(x@data, na.rm = TRUE))
      if(is.null(col))  col <- palGPR("slice")
  }else{
      if(is.null(clim)) clim <- c(-1, 1) * max(abs(x@data), na.rm = TRUE)
      if(is.null(col))  col <-  palGPR(n = 101)
  }
  
  # y-0: x x z (100 x 102) at y = 0
  for(vj in seq_along(j)){
    vx <- x@x
    vy <- rep(x@y[j[vj]], nz)
    vz <- matrix(rep(x@depth, each = nx), ncol = nz, 
                 nrow = nx, byrow = FALSE)
    M1 <- plot3D::mesh(vx, vy)
    
    plot3D::surf3D(M1$x, M1$y, vz, colvar = (x@data[, j[vj], nz:1]),  
                   add = add, colkey = colkey,
                   xlim = xlim, 
                   ylim = ylim,
                   zlim = zlim,
                   clim = clim, inttype = inttype,
                   col = col, ...)
    if(!isTRUE(add)) add <- TRUE
    if(is.null(colkey)) colkey <- list(plot = FALSE)
  }
  
  
  # x-0: y x z () at x = 0
  for(vi in seq_along(i)){
    vx <- rep(x@x[i[vi]], nz)
    vy <- x@y
    vz <- matrix(rep(x@depth, each = ny), ncol = nz, 
                 nrow = ny, byrow = FALSE)
    M1 <- plot3D::mesh(vx, vy)
    
    plot3D::surf3D(M1$x, M1$y, t(vz), colvar = t(x@data[i[vi],,nz:1]),  
                   add = add, colkey = colkey,
                   xlim = xlim, 
                   ylim = ylim,
                   zlim = zlim,
                   clim = clim,  inttype = inttype,
                   col = col, ...)
    if(!isTRUE(add)) add <- TRUE
    if(is.null(colkey)) colkey <- list(plot = FALSE)
    
  }
  
  
  # y-max: y x z () at x = 0
  for(vk in seq_along(k)){
    vx <- x@x
    vy <- x@y
    vz <- matrix(rep(rep(rev(x@depth)[k[vk]], nx), each = ny), 
                 ncol = ny, nrow = nx, byrow = TRUE)
    M1 <- plot3D::mesh(vx, vy)
    
    plot3D::surf3D(M1$x, M1$y, vz, colvar = (x@data[,,k[vk]]),  
                   add = add, colkey = colkey,
                   xlim = xlim, 
                   ylim = ylim,
                   zlim = zlim,
                   clim = clim,  inttype = inttype,
                   col = col, ...)
    if(!isTRUE(add)) add <- TRUE
    if(is.null(colkey)) colkey <- list(plot = FALSE)
    
  }
}

#' Plot a slice.
#'
#' @param x Object of class \code{GPRslice}
#' @param add logical. If \code{TRUE}, add to current plot
#' @param ratio logical. Add fiducial markes
#' @param barscale logical. Add a colorbar scale
#' @param main character. Plot title.
#' @method plot GPRslice 
#' @name plot
#' @rdname plot
#' @export
plot.GPRslice <- function(x, 
                     main = NULL, 
                     xlab = NULL,
                     ylab = NULL,
                     col = NULL,
                     clim = NULL,
                     ...){
  if(is.null(main)){
    if(isTimeUnit(x)){
      main <- paste0("time = ", x@depth, " ", x@depthunit)
    }else{
      main <- paste0("depth = ", x@depth, " ", x@depthunit)
    }
  }
  if(is.null(xlab)){
    xlab <- paste0("x (", x@posunit, ")")
  }
  if(is.null(ylab)){
    ylab <- paste0("y (", x@posunit, ")")
  }
  
  if( min(x@data, na.rm = TRUE) >= 0 ){
    # to plot amplitudes for example...
    if(is.null(clim)) clim <- c(0, max(x@data, na.rm = TRUE))
    if(is.null(col))  col <- palGPR("slice")
  }else{
    if(is.null(clim)) clim <- c(-1, 1) * max(abs(x@data), na.rm = TRUE)
    if(is.null(col))  col <-  palGPR(n = 101)
  }
  
  plot3D::image2D(x = x@x, y = x@y, z = x@data,
                main = main, xlab, ylab, clim = clim, col = col, ...)
}










# define z
defVz <- function(x){
  #if(x@zunit == "ns"){
  # time
  # if unit are not time and if there are coordinates for each GPR data
  if( length(unique(x@posunits)) > 1 ){
    stop("Position units are not identical: \n",
         paste0(unique(x@posunits), collaspe = ", "), "!")
  }
  if(length(unique(x@zunits)) > 1){
    stop("Depth units are not identical: \n",
         paste0(unique(x@zunits), collaspe = ", "), "!\n")
  }
  # if(!all(grepl("[s]$", x@zunits))
  # x@zunits != "ns"  
  if(all(isLengthUnit(x)) && all(sapply(x@coords, length) > 0)){
    # elevation coordinates
    zmax <- sapply(x@coords, function(x) max(x[,3]))
    zmin <- sapply(x@coords, function(x) min(x[,3])) - max(x@dz) * max(x@ntraces)
    vz <- seq(from = min(zmin), to = max(zmax), by = min(x@dz))
  }else{
    # time/depth
    vz <- seq(from = 0, by = min(x@dz), length.out = max(x@nz))
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
                         extend = extend){
  if(!all(sapply(x@coords, length) > 0) ){
    stop("Some of the data have no coordinates. Please set first coordinates to all data.")
  }
  X <- x
  # default z-values (get elevation range)
  x_zi0 <- defVz(X)
  x_zi <- seq(min(x_zi0), by = dz, to = max(x_zi0))
  if(all(isLengthUnit(X)) ){
    x_zi <- sort(x_zi, decreasing = TRUE)
  }
  
  if(is.null(dx)){
    dx <- mean(sapply(x@coords, function(x) mean(diff(posLine(x)))))
    # nx <- (max(sapply(x@coords, function(x) max(x[,1]))) -
    #          min(sapply(x@coords, function(x) min(x[,1])) )) / dxy
    # nx <- round(nx)
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
    if(isLengthUnit(X[[i]])){
      if(length(unique(X[[i]]@coord[,3])) > 1){
        stop("The traces have different elevation!")
      } 
      x_z   <- X[[i]]@coord[1,3] - X[[i]]@depth
    }else{
      x_z   <- X[[i]]@depth
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
  
  # define bounding box + number of points for interpolation
  obb <- tpOBB2D(x)
  bbox <- c(min(obb[,1]), max(obb[,1]), min(obb[,2]), max(obb[,2]))
  
  
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
  SL <- array(dim = c(nx, ny, length(x_zi)))
  
  
  bbox[1] < min(xpos)
  
  j <- 1
  h = 6
  
  n <- 1
  m <- 1
  ratio_x_y <- bbox_dy / bbox_dx
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
    S <- MBA::mba.surf(cbind(xpos, ypos, val[[j]]), nx, ny, n = n, m = m, 
                       extend = extend, h = h)$xyz.est
    SL[,,j] <- S$z
  }
  return(list(x = S$x, y = S$y, z = SL, vz = x_zi, x0 = xpos, y0 = ypos, z0 = val))
  
}  

# x = GPRsurvey object
# dx = resolution along x-axis (e.g., 0.5 [m])
# dy = resolution along y-axis (e.g., 0.5 [m])
# dz = resolution along z-axis (e.g., 2 [ns])
# h = Number of levels in the hierarchical construction 
#     See the function 'mba.surf' of the MBA package
.sliceInterpOLD <- function(x, dx = NULL, dy = NULL, dz = NULL, h = 6){
  if(!all(sapply(x@coords, length) > 0) ){
    stop("Some of the data have no coordinates. Please set first coordinates to all data.")
  }
  X <- x
  # default z-values (get elevation range)
  x_zi0 <- defVz(X)
  x_zi <- seq(min(x_zi0), by = dz, to = max(x_zi0))
  if(all(isLengthUnit(X)) ){
    x_zi <- sort(x_zi, decreasing = TRUE)
  }
  
  if(is.null(dx)){
    dx <- mean(sapply(x@coords, function(x) mean(diff(posLine(x)))))
    # nx <- (max(sapply(x@coords, function(x) max(x[,1]))) -
    #          min(sapply(x@coords, function(x) min(x[,1])) )) / dxy
    # nx <- round(nx)
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
    if(isLengthUnit(X[[i]])){
      if(length(unique(X[[i]]@coord[,3])) > 1){
        stop("The traces have different elevation!")
      } 
      x_z   <- X[[i]]@coord[1,3] - X[[i]]@depth
    }else{
      x_z   <- X[[i]]@depth
    }
    x_data <- X[[i]]@data
    x_data[is.na(x_data)] <- 0
    # interpolation
    V[[i]] <- apply(x_data, 2, trInterp, z = x_z, zi = x_zi )
    # Z[[i]] <- x_zi   # X[[i]]@depth
  }
  
  # vj <- seq(dz, by = dz, to = length(x_zi))
  
  val <- list()
  # vz <- x_zi[vj]
  xpos <- unlist(lapply(X@coords, function(x) x[,1]))
  ypos <- unlist(lapply(X@coords, function(x) x[,2]))
  nx <- abs(diff(range(xpos))) / dx
  ny  <- abs(diff(range(ypos))) / dy
  SL <- array(dim = c(nx, ny, length(x_zi)))
  
  for(j in  seq_along(x_zi)){
    # j <- vj[u]
    #z <- rep(sapply(Z, function(x, i = j) x[i]), sapply(V, ncol))
    val[[j]] <- unlist(lapply(V, function(v, k = j) v[k,]))
    S <- MBA::mba.surf(cbind(xpos, ypos, val[[j]]), nx, ny, n = 1L, m = 1L, 
                       extend = TRUE, h = h)$xyz.est
    SL[,,j] <- S$z
  }
  return(list(x = S$x, y = S$y, z = SL, vz = x_zi, x0 = xpos, y0 = ypos, z0 = val))
}
  
.sliceInterp_old <- function(x, dx = NULL, dy = NULL, dz = NULL, h = 6){
  if(!all(sapply(x@coords, length) > 0) ){
    stop("Some of the data have no coordinates. Please set first coordinates to all data.")
  }
  X <- x
  x_zi <- defVz(X)
  if(all(isLengthUnit(X)) ){
    x_zi <- sort(x_zi, decreasing = TRUE)
  }
  if(is.null(dx)){
    dx <- mean(sapply(x@coords, function(x) mean(diff(posLine(x)))))
    # nx <- (max(sapply(x@coords, function(x) max(x[,1]))) -
    #          min(sapply(x@coords, function(x) min(x[,1])) )) / dxy
    # nx <- round(nx)
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
    if(isLengthUnit(X[[i]])){
      if(length(unique(X[[i]]@coord[,3])) > 1){
        stop("The traces have different elevation!")
      } 
      x_z   <- X[[i]]@coord[1,3] - X[[i]]@depth
    }else{
      x_z   <- X[[i]]@depth
    }
    x_data <- X[[i]]@data
    x_data[is.na(x_data)] <- 0
    # interpolation
    V[[i]] <- apply(x_data, 2, trInterp, z = x_z, zi = x_zi )
    # Z[[i]] <- x_zi   # X[[i]]@depth
    
  }
  vj <- seq(dz, by = dz, to = length(x_zi))
  
  val <- list()
  vz <- x_zi[vj]
  xpos <- unlist(lapply(X@coords, function(x) x[,1]))
  ypos <- unlist(lapply(X@coords, function(x) x[,2]))
  nx <- abs(diff(range(xpos))) / dx
  ny  <- abs(diff(range(ypos))) / dy
  SL <- array(dim = c(nx, ny, length(vj)))
  for(u in  seq_along(vj)){
    j <- vj[u]
    #z <- rep(sapply(Z, function(x, i = j) x[i]), sapply(V, ncol))
    val[[u]] <- unlist(lapply(V, function(u, k = j) u[k,]))
    S <- MBA::mba.surf(cbind(xpos, ypos, val[[u]]), nx, ny, n = 1, m = 1, 
                       extend = TRUE, h = h)$xyz.est
    SL[,,u] <- S$z
  }
  return(list(x = S$x, y = S$y, z = SL, vz = vz, x0 = xpos, y0 = ypos, z0 = val))
}


#' Interpolate horizontal slices
#'
#' @name interpSlices 
#' @rdname interpSlices
#' @export
setMethod("interpSlices", "GPRsurvey", function(x, 
                                                dx = NULL, 
                                                dy = NULL, 
                                                dz = NULL, 
                                                h = 6,
                                                extend = TRUE){
  
  if(is.null(dx) || is.null(dy) || is.null(dz)){
    stop("'dx', 'dy' and 'dz' must all be defined!")
  }
  if( dx <= 0 || dy <= 0 || dz <= 0 ){
    stop("'dx', 'dy' and 'dz' must all be strickly positive!")
  }
  
  SXY <- .sliceInterp(x = x, dx = dx, dy = dy, dz = dz, h = h,
                      extend = extend)
  
  xyref <- c(min(SXY$x), min(SXY$y))
  xpos <- SXY$x - min(SXY$x)
  ypos <- SXY$y - min(SXY$y)
  
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
           version      = "0.1",
           name         = "",
           date         = as.character(Sys.Date()),  
           freq         = xfreq,
           filepaths    = x@filepaths,
           x            = xpos,
           y            = ypos,
           data         = ddata,
           coord        = xyref,
           posunit      = x@posunits[1],
           crs          = x@crs,
           depth        = SXY$vz,
           depthunit    = x@zunits[1],
           #vel         = "list",               
           #delineations = "list",
           obs          = list(x = SXY$x0,
                               y = SXY$y0,
                               z = SXY$z0)
           #transf       = "numeric"
          )
   return(y)
})
