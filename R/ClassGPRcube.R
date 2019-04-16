
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
    freq = "numeric",            # antenna frequency (if unique)

    filepaths = "character",     # filepath of the profile
    
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





#' Class GPRcube
#' 
#' An S4 class to represent 3D ground-penetrating radar (GPR) data.
#' 
#' @name GPRcube-class
#' @rdname GPRcube-class
#' @export
setClass(
  Class="GPRcube",  
  #### extend= "GPRsurvey"
  slots=c(
    version = "character",      # version of the class
    
    date = "character",          # date of creation, format %Y-%m-%d
    freq = "numeric",            # antenna frequency (if unique)
    
    filepaths = "character",     # filepath of the profile
    
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

#------------------------------
# "["
#' extract parts of GPRsurvey
#'
#' Return an object of class GPR slice
#' @name GPRcube-subset
#' @docType methods
#' @rdname GPRcube-subset
setMethod(
  f= "[",
  signature="GPRsurvey",
  definition=function(x,i,j,drop){
    if(missing(i)) i <- j
    # cat(typeof(i),"\n")
    # cat(j,"\n")
    # i <- as.numeric(i)
    y <- x
    y@filepaths      <- x@filepaths[i]
    y@names          <- x@names[i]
    y@descriptions   <- x@descriptions[i]
    y@freqs          <- x@freqs[i]
    y@lengths        <- x@lengths[i]
    y@surveymodes    <- x@surveymodes[i]
    y@dates          <- x@dates[i]
    y@antseps        <- x@antseps[i]
    y@crs            <- x@crs[i]
    y@coords         <- x@coords[x@names[i]]
    y@fids           <- x@fids[x@names[i]]
    y@intersections  <- x@intersections[x@names[i]]
    y@ntraces        <- x@ntraces[i]
    y@nz             <- x@nz[i]
    y@dz             <- x@dz[i]
    y@zunits         <- x@zunits[i]
    y@posunits       <- x@posunits[i]
    return(y)
  }
)


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
  xi <- signal::interp1(x = z[!isNA], y = x[!isNA], xi = zi, method = "spline", extrap = 0)
  return(xi)
}

# x = GPRsurvey object
# nx = resolution along x-axis (e.g., 0.5 [m])
# ny = resolution along y-axis (e.g., 0.5 [m])
# nz = resolution along z-axis (e.g., 2 [ns])
# h = Number of levels in the hierarchical construction 
#     See the function 'mba.surf' of the MBA package
.sliceInterp <- function(x, nx, ny, nz, h = 6){
  if(!all(sapply(x@coords, length) > 0) ){
    stop("Some of the data have no coordinates. Please set first coordinates to all data.")
  }
  X <- x
  x_zi <- defVz(X)
  if(all(isLengthUnit(X)) ){
    x_zi <- sort(x_zi, decreasing = TRUE)
  }
  xpos <- unlist(lapply(X@coords, function(x) x[,1]))
  ypos <- unlist(lapply(X@coords, function(x) x[,2]))
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
  vj <- seq(nz, by = nz, to = length(x_zi))
  
  SL <- array(dim = c(nx, ny, length(vj)))
  val <- list()
  vz <- x_zi[vj]
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
setMethod("interpSlices", "GPRsurvey", function(x, nx, ny, dz, h = 6){
  SXY <- .sliceInterp(x = x, nx = nx, ny = ny, nz = dz, h = h)
  
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
  
  x <- new("GPRcube",
           version      = "0.1",
           date         = as.character(Sys.Date()),  
           freq         = xfreq,
           filepaths    = x@filepaths,
           x            = xpos,
           y            = ypos,
           data         = SXY$z,
           coord        = xyref,
           posunit      = x@posunits[1],
           crs          = x@crs,
           depth        = SXY$vz,
           depthunit    = x@posunits[1],
           #vel         = "list",               
           #delineations = "list",
           obs          = list(x = SXY$x0,
                               y = SXY$y0,
                               z = SXY$z0)
           #transf       = "numeric"
          )
  
})
