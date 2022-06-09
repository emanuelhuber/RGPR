
#' @name convertTimeToDepth
#' @rdname convertTimeToDepth
#' @export
setGeneric("convertTimeToDepth", function(x, dz = NULL, zmax = NULL, 
                                          method = c("pchip", "linear", "nearest", "cubic", "spline")) 
  standardGeneric("convertTimeToDepth"))


# max_depth = to which depth should the migration be performed
# dz = vertical resolution of the migrated data
# fdo = dominant frequency of the GPR signal

# for static time-to-depth migration 
# dz = depth resolution for the time to depth conversion. If dz = NULL, then
#      dz is set equal to the smallest depth resolution computed from x_depth.
# d_max = maximum depth for the time to depth conversion. If d_max = NULL, then
#         d_max is set equal to the largest depth in x_depth.
# method = method for the interpolation (see ?signal::interp1)

#' Time to depth conversion
#' 
#' Convert the two-way travel time of the recorded waves into depth. It does
#' not account for the topography. To add the topography, 
#' use \code{\link{migrate}} instead.
#' 
#' @param dz        vertical resolution of the migrated data. 
#'                  If \code{dz = NULL}, then \code{dz} is set equal to the 
#'                  smallest depth resolution inferred from the data.
#' @param zmax      maximum depth for the time to depth conversion. If 
#'                  \code{zmax = NULL}, then \code{zmax} is set equal to the 
#'                  largest depth inferred from the data.
#' @param method    method for the interpolation 
#'                  (see \code{\link[signal]{interp1}}).
#' 
#' @name convertTimeToDepth
#' @rdname convertTimeToDepth
#' @export
setMethod("convertTimeToDepth", "GPR", function(x, dz = NULL, zmax = NULL, 
                                                method = c("pchip", "linear", "nearest", "cubic", "spline")){
  if(is.null(x@vel) || length(x@vel)==0){
    stop("You must first define the EM wave velocity ",
         "with 'vel(x) <- 0.1' for example!")
  }else{
    x_vel <- .getVel2(x, type = "vint", strict = FALSE)
  }
  if(length(x@coord) != 0 && ncol(x@coord) == 3){
    topo <- x@coord[1:ncol(x@data), 3]
  }else{
    topo <- rep.int(0L, ncol(x@data))
    message("Trace vertical positions set to zero!")
  }
  
  if(any(x@time0 != 0)){
    x <- time0Cor(x, method = c("pchip"))
  }
  
  if( !isTimeUnit(x) ){
    stop("Vertical unit (", x@depthunit , ") is not a time unit...")
  }
    
  # dots <- list(...)
  # if( is.null(dz)){
  #   dz <- min(x@vel[[1]]) * min(diff(depth(x)))/2
  # }
  # print(zmax)
  method <- match.arg(method, c("pchip", "linear", "nearest", "cubic", "spline"))
    
  # single velocity value
  if(length(x_vel) == 1){
    message("time to depth conversion with constant velocity (", x_vel,
            " ", x@posunit, "/", x@depthunit, ")")
    x_depth <- timeToDepth(x@depth, time_0 = 0, v = x_vel, 
                     antsep = antsep(x))
    test <- !is.na(x_depth)
    x <- x[test,]
    x_depth <- x_depth[test]
    
    if(is.null(dz)){
      x@dz <-  x@dz * x_vel/ 2
    }else{
      x@dz <- dz
    }
    
    if( is.null(zmax)){
      zmax <- max(x_depth, na.rm = TRUE)
    }
    
    # x@dz <-  x@dz * x@vel[[1]]/ 2
    # x@depth <- seq(from = 0, to = tail(z, 1), by = x@dz)
    d <- seq(from = 0, to = zmax, by = dz)
    funInterp <- function(xt, x_depth, x_depth_int, method){
      signal::interp1(x = x_depth, y = xt, xi = x_depth_int, 
                      method = method, extrap = TRUE)
    }
    x@data <- apply(x@data, 2, funInterp, 
                    x_depth = x_depth, 
                    x_depth_int = d, 
                    method = method)
  # vector velocity
  }else if( is.null(dim(x_vel)) && length(x_vel) == nrow(x) ){
    x_depth <- timeToDepth(x@depth, 0, v = x_vel, 
                           antsep = x@antsep) # here difference to matrix case
    test <- !is.na(x_depth)
    x <- x[test,]
    x_depth <- x_depth[test]
    if(is.null(dz)){
      dz <- min(x_vel) * min(diff(x@depth))/2
    } 
    if( is.null(zmax)){
      zmax <- max(x_depth, na.rm = TRUE)
    }
    
    d <- seq(from = 0, to = zmax, by = dz)
    funInterp <- function(xt, x_depth, x_depth_int, method){
      signal::interp1(x = x_depth, y = xt, xi = x_depth_int, 
                      method = method)
    }
    x@data <- apply(x@data, 2, funInterp, 
                    x_depth = x_depth, 
                    x_depth_int = d, 
                    method = method)
    
    # x_new <- matrix(nrow = length(d), ncol = ncol(x))
    # for(i in seq_along(x)){
    #   x_new[, i] <- signal::interp1(x  = x_depth,  # here difference to matrix case
    #                                 y  = as.numeric(x[,i]),
    #                                 xi = d,
    #                                 method = method)
    # }
    # x@data      <- x_new
    # x@depth     <- d
    # x@dz        <- dz
      
    # print("lkj")
  # matrix velocity
  }else if(is.matrix(x_vel)){
    x_depth <- apply(c(0, diff(depth(x))) * x_vel/2, 2, cumsum)
    # FIXME account for antenna separation -> and remove pixels with NA... not so easy..
    # x_depth <- apply(c(0, diff(depth(x))) * x@vel[[1]]/2, 2, cumsum)
    # x_detph <- x_depth^2 - antsep^2
    # test <- (x_detph >= 0)
    # x_detph[!test] <- NA
    # x_detph[test] <- sqrt(x_detph[test])/2
    if(is.null(dz)){
      dz <- min(x_vel) * min(diff(x@depth))/2
    }
    if( is.null(zmax)){
      zmax <- max(x_depth, na.rm = TRUE)
    }
    
    # if( !is.null(dots$dz)){
    #   dz <- dots$dz
    # }else{
    #   dz <- min(x@vel[[1]]) * min(diff(depth(x)))/2
    # }
    # if( !is.null(dots$zmax)){
    #   zmax <- dots$zmax
    # }else{
    #   zmax <- max(x_depth)
    # }
    # if( !is.null(dots$method)){
    #   method <- match.arg(dots$method, c("linear", "nearest", "pchip", "cubic", "spline"))
    # }else{
    #   method <- "pchip"
    # }
    d <- seq(from = 0, by = dz, to = zmax)
    x_new <- matrix(nrow = length(d), ncol = ncol(x))
    for(i in seq_along(x)){
      x_new[, i] <- signal::interp1(x  = as.numeric(x_depth[,i]),
                                    y  = as.numeric(x[,i]),
                                    xi = d,
                                    method = method)
    }
    x@data      <- x_new
  }
  x@depth     <- d
  x@dz        <- dz
  x@depthunit <- "m"
  
  zShift <- (max(topo) - topo)
  if( all(zShift != 0) ){
    x <- traceShift(x,  ts = zShift, method = c("pchip"), crop = FALSE)
  }
  if(length(x@coord) > 0 && ncol(x@coord) == 3 ){
    x@coord[, 3] <- max(x@coord[,3])
  }
  x@vel <- list() 

  proc(x) <- getArgs()
  return(x)
} 
)


