## FIXME: clean the code!


#' Time to depth conversion
#' 
#' Convert the two-way travel time of the recorded waves into depth. It does
#' not account for the topography. To add the topography, 
#' use \code{\link{migrate}} instead. This is a non-linear operation
#' @param x      [\code{GPR* object}] An object of the class \code{GPR}
#' @param dz     [\code{numeric(1)}] Desired depth resolution. 
#'               If \code{dz = NULL}, then \code{dz} is set equal 
#'               to the smallest depth resolution inferred from the data.
#' @param zmax   [\code{numeric(1)}] Maximum Desired depth.
#'               If \code{zmax = NULL}, then \code{zmax} is set 
#'               equal to the largest depth inferred from the data.
#' @param method [\code{character(1)}] Interpolation method to be applied:
#'               one of \code{pchip}, \code{linear}, \code{nearest}, 
#'               \code{spline}, or \code{cubic}
#'               (see also \code{\link[signal]{interp1}}). 
#' @return [\code{GPR* object}] with signal as a function of depth.
#' @name convertTimeToDepth
#' @concept signal processing
setGeneric("convertTimeToDepth", 
           function(x, dz = NULL, zmax = NULL,
                    method = c("pchip", "linear","nearest", "spline", "cubic")) 
  standardGeneric("convertTimeToDepth"))


#' @rdname convertTimeToDepth
#' @export
setMethod("convertTimeToDepth", "GPR", function(x, dz = NULL, zmax = NULL, 
                                                method = c("pchip", "linear", 
                                                           "nearest", "spline", 
                                                           "cubic")){
  method <- match.arg(method[1], c("pchip", "linear", "nearest", 
                                   "spline", "cubic"))
  if(is.null(x@vel) || length(x@vel)==0 ){ 
    stop("You must first define the EM wave velocity ",
         "with 'vel(x) <- 0.1' for example!")
  }
  if( !isZTime(x) ){
    stop("Vertical unit (", x@zunit , ") is not a time unit...")
  }
  
  if(length(x@coord) != 0 && ncol(x@coord) == 3){
    topo <- x@coord[1:ncol(x@data), 3]
  }else{
    topo <- rep.int(0L, ncol(x@data))
    message("Trace vertical positions set to zero!")
  }
  
  if(any(x@z0 != 0)){
    x <- shiftToTime0(x, method = c("pchip"))
  }
  
  x_vel <- .getVel(x, type = "vint", strict = FALSE)
  
  x[is.infinite(x) | is.na(x)] <- 0
  
  # single velocity value
  if(length(x_vel) == 1){
    message("time to depth conversion with constant velocity (", x_vel,
            " ", x@xunit, "/", x@zunit, ")")
    z <- timeToDepth(twt = x@z, t0 = 0, v = x_vel, 
                     antsep = antsep(x))
    test <- !is.na(z)
    x <- x[test,]
    if(is.null(dz)){
      dz <- min(x_vel) * min(diff(x@z))/2
    }
    if(is.null(zmax)){
      zmax <- max(z, 1, na.rm = TRUE)
    }
    x@z <- seq(from = 0, to = zmax, by = dz)
    funInterp321 <- function(x, z, zreg, method){
      signal::interp1(x = z, y = x, xi = zreg, 
                      method = method, extrap = TRUE)
    }
    x@data <- apply(x@data, 2, funInterp321, 
                    z = z[test], zreg = x@z, method = method)
    # vector velocity
  }else if( is.null(dim(x_vel)) && length(x_vel) == nrow(x) ){
    x_depth <- timeToDepth(twt = x@z, t0 = 0, v = x_vel, 
                           antsep = x@antsep) # here difference to matrix case
    test <- !is.na(x_depth)
    x <- x[test,]
    x_depth <- x_depth[test]
    if(is.null(dz)){
      dz <- min(x_vel) * min(diff(x@z))/2
    }
    if( is.null(zmax)){
      zmax <- max(x_depth, na.rm = TRUE)
    }
    # print(zmax)
    d <- seq(from = 0, by = dz, to = zmax)
    funInterp123 <- function(A, x_depth, x_depth_int, method){
      signal::interp1(x = x_depth, y = A, xi = x_depth_int, 
                      method = method)
    }
    x@data <- apply(x@data, 2, funInterp123, 
                    x_depth = x_depth, 
                    x_depth_int = d, 
                    method = method)
    # signal::interp1(x = x_depth, y = x@data[,2], xi = d, 
    #                 method = method)
    
    
    # x_new <- matrix(nrow = length(d), ncol = ncol(x))
    # for(i in seq_along(x)){
    #   x_new[, i] <- signal::interp1(x  = x_depth,  # here difference to matrix case
    #                                 y  = as.numeric(x[,i]),
    #                                 xi = d,
    #                                 method = method)
    # }
    # x@data      <- x_new
    x@z     <- d
    # x@dz        <- dz
    
    # print("lkj")
    # matrix velocity
  }else if(is.matrix(x_vel)){
    x_depth <- apply(c(0, diff(x@z)) * x_vel/2, 2, cumsum)
    # FIXME account for antenna separation -> and remove pixels with NA... not so easy..
    # x_depth <- apply(c(0, diff(depth(x))) * x@vel[[1]]/2, 2, cumsum)
    # x_detph <- x_depth^2 - antsep^2
    # test <- (x_detph >= 0)
    # x_detph[!test] <- NA
    # x_detph[test] <- sqrt(x_detph[test])/2
    if(is.null(dz)){
      dz <- min(x_vel) * min(diff(x@z))/2
    }
    if(is.null(zmax)){
      zmax <- max(x_depth)
    }
    d <- seq(from = 0, by = dz, to = zmax)
    x_new <- matrix(nrow = length(d), ncol = ncol(x))
    for(i in seq_along(x)){
      x_new[, i] <- signal::interp1(x  = as.numeric(x_depth[,i]),
                                    y  = as.numeric(x[,i]),
                                    xi = d,
                                    method = method)
    }
    x@data <- x_new
    x@z    <- d
  }
  
  # FIXME
  # zShift <- (max(topo) - topo)
  # if( all(zShift != 0) ){
  #   x <- traceShift(x,  ts = zShift, method = c("pchip"), crop = FALSE)
  # }
  # if(length(x@coord) > 0 && ncol(x@coord) == 3 ){
  #   x@coord[, 3] <- max(x@coord[,3])
  # }
  
  # x@vel <- list()  # keep velocity model
  x@zunit <- x@xunit # FIXME: check that
  x@zlab <- "depth"
  proc(x) <- getArgs()
  return(x)
} 
)


