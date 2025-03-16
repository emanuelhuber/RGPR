## FIXME: clean the code!

# To add the topography, 
# use [migrate()] instead. This is a non-linear operation


#' Time to depth conversion
#' 
#' Convert the two-way travel time of the recorded waves into depth. It does
#' not account for the topography. 
#' @param x      (`GPR* object`) An object of the class `GPR`
#' @param dz     (`numeric[1]`) Desired depth resolution. 
#'               If `dz = NULL`, then `dz` is set equal 
#'               to the smallest depth resolution inferred from the data.
#' @param zmax   (`numeric[1]`) Maximum Desired depth.
#'               If `zmax = NULL`, then `zmax` is set 
#'               equal to the largest depth inferred from the data.
#' @param method (`character[1]`) Interpolation method to be applied:
#'               one of `pchip`, `linear`, `nearest`, 
#'               `spline`, or `cubic`
#'               (see also [signal::interp1()]). 
#' @return (`GPR* object`) with signal as a function of depth.
#' @name convertTimeToDepth
#' @concept signal processing
#' @export
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
  if(length(x@vel) == 0) stop(msg_no_vel)
  if(!isZTime(x))    stop(msg_set_zunitToDepth)
  
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
    d <- seq(from = 0, by = dz, to = zmax)
    funInterp123 <- function(A, x_depth, x_depth_int, method){
      signal::interp1(x = x_depth, y = A, xi = x_depth_int, 
                      method = method)
    }
    x@data <- apply(x@data, 2, funInterp123, 
                    x_depth = x_depth, 
                    x_depth_int = d, 
                    method = method)
    x@z     <- d
  # matrix velocity
  }else if(is.matrix(x_vel)){
    x_depth <- timeToDepth(twt = x@z, t0 = 0, v = x_vel, 
                           antsep = x@antsep) 
    if(is.null(dz)){
      dz <- min(x_vel) * min(diff(x@z))/2
    }
    if(is.null(zmax)){
      zmax <- max(x_depth, na.rm = TRUE)
    }
    d <- seq(from = 0, by = dz, to = zmax)
    x_new <- matrix(nrow = length(d), ncol = ncol(x))
    n <- nrow(x)
    for(i in seq_along(x)){
      # xNA <- is.na(x_depth[,i])
      # ni <- max(which(xNA))+1
      # vi <- ni:n
      vi <- !is.na(x_depth[,i])
      x_new[, i] <- signal::interp1(x  = x_depth[vi,i],
                                    y  = x@data[vi,i],
                                    xi = d,
                                    method = method, extrap = FALSE)
    }
    if(length(x@delineations) > 0){
      x@delineations <- convertTimeToDepthDelineation(x@delineations, d, x_depth)
    }
    if(!is.null(x@md$velocity_interfaces) && length(x@md$velocity_interfaces) ){
      x@md$velocity_interfaces <- convertTimeToDepthDelineation(x@md$velocity_interfaces, d, x_depth)
    }
      
     
    
    x@data <- x_new
    x@z    <- d
    x@vel <- list()
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


convertTimeToDepthDelineation <- function(del, d, x_depth){
  if(inherits(del, "list")){
    del <- lapply(del, convertTimeToDepthDelineation, d, x_depth)
    return(del)
  }else{
    del_d <- x_depth[cbind(del[,"j"],del[,"i"])]
    del[,"j"] <- closest(del_d, d)
    return(del)
  }
}
