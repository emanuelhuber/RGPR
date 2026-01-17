## FIXME: clean the code!

# To add the topography, 
# use [migrate()] instead. This is a non-linear operation


#' Time to depth conversion
#' 
#' Convert the two-way travel time of the recorded waves into depth. It does
#' not account for the topography. 
#' @param obj      (`GPR* object`) An object of the class `GPR`
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
           function(obj, dz = NULL, zmax = NULL,
                    method = c("pchip", "linear","nearest", "spline", "cubic")) 
  standardGeneric("convertTimeToDepth"))


#' @rdname convertTimeToDepth
#' @export
setMethod("convertTimeToDepth", "GPR", function(obj, dz = NULL, zmax = NULL, 
                                                method = c("pchip", "linear", 
                                                           "nearest", "spline", 
                                                           "cubic")){
  method <- match.arg(method[1], c("pchip", "linear", "nearest", 
                                   "spline", "cubic"))
  if(length(obj@vel) == 0) stop(msg_no_vel)
  if(!isZTime(obj))    stop(msg_set_zunitToDepth)
  
  if(length(obj@coord) != 0 && ncol(obj@coord) == 3){
    topo <- obj@coord[1:ncol(obj@data), 3]
  }else{
    topo <- rep.int(0L, ncol(obj@data))
    message("Trace vertical positions set to zero!")
  }
  
  if(any(obj@z0 != 0)){
    obj <- shiftToTime0(obj, method = c("pchip"))
  }
  
  x_vel <- .getVel(obj, type = "vint", strict = FALSE)
  
  obj[is.infinite(obj) | is.na(obj)] <- 0
  
  # single velocity value
  if(length(x_vel) == 1){
    message("time to depth conversion with constant velocity (", x_vel,
            " ", obj@xunit, "/", obj@zunit, ")")
    z <- timeToDepth(twt = obj@z, t0 = 0, v = x_vel, 
                     antsep = antsep(obj))
    test <- !is.na(z)
    obj <- obj[test,]
    if(is.null(dz)){
      dz <- min(x_vel) * min(diff(obj@z))/2
    }
    if(is.null(zmax)){
      zmax <- max(z, 1, na.rm = TRUE)
    }
    obj@z <- seq(from = 0, to = zmax, by = dz)
    funInterp321 <- function(x, z, zreg, method){
      signal::interp1(x = z, y = x, xi = zreg, 
                      method = method, extrap = TRUE)
    }
    obj@data <- apply(obj@data, 2, funInterp321, 
                    z = z[test], zreg = obj@z, method = method)
    # vector velocity
  }else if( is.null(dim(x_vel)) && length(x_vel) == nrow(obj) ){
    x_depth <- timeToDepth(twt = obj@z, t0 = 0, v = x_vel, 
                           antsep = obj@antsep) # here difference to matrix case
    
    # FIXME > compute depth for the velocity model
    # obj@vel$xvrms$d <- timeToDepth(twt = obj@vel$xvrms$t, t0 = 0, v = x_vel, 
    #             antsep = obj@antsep) # here difference to matrix case
    
    test <- !is.na(x_depth)
    obj <- obj[test,]
    x_depth <- x_depth[test]
    if(is.null(dz)){
      dz <- min(x_vel) * min(diff(obj@z))/2
    }
    if( is.null(zmax)){
      zmax <- max(x_depth, na.rm = TRUE)
    }
    d <- seq(from = 0, by = dz, to = zmax)
    funInterp123 <- function(A, x_depth, x_depth_int, method){
      signal::interp1(x = x_depth, y = A, xi = x_depth_int, 
                      method = method)
    }
    obj@data <- apply(obj@data, 2, funInterp123, 
                    x_depth = x_depth, 
                    x_depth_int = d, 
                    method = method)
    obj@z     <- d
  # matrix velocity
  }else if(is.matrix(x_vel)){
    x_depth <- timeToDepth(twt = obj@z, t0 = 0, v = x_vel, 
                           antsep = obj@antsep) 
    if(is.null(dz)){
      dz <- min(x_vel) * min(diff(obj@z))/2
    }
    if(is.null(zmax)){
      zmax <- max(x_depth, na.rm = TRUE)
    }
    d <- seq(from = 0, by = dz, to = zmax)
    x_new <- matrix(nrow = length(d), ncol = ncol(obj))
    n <- nrow(obj)
    for(i in seq_along(obj)){
      # xNA <- is.na(x_depth[,i])
      # ni <- max(which(xNA))+1
      # vi <- ni:n
      vi <- !is.na(x_depth[,i])
      x_new[, i] <- signal::interp1(x  = x_depth[vi,i],
                                    y  = obj@data[vi,i],
                                    xi = d,
                                    method = method, extrap = FALSE)
    }
    if(length(obj@delineations) > 0){
      obj@delineations <- convertTimeToDepthDelineation(obj@delineations, d, x_depth)
    }
    if(!is.null(obj@md$velocity_interfaces) && length(obj@md$velocity_interfaces) ){
      obj@md$velocity_interfaces <- convertTimeToDepthDelineation(obj@md$velocity_interfaces, d, x_depth)
    }
      
     
    
    obj@data <- x_new
    obj@z    <- d
    obj@vel <- list()
  }
  
  # FIXME
  # zShift <- (max(topo) - topo)
  # if( all(zShift != 0) ){
  #   obj <- traceShift(obj,  ts = zShift, method = c("pchip"), crop = FALSE)
  # }
  # if(length(obj@coord) > 0 && ncol(obj@coord) == 3 ){
  #   obj@coord[, 3] <- max(obj@coord[,3])
  # }
  
  # obj@vel <- list()  # keep velocity model
  obj@zunit <- obj@xunit # FIXME: check that
  obj@zlab <- "depth"
  proc(obj) <- getArgs()
  return(obj)
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
