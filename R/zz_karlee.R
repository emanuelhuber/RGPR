
#==============================================================================#
# NOTE: function starting with a dot (e.g., '.myFunction()') are "private"
# (they are used internally by the main function) and therefore, you should
# not use them.


#----------------- FUNCTION TO INTERPOLATE DELINEATIONS -----------------------#

# x = GPR object with delineations
# extrap = should the delineation be extrapolated over all the traces
# (I recommend you to set extrap = NA and 
# to "correctly" delineate the interfaces)
# method = method for the interpolation (see ?signal::interp1)
# RETURN a nxm-matrix with n = number of delineations, m = number of traces
#   the rows are equal to the position of the interface in time
#   and with NA values where the interface does not exist.
interpInterface <- function(x, extrap = TRUE, method = "pchip", clean = TRUE){
  
  int <- RGPR:::.getXYZrelIntp(x, method = "pchip") # not necessary in fact (for 2D)
  
  int_xy <- sapply(int, .extrapInterface, x = x, 
                   extrap = extrap, method = method)
  int_xy <- t(int_xy)
  if(isTRUE(clean)){
    int_xy <- apply(int_xy, 2, .rmOlder, n = nrow(int_xy))
  }
  if(is.null(dim(int_xy))){
    dim(int_xy) <- c(1, length(int_xy))
  }
  return(int_xy)
}

# private function
.extrapInterface <- function(int, x, extrap = TRUE, method = "pchip"){
  # FIX - 20200516
  test <- diff(int[, "xrel"]) > sqrt(.Machine$double.eps)
  int <- int[test, ]
  #--- end fix
  signal::interp1(x = int[, "xrel"],
                  y = int[, "zrel"],
                  xi = pos(x),
                  extrap = extrap,
                  method = method)
}

# private function
.rmOlder <- function(z, n){
  # n <- length(z)
  for(i in seq_len(n-1)){
    z[(i+1):n][which(z[(i+1):n] < z[i])] <- NA
  }
  return(z)
}


#----------------- FUNCTION TO SET THE VELOCITY MODEL v(x, t) -----------------#

# x = GPR object with delineations
# twt_int = output of the function "interpInterface()"
# v = velocities (length(v) = nrow(twt_int + 1))  
#     (+1 because of the last layers)
# RETURN: a GPR object (like x) but with values equal to the velocity at (x, t)
getVel <- function(x, twt_int, v){
  if(nrow(twt_int) != length(v) - 1){
    stop("'length(v)' must be equal to ", nrow(twt_int) + 1)
  }
  x[] <- apply(twt_int, 2, .getVel, t_twt = depth(x), v = v)
  return(x)
}

# private function
.getVel <- function(z, t_twt, v){
  k <- is.na(z)
  u <- findInterval(t_twt, z[!k])
  # plot(u, t_twt, type = "l")
  v_u <- c(v[-length(v)][!k], v[length(v)])
  v_u[u+1]
}


#----------------- FUNCTION TO GET THE DEPTH as function of v(x, t) -----------#
# x = GPR object
# x_vel = output of the function "getVel()"
# RETURN: a GPR object (like x) but with values equal to the depth at (x, t)
getDepth <- function(x, x_vel){
  X <- c(0, diff(depth(x))) * x_vel/2
  X[] <- apply(X@data, 2, cumsum)
  return(X)
}



#----------------- FUNCTION FOR TIME-TO-DEPH CONVERSION -----------------------#

# Special function to solve your problem
# x = GPR object
# x_vel = output of the function "getVel()"
# dz = depth resolution for the time to depth conversion. If dz = NULL, then
#      dz is set equal to the smallest depth resolution computed from x_depth.
# d_max = maximum depth for the time to depth conversion. If d_max = NULL, then
#         d_max is set equal to the largest depth in x_depth.
# method = method for the interpolation (see ?signal::interp1)
timeToDepthVel <- function(x, 
                           x_vel, 
                           dz = NULL, 
                           d_max = NULL, 
                           method = "pchip"){
  x_depth <- getDepth(x, x_vel)
  if(is.null(d_max)) d_max <- max(x_depth)
  if(is.null(dz)) dz <- min(apply(x_depth, 2, diff))
  d <- seq(from = 0, by = dz, to = d_max)
  x_new <- matrix(nrow = length(d), ncol = ncol(x))
  for(i in seq_along(x)){
    x_new[, i] <- signal::interp1(x  = as.numeric(x_depth[,i]),
                                  y  = as.numeric(x[,i]),
                                  xi = d,
                                  method = method)
  }
  x@data      <- x_new
  x@depth     <- d
  x@dz        <- dz
  x@depthunit <- "m"
  return(x)
}


#----------------- FUNCTION FOR TIME-TO-DEPH CONVERSION OF THE INTERFACES -----#

# twt_int = output of the function "interpInterface()"
# v = velocities (length(v) = nrow(twt_int + 1))  
#     (+1 because of the last layers)
# RETURN: matrix of the same dimensions as twt_int, but indicating the depth
#         of the interfaces
getDepthInterface <- function(twt_int, v){
  apply(twt_int, 2, .getDepthInt, v[-length(v)])
}

.getDepthInt <- function(z, v){
  k <- !is.na(z)
  z[k] <- cumsum(c(z[k][1], diff(z[k])) * v[k]/2)
  return(z)
}