
#' @name setLayerVelocities
#' @rdname setLayerVelocities
#' @export
setGeneric("setLayerVelocities", function(x, v, twt = NULL, method = "pchip", 
                                         clean = TRUE, extrap = NA) 
  standardGeneric("setLayerVelocities"))



#' Set layer velocities
#'
#' @param x GPR object   
#' @param v velocities (\code{length(v) = nrow(twt_int + 1)}, (\code{+1} because of the last layers)
#' @param twt        vertical resolution of the migrated data
#' @param method       interpolation method
#' @name setLayerVelocities
#' @rdname setLayerVelocities
#' @export
setMethod("setLayerVelocities", "GPR", function(x, v, twt = NULL, 
                                               method = "pchip", clean = TRUE, 
                                               extrap = NA){
  if(is.null(twt)){
    twt <- interpInterface(x, extrap = extrap, method = method, clean = clean)
  }
  # twt
  # x@delineations <- twt is a matrix
  x@vel <- list(.getVelFromInterface(x, twt, v))
  return(x)
})




# x = GPR object with delineations
# extrap = should the delineation be extrapolated over all the traces
# (I recommend you to set extrap = NA and 
# to "correctly" delineate the interfaces)
# method = method for the interpolation (see ?signal::interp1)
# RETURN a nxm-matrix with n = number of delineations, m = number of traces
# the rows are equal to the position of the interface in time
# and with NA values where the interface does not exist.

#' @export
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
.getVelFromInterface <- function(x, twt_int, v){
   if(nrow(twt_int) != length(v) - 1){
     stop("'length(v)' must be equal to ", nrow(twt_int) + 1)
   }
   xvel <- apply(twt_int, 2, .getVel, t_twt = depth(x), v = v)
   return(xvel)
}

# private function
.getVel <- function(z, t_twt, v){
  k <- is.na(z)
  u <- findInterval(t_twt, z[!k])
  # plot(u, t_twt, type = "l")
  v_u <- c(v[-length(v)][!k], v[length(v)])
  v_u[u+1]
}
