

#' @name plotVelocityLayers
#' @rdname setLayerVelocities
#' @export
setGeneric("plotVelocityLayers", 
           function(x, 
                    method = c("linear", "nearest", "pchip", 
                               "cubic", "spline", "none"), 
                    col = NULL, ...) 
             standardGeneric("plotVelocityLayers"))

#' Plot the delineation on a 2D plot
#'
#' @name plotVelocityLayers
#' @rdname setLayerVelocities
#' @export
setMethod("plotVelocityLayers", "GPR", 
          function(x, 
                   method = c("linear", "nearest", "pchip", 
                              "cubic", "spline", "none"), 
                   col = NULL, ...){
            x@delineations <- x@hd[["velocity_interfaces"]] 
            plotDelineations(x, method = method, col = col, ...)
})


#' @name setLayerVelocities
#' @rdname setLayerVelocities
#' @export
setGeneric("setLayerVelocities", function(x, v, twt = NULL, method = "pchip", 
                                         clean = TRUE) 
  standardGeneric("setLayerVelocities"))



#' Set layer velocities
#' 
#' Given delineation or the output of the function... and velocity values,
#' set 2D velocity model
#'
#' @param x GPR object   
#' @param v [\code{numeric}] velocities (length equal number of delineations plus one)
#' @param twt        vertical resolution of the migrated data
#' @param method [\code{character(1)}] interpolation method. 
#'               One of \code{linear}, \code{nearest}, \code{pchip},
#'               \code{cubic}, \code{spline}.
#' @name setLayerVelocities
#' @rdname setLayerVelocities
#' @export
setMethod("setLayerVelocities", "GPR", function(x, v, twt = NULL, 
                                                method = "pchip", clean = TRUE){
  if(is.null(twt)){
    twt <- interpInterface(x, extrap = extrap, method = method, clean = clean)
  }
  # twt
  x_del <- (apply(twt, 1, .twt_to_delineations, dtt = x@dz))
  
  # FUN <- function(x){
  #   if(is.list(x[[1]])){
  #     unlist(x, recursive = FALSE)
  #   }else{
  #     x[[1]]
  #   }
  # }
  # 
  # x@delineations <- lapply(x_del, FUN)
  x@hd[["velocity_interfaces"]] <- unlist(x_del, recursive = FALSE)
  x@vel <- list(.getVelFromInterface(x, twt, v))
  return(x)
})


.twt_to_delineations <- function(x, dtt){
  # idx <- seq_along(x)
  # idx[is.na(x)] <- NA
  # idx_splt <- splitVectAtNA(idx)
  # 
  # u <- lapply(idx_splt, .makeMatrixDel, x, dtt)
  # names(u) <- NULL
  # if(length(u) > 1) return(u)
  # return(u[[1]])
  tst <- !is.na(x)
  u <- matrix(nrow = length(x), ncol = 2)
  # print(dim(u))
  u[, 1] <- seq_along(x)
  u[tst, 2] <- 1L + round(x[tst]/dtt)
  u[!tst, 2] <- NA
  # u <- cbind(which(tst), round(x[tst]/dtt))
  colnames(u) <- c("i", "j")
  return(list(u))
}
# .twt_to_delineations <- function(x, dtt){
#   idx <- seq_along(x)
#   idx[is.na(x)] <- NA
#   idx_splt <- splitVectAtNA(idx)
#   
#   u <- lapply(idx_splt, .makeMatrixDel, x, dtt)
#   names(u) <- NULL
#   if(length(u) > 1) return(u)
#   return(u[[1]])
#   # tst <- !is.na(x)
#   # u <- matrix(nrow = length(x), ncol = 2)
#   # print(dim(u))
#   # u[, 1] <- seq_along(x)
#   # u[tst, 2] <- round(x[tst]/dtt)
#   # u[!tst, 2] <- NA
#   # # u <- cbind(which(tst), round(x[tst]/dtt))
#   # colnames(u) <- c("i", "j")
#   # return(list(u))
# }

.makeMatrixDel <- function(x, y, dtt){
  u <- matrix(nrow = length(x), ncol = 2)
  u[, 1] <- x
  u[, 2] <- round(y[x]/dtt)
  colnames(u) <- c("i", "j")
  return(u)
}

splitVectAtNA <- function( x ){
  idx <- 1 + cumsum( is.na( x ) )
  not.na <- ! is.na( x )
  split( x[not.na], idx[not.na] )
}
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
  
  int <- .getXYZrelIntp(x, method = method) # not necessary in fact (for 2D)
  
  twt <- matrix(nrow = length(int), ncol = ncol(x))
  for(i in seq_along(int)){
    twt[i, int[[i]][, "i"]] <- int[[i]][,"zrel"]
  }
  
  if(isTRUE(clean)){
    twt <- apply(twt, 2, .rmOlder, n = nrow(twt))
  }
  if(is.null(dim(twt))){
    dim(twt) <- c(1, length(twt))
  }
  return(twt)
  
  # int <- .getXYZrelIntp(x, method = method) # not necessary in fact (for 2D)
  # int_xy <- sapply(int, .extrapInterface, x = x, 
  #                  extrap = extrap, method = method)
  # int_xy <- t(int_xy)
  # if(isTRUE(clean)){
  #   int_xy <- apply(int_xy, 2, .rmOlder, n = nrow(int_xy))
  # }
  # if(is.null(dim(int_xy))){
  #   dim(int_xy) <- c(1, length(int_xy))
  # }
  # return(int_xy)
}

# # private function
# .extrapInterface <- function(int, x, extrap = TRUE, method = "pchip"){
#   # no interpolation required
#   if(nrow(int) == ncol(x) && all(int[, "xrel"] == relTrPos(x) )){ 
#     return(int[, "zrel"])
#   }else{
#     # FIX - 20200516
#     # int <- int[!is.na(int[, "y"]), ]
#     test <- diff(int[, "xrel"]) > sqrt(.Machine$double.eps)
#     int <- int[test, ]
#     #--- end fix
#     signal::interp1(x = int[, "xrel"],
#                     y = int[, "zrel"],
#                     xi = relTrPos(x),
#                     extrap = extrap,
#                     method = method)
#   }
# }

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
