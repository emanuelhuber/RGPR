

#' Set layer velocities
#' 
#' Given delineation or the output of the function... and velocity values,
#' set 2D velocity model
#'
#' @param obj (`GPR* object`) An object of the class `GPR`
#' @param v (`numeric[n]`)  velocities (length `n` equal number of delineations plus one)
#' @param twt  (FIXME)      Two-way travel time (optinal)
#' @param method (`character[1]`)  interpolation method. 
#'               One of \code{linear}, \code{nearest}, \code{pchip},
#'               \code{cubic}, \code{spline}.
#' @param clean (`logical[1]`) When the interface crosses, should the older interfaces be clipped by the new ones?
#' @name velSetLayers
#' @rdname velSetLayers
#' @export
setGeneric("velSetLayers", function(obj, v, twt = NULL, method = "pchip", 
                                 clean = TRUE) 
  standardGeneric("velSetLayers"))



#' @rdname velSetLayers
#' @export
setMethod("velSetLayers", "GPR", function(obj, v, twt = NULL, method = "pchip", 
                                       clean = TRUE){
  if(is.null(twt)){
    twt <- interpInterface(obj, method = method, clean = clean)
  }else{
    if(is.null(dim(twt))) dim(twt) <- c(1, length(twt))
  }
  # twt
  dz = mean(diff(obj@z))
  obj_del <- (apply(twt, 1, .twt_to_delineations, dtt = dz))
  
  # FUN <- function(x){
  #   if(is.list(x[[1]])){
  #     unlist(x, recursive = FALSE)
  #   }else{
  #     x[[1]]
  #   }
  # }
  # 
  # x@delineations <- lapply(x_del, FUN)
  obj@md[["velocity_interfaces"]] <- unlist(obj_del, recursive = FALSE)
  obj@vel <- list("vint" = .getVelFromInterface(obj, twt, v))
  return(obj)
})



interpInterface <- function(x, method = "pchip", clean = TRUE){
  
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

# private function
.rmOlder <- function(z, n){
  # n <- length(z)
  for(i in seq_len(n-1)){
    z[(i+1):n][which(z[(i+1):n] < z[i])] <- NA
  }
  return(z)
}


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

# x = GPR object with delineations
# twt_int = output of the function "interpInterface()"
# v = velocities (length(v) = nrow(twt_int + 1))  
#     (+1 because of the last layers)
# RETURN: a GPR object (like x) but with values equal to the velocity at (x, t)
.getVelFromInterface <- function(x, twt_int, v){
  if(nrow(twt_int) != length(v) - 1){
    stop("'length(v)' must be equal to ", nrow(twt_int) + 1)
  }
  xvel <- apply(twt_int, 2, .getVelLayer, t_twt = x@z, v = v)
  return(xvel)
}

# private function
.getVelLayer <- function(z, t_twt, v){
  k <- is.na(z)
  u <- findInterval(t_twt, z[!k])
  # plot(u, t_twt, type = "l")
  v_u <- c(v[-length(v)][!k], v[length(v)])
  v_u[u+1]
}
