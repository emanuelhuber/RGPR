
#  x = GPR object
#  x_vel = output of the function "getDepth()"
#  RETURN: a GPR object (like x) but with values equal to the depth at (x, t)

#' @name getDepth
#' @rdname getDepth
#' @export
setGeneric("getDepth", function(x) 
  standardGeneric("getDepth"))



#' Get depth model
#'
#' @param x GPR object   
#' @name getDepth
#' @rdname getDepth
#' @export
setMethod("getDepth", "GPR", function(x){
  # X <- c(0, diff(depth(x))) * x_vel/2
  # X[] <- apply(X@data, 2, cumsum)
  # return(X)
  # single velocity value
  if(length(x@vel[[1]]) == 1){
    x <- x[, 1]
    x@pos <- 1
    x@data[ ,1] <- timeToDepth(x@depth, time_0 = 0, v = x@vel[[1]], 
                     antsep = antsep(x))
    x <- x[!is.na(as.vector(x@data)),]
    
    # vector velocity
  }else if( is.null(dim(x@vel[[1]])) && length(x@vel[[1]]) == nrow(x) ){
    x <- x[, 1]
    x@pos <- 1
    x@data[ ,1] <- timeToDepth(x@depth, 0, v = x@vel[[1]], 
                           antsep = x@antsep) # here difference to matrix case
    x <- x[!is.na(as.vector(x@data)),]
    
  }else if(is.matrix(x@vel[[1]])){
    x@data <- apply(c(0, diff(depth(x))) * x@vel[[1]]/2, 2, cumsum)
  }
  return(x)
})

# twt_int = output of the function "interpInterface()"
# v = velocities (length(v) = nrow(twt_int + 1))  
#     (+1 because of the last layers)
# RETURN: matrix of the same dimensions as twt_int, 
# but indicating the depth    of the interfaces

#' @export
getDepthInterface <- function(twt_int, v){
  apply(twt_int, 2, .getDepthInt, v[-length(v)])
}

.getDepthInt <- function(z, v){
  k <- !is.na(z)
  z[k] <- cumsum(c(z[k][1], diff(z[k])) * v[k]/2)
  return(z)
}