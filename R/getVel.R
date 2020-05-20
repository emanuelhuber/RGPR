
#' @name getVel
#' @rdname getVel
#' @export
setGeneric("getVel", function(x) 
  standardGeneric("getVel"))

#' Get velocity model
#'
#' Return the velocity as a function of two-way travel time.
#'
#' @param x GPR object
#' @name getVel
#' @rdname getVel
#' @export
setMethod("getVel", "GPR", function(x){
  if(is.null(dim(x@vel[[1]]))){
    x <- x[, 1]
    x@pos <- 0
    x@data[] <- x@vel[[1]]
  }else{
    x@data <- x@vel[[1]]  
  }
  return(x)
})

