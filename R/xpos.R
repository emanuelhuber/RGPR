#' x-position
#' 
#' Antenna separation distance(s)
#' 
#' Modified slots
#' \itemize{
#'   \item `x` the x-position 
#'   \item `xpos` the antenna separation distance only for CMP/WARR (plot twt as a function of antenna separation)
#' }
#' @param x (`GPR`) An object of the class GPR.
#' @param value (`numeric[m]`)
#' @return (`GPR`) 
#' @name xpos
#' @rdname xpos
#' @concept spatial computation
setGeneric("xpos", function(x) standardGeneric("xpos"))

#' @name xpos<-
#' @rdname xpos
setGeneric("xpos<-", function(x, value) standardGeneric("xpos<-"))

#' @rdname xpos
#' @export
setMethod("xpos", "GPR", function(x){
  return(x@x)
})


#' @rdname xpos
#' @export
setReplaceMethod("xpos", "GPR", function(x, value){
  if(length(value) != ncol(x)){
    stop("must have ", ncol(x), " values.")
  }
  if(any( abs(diff(value)) < sqrt(.Machine$double.eps))){
    stop("All values must be different!")
  }
  if(length(x@coord) > 0){
    warning("Changing 'xpos' does not change the trace coordinates.")
  }
  x@x <- value
  if(isCMP(x)){
    x@antsep <- value
  }
  x@proc <- c(x@proc, "xpos<-")
  return(x)
}
)
