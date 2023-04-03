#' Antenna separation distance(s)
#' 
#' Antenna separation distance(s)
#' 
#' Modified slots
#' \itemize{
#'   \item \code{antsep} the antenna separation distance
#'   \item \code{x} the x-position only for CMP/WARR (plot twt as a function of antenna separation)
#' }
#' @param x [\code{GPR}] An object of the class GPR.
#' @param value [\code{numeric(1)|numeric(m)}]
#' @return [\code{GPR}] 
#' @name antsep
#' @rdname antsep
#' @concept getters/setters
setGeneric("antsep", function(x) standardGeneric("antsep"))

#' @name antsep<-
#' @rdname antsep
setGeneric("antsep<-", function(x, value) standardGeneric("antsep<-"))

#' @rdname antsep
#' @export
setMethod("antsep", "GPR", function(x){
  return(x@antsep)
})


#' @rdname antsep
#' @export
setReplaceMethod("antsep", "GPR", function(x, value){
  if(isCMP(x)){
    if(length(value) != ncol(x)){
      stop("must have ", ncol(x), " values.")
    }
    x@antsep <- value
    x@x <- value
  }else{
    if(length(value) != 1){
      stop("must be a length-one numeric value")
    }
    x@antsep <- value
  }
  x@proc <- c(x@proc, "antsep<-")
  return(x)
}
)