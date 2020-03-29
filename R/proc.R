#' Get processing steps applied to the data
#' 
#' \code{processing} returns all the processing steps applied to the data.
#' @param x [\code{GPR}] An object of the class GPR.
#' @param value [\code{character}]
#' @return A character vector whose elements contain the name of the 
#' processing functions with their arguments applied previously on the
#' GPR data.
#' @name proc
#' @rdname proc
setGeneric("proc", function(x) standardGeneric("proc"))


#' @name proc<-
#' @rdname proc
setGeneric("proc<-", function(x, value) standardGeneric("proc<-"))


#' @rdname proc
#' @export
setMethod("proc", "GPR", function(x){
  return(x@proc)
})


#' @rdname proc
#' @export
setReplaceMethod("proc", "GPR", function(x, value){
  value <- as.character(value)
  x@proc <- c(x@proc, value)
  return(x)
})