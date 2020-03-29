#' Annotations of the GPR data
#' 
#' Annotations are currently used to store the intersections of GPR lines.
#' @param x [\code{GPR}] An object of the class GPR.
#' @param value [\code{character}]
#' @return [\code{GPR}] 
#' @name ann
#' @rdname ann
setGeneric("ann", function(x) standardGeneric("ann"))

#' @name ann<-
#' @rdname ann
setGeneric("ann<-", function(x, value) standardGeneric("ann<-"))

#' @rdname ann
#' @export
setMethod("ann", "GPR", function(x){
  return(x@ann)
} 
)

# FIXME: x@intersections -> project to GPR data and update ann!

#' @rdname ann
#' @export
setReplaceMethod("ann", "GPR", function(x, value){
  vals <- value
  if(!is.matrix(value)){
    vals <- matrix(value, nrow = 1, ncol = length(value))
  }
  traces <- (value[,1])
  annnames <- as.character(value[,2])
  valuesList <- (tapply(annnames, traces, identity))
  test <- unlist(lapply(valuesList, paste, sep = "", collapse = "#"), 
                 use.names = TRUE)
  if(length(x@ann) != length(x)) x@ann <- character(length(x))
  test <- sapply(test, trimStr)
  x@ann[as.numeric(names(test))] <- test
  x@proc <- c(x@proc, "ann<-")
  return(x)
}
)