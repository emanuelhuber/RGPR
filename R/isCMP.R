#' Return TRUE if survey mode is CMP
#' 
#' Return TRUE if survey mode is CMP/WARR
#' @param x [\code{GPR}] An object of the class GPR.
#' @return [\code{logical()}] \code{TRUE} if \code{x} is of type CMP/WARR
#' @name isCMP
#' @rdname isCMP
#' @export
setGeneric("isCMP", function(x) standardGeneric("isCMP"))

#' @rdname isCMP
#' @export
setMethod("isCMP", "GPR", function(x){
  (grepl("CMP", toupper(x@mode)) || 
     grepl("WARR", toupper(x@mode))) && 
    !grepl("CMPANALYSIS", toupper(x@mode))
})