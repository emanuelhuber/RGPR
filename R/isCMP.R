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
     grepl("WARR", toupper(x@mode)))
})


#' @rdname isCMP
#' @export
setGeneric("isCommonOffset", function(x) standardGeneric("isCommonOffset"))

#' @rdname isCMP
#' @export
setMethod("isCommonOffset", "GPR", function(x){
  (grepl("CO", toupper(x@mode)) )
})


#' @rdname isCMP
#' @export
setGeneric("isVelSpectrum", function(x) standardGeneric("isVelSpectrum"))

#' @rdname isCMP
#' @export
setMethod("isVelSpectrum", "GPR", function(x){
  toupper("isVelSpectrum") == toupper(x@mode)
})


#' @rdname isCMP
#' @export
setGeneric("isVelModel", function(x) standardGeneric("isVelModel"))

#' @rdname isCMP
#' @export
setMethod("isVelModel", "GPR", function(x){
  toupper("velModel") == toupper(x@mode)
})

