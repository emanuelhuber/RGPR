# FIXME!
#' Is the z-dimension time?
#' 
#' Is the z-dimension time?
#' @param x  [\code{GPR class}] An object of the class \code{GPR}
#' @return   [\code{logical(1)}] \code{TRUE} if the z-dimension is in unit of
#'           time, \code{FALSE} if the z-dimension is in unit of length (depth)
#' @name isZTime
setGeneric("isZTime", function(x) 
  standardGeneric("isZTime"))



#' @rdname isZTime   
#' @export
setMethod("isZTime", "GPRvirtual", function(x){
  return(grepl("(s|min|h)$", x@zunit))
})

#' @rdname isZTime   
#' @export
setMethod("isZTime", "GPRsurvey", function(x){
  return(grepl("(s|min|h)$", x@zunits))
})

#------------------------------------------------------------------------------#

 
#' @aliases isZDepth,GPRsurvey-method
#' @rdname isZTime
setGeneric("isZDepth", function(x) 
  standardGeneric("isZDepth"))


#' @rdname isZTime   
#' @export
setMethod("isZDepth", "GPRsurvey", function(x){
    !isZTime(x)
})

#' @rdname isZTime   
#' @export
setMethod("isZDepth", "GPRvirtual", function(x){
  !isZTime(x)
})