# FIXME!
#' Is the z-dimension time?
#' 
#' Is the z-dimension time?
#' @param x  [\code{GPR class}] An object of the class \code{GPR}
#' @return   [\code{logical(1)}] \code{TRUE} if the z-dimension is in unit of
#'           time, \code{FALSE} if the z-dimension is in unit of length (depth)
#' @name isDepthTime
setGeneric("isDepthTime", function(x) 
  standardGeneric("isDepthTime"))



#' @rdname isDepthTime   
#' @export
setMethod("isDepthTime", "GPR", function(x){
  return(grepl("(s|min|h)$", x@depthunit))
})

#' @rdname isDepthTime   
#' @export
setMethod("isDepthTime", "GPRsurvey", function(x){
  return(grepl("(s|min|h)$", x@depthunit))
})

#------------------------------------------------------------------------------#


#' @aliases isDepthDepth,GPRsurvey-method
#' @rdname isDepthTime
setGeneric("isDepthDepth", function(x) 
  standardGeneric("isDepthDepth"))


#' @rdname isDepthTime   
#' @export
setMethod("isDepthDepth", "GPRsurvey", function(x){
  !isDepthTime(x)
})

#' @rdname isDepthTime   
#' @export
setMethod("isDepthDepth", "GPR", function(x){
  !isDepthTime(x)
})