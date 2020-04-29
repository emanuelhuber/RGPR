#' Oriented bounding box
#' 
#' source "whuber" from stackexchange.com
# https://gis.stackexchange.com/questions/22895/finding-minimum-area-rectangle-for-given-points/181883#181883
#' @param x      [\code{GPR class}] An object of the class \code{GPR}
#' @return [\code{matrix(5,2)}] The coordinates of the corners of the oriented 
#'          bounding box, whereby the last row is identical to the first row.
#'          FIXME!!
#' @name isZunitTime
setGeneric("isZunitTime", function(x) 
  standardGeneric("isZunitTime"))



#' @rdname isZunitTime   
#' @export
setMethod("isZunitTime", "GPRvirtual", function(x){
  return(grepl("(s|min|h)$", x@zunit))
})

#' @rdname isZunitTime   
#' @export
setMethod("isZunitTime", "GPRsurvey", function(x){
  return(grepl("(s|min|h)$", x@zunits))
})

#------------------------------------------------------------------------------#

 
#' @aliases isZunitLength,GPRsurvey-method
#' @rdname isZunitTime
setGeneric("isZunitLength", function(x) 
  standardGeneric("isZunitLength"))


#' @rdname isZunitTime   
#' @export
setMethod("isZunitLength", "GPRsurvey", function(x){
    !isZunitTime(x)
})

#' @rdname isZunitTime   
#' @export
setMethod("isZunitLength", "GPRvirtual", function(x){
  !isZunitTime(x)
})