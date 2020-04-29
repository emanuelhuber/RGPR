#' Arc-tangent
#' 
#' The arc-tangent of two arguments atan2(y, x) returns the angle between 
#' the x-axis and the vector from the origin to (x, y), i.e., 
#' for positive arguments atan2(y, x) == atan(y/x).
#' @param y [\code{GPR}|numeric|matrix|complex]
#' @param x [\code{GPR}|numeric|matrix|complex]
#' @return [\code{GPR}] 
#' @rdname atan2
#' @name atan2
setGeneric("atan2", function(y, x) standardGeneric("atan2"))




#' @rdname atan2
setMethod(
  f = "atan2",
  signature = c(y = "GPRvirtual", x = "ANY"), 
  definition = function(y, x){
    y@data <- atan2(y@data, x)
    return(y)
  }
)


#' @rdname atan2
setMethod(
  f = "atan2",
  signature = c(y = "GPRvirtual", x = "GPRvirtual"), 
  definition = function(y, x){
    y@data <- atan2(y@data, x@data)
    return(y)
  }
)

#' @rdname atan2
setMethod(
  f = "atan2",
  signature = c(y = "ANY", x = "GPRvirtual"), 
  definition = function(y, x){
    x@data <- atan2(y, x@data)
    return(x)
  }
)



