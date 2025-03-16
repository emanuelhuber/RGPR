#' Plot the velocity layers on a 2D plot
#'
#' Plot the velocity layers on a 2D plot
#' @param obj (`GPR* object`) An object of the class `GPR`
#' @param method (`character[1]`) Interpolation method.
#' @param col (`character[1|n]`) Color.
#' @param ... Additional arguments for the function `plotDelineations()`
#' @name plotVelLayers
#' @rdname plotVelLayers
#' @export
setGeneric("plotVelLayers", 
           function(obj, 
                    method = c("linear", "nearest", "pchip", 
                               "cubic", "spline", "none"), 
                    col = NULL, ...) 
             standardGeneric("plotVelLayers"))


#' @rdname plotVelLayers
#' @export
setMethod("plotVelLayers", "GPR", 
          function(obj, 
                   method = c("linear", "nearest", "pchip", 
                              "cubic", "spline", "none"), 
                   col = NULL, ...){
            obj@delineations <- obj@md[["velocity_interfaces"]] 
            plotDelineations(obj, method = method, col = col, ...)
          })
