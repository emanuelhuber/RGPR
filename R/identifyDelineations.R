#' Identify the delineation on a 2D plot
#'
#' Works only close to the points !!!
#' @param obj (`GPR* object`) An object of the class `GPR`
#' @param method (`character[1]`) Interpolation method.
#' @param ... Additional parameters (not yet used)
#' @name identifyDelineations
#' @rdname delineation
#' @export
setGeneric("identifyDelineations", function(obj, 
                                                method = c("linear", "nearest", "pchip", 
                                                           "cubic", "spline", "none"), 
                                                ...) 
  standardGeneric("identifyDelineations"))

#' @rdname delineation
#' @export
setMethod("identifyDelineations", "GPR", function(obj, 
                                                 method = c("linear", "nearest", "pchip", 
                                                            "cubic", "spline", "none"), 
                                                 ...){
  if(is.null(dev.list())){
    stop("You must first plot the GPR profile with the function \"plot\"!\n")
  }
  if(length(obj@delineations) > 0){
    method <- match.arg(method, c("linear", "nearest", "pchip", 
                                  "cubic", "spline", "none"))
    if(method == "none"){
      xyzrel <- .getXYZrel(obj)
    }else{
      xyzrel <- .getXYZrelIntp(obj, method)
    }
    xyzrel_id <- seq_along(xyzrel)
    xzrel <- Map(function(obj, y) cbind(obj[,4:5], y), xyzrel, xyzrel_id)
    xzrel <- do.call(rbind, xzrel) 
    colnames(xzrel) <- c("xrel", "zrel", "id")
    A <- identify(xzrel, labels = xzrel[, 3], n = 100)
    return(xzrel[A, 3])
  }else{
    message("There is no delineations. Use 'delineate()' ",
            "to delineate reflectors/structures on your data.")
  }
})