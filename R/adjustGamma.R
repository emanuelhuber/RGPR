

#-------------------------------- DEPRECATED ----------------------------------#
setGenericVerif("gammaCorrection", function(x, a = 1, b = 1, track = TRUE) 
  standardGeneric("gammaCorrection"))

#' Gamma correction of the amplitude
#'
#' This function computes the gamma correction.
#' 
#' @name gammaCorrection
#' @rdname rgCorrectGamma
#' @export
setMethod("gammaCorrection", "GPR", function(x, a = 1, b = 1,
                                             track = TRUE){
  warning("Function 'gammaCorrection()' is deprecated, use 'rgCorrectGamma()' instead.")
  x@data <- .gammaCorrection(x@data,a,b)
  if(isTRUE(track)) proc(x) <- getArgs()
  #   x@proc <- c(x@proc, proc)
  return(x)
} 
)


setGenericVerif("adjustGamma", function(x, a = 1, b = 1, track = TRUE) 
  standardGeneric("adjustGamma"))

#' Gamma correction of the amplitude
#' 
#' This function computes the gamma correction.
#'
#' @name adjustGamma
#' @rdname adjustGamma
#' @export
setMethod("adjustGamma", "GPR", function(x, a = 1, b = 1,
                                             track = TRUE){
  x@data <- .gammaCorrection(x@data,a,b)
  if(isTRUE(track)) proc(x) <- getArgs()
  #   x@proc <- c(x@proc, proc)
  return(x)
} 
)

.gammaCorrection <- function(A,a,b){
  return(a*sign(A)*abs(A)^b)
}
