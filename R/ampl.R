setGenericVerif("ampl", function(x, npad = 100, FUN = mean, ...) 
  standardGeneric("ampl"))

#' Amplitude (deprecated)
#'
#' Do not use this function!
#'
#' @name ampl
#' @rdname ampl
#' @export
setMethod("ampl", "GPR", function(x, npad = 100, FUN = NULL, ...){
  warning("Deprecated! Use 'envelope()' instead.")
  if(is.null(FUN)){
    xmax <- max(abs(x), na.rm = TRUE)
    xH <- apply(x, 2, HilbertTransf, npad = npad)
    x <- sqrt(x^2 + base::Re(xH)^2)
    x@data[abs(x@data) > xmax] <- xmax
  }else{
    #funName <- getFunName(FUN)
    x@data[] <- apply(x, 1, FUN, ...)
  }
  # proc(x) <- getArgs( addArgs = c('FUN' = funName))
  proc(x) <- getArgs()
  return(x)
}
)
