
setGeneric("unwrapPhase", function(x) 
  standardGeneric("unwrapPhase"))


#' Unwrap signal phase
#' 
#' Based on the function `signal::unwrap()`
#' 
#' @export
setMethod("unwrapPhase", "GPR", function(x){
  x@data <-apply(x, 2, signal::unwrap)
  # x@data <-signal::unwrap(x@data)
  proc(x) <- getArgs()
  return(x)
})