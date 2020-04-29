#' Normal Move-Out streching
#' 
#' Compute the Normal Move-Out (NMO) streching for a data set given a constant velocity.
#' The NMO streching is defined by \deqn{S_{NMO} = \frac{\Delta_{NMO}}{t_0}}, where
#' \deqn{t_0 = t_{TWT}(x = 0) = \frac{2z}{v}} is the vertical two-way traveltime at zero offset. 
#' @param x An object of the class \code{GPR}
#' @param v A length-one numeric vector defining the radar wave velocity in 
#'          the ground
#' @name NMOstreching
setGeneric("NMOstreching", function(x, v = NULL) 
  standardGeneric("NMOstreching"))

#' @rdname NMOstreching
#' @export
setMethod("NMOstreching", "GPR", function(x, v = NULL){
  # if(any(x@z0 > 0)){
  #   stop("You must first shift the traces to time-zero with\n",
  #        "'shiftToTime0()'")
  # }
  # if(is.null(v)){
  #   stop("You must assign a positiv numerical value to 'v'!")
  # }
  S_NMO <- NMO(x, v) / x@z
  S_NMO@data[is.infinite(S_NMO@data)] <- 0
  S_NMO@dlab <- "NMO strech"
  S_NMO@dunit <- ""
  
  proc(S_NMO) <- getArgs()
  return(S_NMO)
})

