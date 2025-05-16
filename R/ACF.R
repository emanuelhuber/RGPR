
setGenericVerif("ACF", function(x,
                                lag.max   = 100,
                                type      = c("correlation", "covariance", "partial"),
                                MARGIN    = 2,
                                na.action = na.fail, 
                                demean    = TRUE, 
                                ...) 
standardGeneric("ACF"))


#' Auto-correlation function 
#' 
#' This function computes the auto-correlation function.
#'
#' @name ACF
#' @rdname ACF
#' @export
setMethod("ACF", "GPR", function(x, 
                lag.max   = 100,
                type      = c("correlation", "covariance", "partial"),
                MARGIN    = 2,
                na.action = na.fail, 
                demean    = TRUE, 
                ...){
  if(lag.max > nrow(x)) stop("'lag.max' must be less than or equal to 'nrow(x)'.")
  Xout <- x[1:(lag.max + 1), ]
  Xout@data <- apply(x@data, 2, .acf, 
                     lag.max = lag.max, 
                     type = type,
                     plot = FALSE,
                     na.action = na.action,
                     demean = demean,
                     ...)
  Xout@dz <- 1
  Xout@depth <- seq_len(lag.max + 1)
  proc(Xout) <- getArgs()
  return(Xout)
})

# private function
.acf <- function(x, lag.max = NULL,
                 type = c("correlation", "covariance", "partial"),
                 plot = TRUE, na.action = na.fail, demean = TRUE, ...){
  acf(x = x, lag.max = lag.max,
      type = type,
      plot = plot, na.action = na.action, demean = demean, ...)$acf
}
