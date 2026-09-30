
#' Auto-Correlation and Cross-Covariance Function Estimation
#'
#' @description Computes the auto-correlation or cross-covariance sequences 
#' for `GPR` objects. This is a wrapper around the base \code{\link[stats]{acf}} 
#' applied across the specified margin of the GPR data.
#'
#' @param x An object of class \code{GPR}.
#' @param lag.max Maximum lag at which to calculate the acf. Default is 100. 
#'   Must be less than or equal to the number of rows in \code{x}.
#' @param type Character string giving the type of acf to be computed. 
#'   Allowed values are \code{"correlation"} (default), \code{"covariance"}, 
#'   or \code{"partial"}.
#' @param MARGIN A vector giving the subscripts which the function will be applied over. 
#'   E.g., for a matrix, \code{1} indicates rows, \code{2} indicates columns. Default is 2.
#' @param na.action Function to be called to handle missing values. Default is \code{na.fail}.
#' @param demean Logical. Should the covariances be about the sample means? Default is \code{TRUE}.
#' @param ... Additional arguments passed to \code{\link[stats]{acf}}.
#'
#' @return An object of class \code{GPR} where the data contains the ACF values, 
#'   the depth axis represents the lags, and \code{dz} is set to 1.
#' 
#' @export
#' @name ACF
#' @rdname ACF
setGeneric("ACF", function(x,
                                lag.max   = 100,
                                type      = c("correlation", "covariance", "partial"),
                                MARGIN    = 2,
                                na.action = na.fail, 
                                demean    = TRUE, 
                                ...) 
standardGeneric("ACF"))



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
