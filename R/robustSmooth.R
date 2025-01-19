#' Robust smoothing
#' 
#' A wrapper for the functions `robfilter::hybrid.filter` and
#' `smooth.spline`
#' @param x       a numeric vector or (univariate) time series object.
#' @param spar    smoothing parameter, typically (but not necessarily) in (0,1].
#'                See `[stats:smooth.spline()]`.
#' @param width	  an odd positive integer (>=3) defining the window width 
#'                used for fitting.
#'                See `[robfilter:hybrid.filter()]`.
#' @param method	a (vector of) character string(s) containing the method(s) 
#'                to be used for the estimation of the signal level. 
#'                Method choice: "MED", "RM", 
#'                "MEAN", FMH, "PFMH", "CFMH", "MH", "PRMH", "CRMH", "MMH", 
#'                "PRMMH", "CRMMH".
#'                See `[robfilter:hybrid.filter()]`.
#' @param extrapolate	a logical indicating whether the level estimations 
#'                    should be extrapolated to the edges of the time series.  
#'                    See `[robfilter:hybrid.filter()]`. 
#' @param  minNonNAs a positive integer defining the minimum number of 
#'                   non-missing observationswithin each window (half) 
#'                   which is required for a 'sensible' estimation.
#'                   See `[robfilter:hybrid.filter()]`.
#' @export            
robustSmooth <- function(x, spar = NULL, width,  method = "PRMMH", 
                         extrapolate = TRUE,
                         minNonNAs = 3){
  if (missing(width)) {
    stop("argument 'width' is missing with no default")
  }
  xf <- robfilter::hybrid.filter(x, width = width, method = method, 
                                 extrapolate = extrapolate,
                                 minNonNAs = minNonNAs)
  if(sum(is.na(xf$level$PRMMH)) > 1){
    x_NA <- is.na(xf$level$PRMMH)
    xfok <- signal::interp1(which(!x_NA), xf$level$PRMMH[!x_NA], seq_along(x_NA), method = "pchip", 
                            extrap = TRUE)
  }else{
    xfok <- xf$level$PRMMH
  }
  xfs <- smooth.spline(x = xfok,  spar = spar)$y
  return(xfs)
}
