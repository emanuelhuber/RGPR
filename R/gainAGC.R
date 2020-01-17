
setGenericVerif("gainAGC", function(x, w = 10, p = 2, r = 0.5, track = TRUE) 
  standardGeneric("gainAGC")) 

#' Automatic Gain Control (AGC) gain
#' 
#' \code{gainAGC} applies an AGC (Automatic Gain Control) gain.
#' The trace signal is smoothed with a Gaussian filter. The smoothed trace
#' is substracted from the original trace, raised to power \code{p}, smoothed
#' by a Gaussian filter and raised to power \code{r} to obtain the gain.
# Subtract image from local mean, raise to power 'p' then apply Gaussian
# smoothing filter to obtain a local weighted sum. 
# Finally raise the result
# to power 'r' to obtain the 'gain'.  Typically p = 2 and r = 0.5 which will
# make gain equal to the local RMS.  The abs() function is used to allow
# for arbitrary 'p' and 'r'.
# Apply inverse gain to the difference between the image and the local
# mean to obtain the final AGC image.
#' 
#' Spreading and Exponential Compensation (SEC) gain can be written as 
#' \eqn{\exp(a \cdot t) \cdot t^b}, where \eqn{t^b} is the power gain
#' (set \eqn{b = 1} to get a linear gain) and \eqn{\exp(a \cdot t)} is the
#' exponential gain.
#' 
#' Modified slots
#' \itemize{
#'   \item \code{data}: trace gained.
#'   \item \code{proc}: updated with function name and arguments.
#' }
#' 
#' @param x    [\code{GPR class}] An object of the class GPR.
#' @param w    [\code{numeric(1)}] Standard deviation of the 
#'             Gaussian smoother (in trace unit).
#' @param p    [\code{numeric(1)}] Parameter of the power filter
#'             (\code{b} \eqn{\geq} 0). Usually, \code{b = 1}.
#' @param r   [\code{numeric}] Start time of the gain filter
#'             (if \code{t0 = NULL}, \code{t0} is set equal to \code{time0(x)}).
#'             
#' @return [\code{GPR class}] An object of the class GPR.
#' 
#' @seealso \code{\link{gainSEC}}
#' 
#' @name gainAGC
#' @rdname gainAGC
#' @export
setMethod("gainAGC", "GPR", function(x, w = 10, p = 2, r = 0.5, track = TRUE){
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(w,    msg, "NUMERIC1_SPOS", Inf)
  msg <- checkArg(p,    msg, "NUMERIC1_POS", Inf)
  msg <- checkArg(r,    msg, "NUMERIC1_POS", Inf)
  checkArgStop(msg)
  #-----------------------------------
  
  xG <- .gainAgc(x@data, x@dz, w = w, p = p, r = r)
  
  h1 <- quantile(as.vector(abs(xG)), 0.99, na.rm = TRUE)
  h2 <- quantile(as.vector(abs(x)), 0.99, na.rm = TRUE)
  x@data <- xG / h1 * h2
  
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
})

.gainAgc <- function(A, dts, w = 10, p = 2, r = 0.5){
  w <- w/dts
  Anew <- apply(A, 2, .gainAgc0, w, p, r)
  #s1 = ((max(A))-(min(A)));  # scale factor
  #s2 = ((max(Anew))-(min(Anew)));  # scale factor
  #return(Anew * s1/s2)
  return(Anew)
}

.gainAgc0 <- function(d, w = 10, p = 2 , r = 0.5){
  # convert NA into 0
  d[is.na(d)] <- 0
  dAmp   <- d - mmand::gaussianSmooth(d, w)
  dGain <- (mmand::gaussianSmooth(abs(dAmp)^p, w))^r
  test <- dGain > 0
  d[test ] <- dAmp[test]/dGain[test ]
  return(d)
}

