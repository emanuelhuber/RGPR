#' Classical Automatic Gain Control (AGC) 
#' 
#' `gainAGC` applies an AGC (Automatic Gain Control) gain to
#' compensate for amplitude decay and enhances visibility of weaker signals.
#' The function uses a Gaussian-weighted local RMS or power measure, with
#' local mean removal. A gain floor is applied to avoid extreme
#' amplification in regions of near-zero signal.
#' 
#' The trace signal is smoothed with a Gaussian filter. The smoothed trace
#' is substracted from the original trace, raised to power `p`, smoothed
#' by a Gaussian filter (to obtain a local weighted sum) and raised to 
#' power `r` to obtain the gain.
#' Typical values are `p` = 2 and `r` = 0.5 which will
#'  make gain equal to the local RMS.  The abs() function is used to allow
#' for arbitrary 'p' and 'r'.
#'
#' Modified slots
#'   `data`: trace gained.
#'   `proc`: updated with function name and arguments.
#' 
#' @param obj    (`GPR* object`) An object of the class GPR.
#' @param sig    (`numeric[1]`) standard deviation of the kernel.
#' @param p    (`numeric[1]`) Parameter of the power filter
#'             (`p` \eqn{\geq} 0). Usually, `p` = 2.
#' @param r    (`numeric[1]`) Root applied to the smoothed power. 
#'              Usually, `r` = 0.5.
#' @param floorquantile (`numeric[1]`) Quantile of local gain used as minimum 
#'                       threshold to prevent extreme amplification 
#'                       (default = 0.05, value between 0 and 1.).
#' @param return_gain (`logical[1]`) Should the gain be returned (instead of the gained object)?
#' @param track (`logical[1]`) Should the processing step be tracked?             
#' @return `GPR class` An object of the class GPR.
#' 
#'     
#' 
#' @name gainAGC
#' @rdname gainAGC
#' @export
setGeneric("gainAGC", function(obj, sig = 10, p = 2, r = 0.5, 
                               floorquantile = 0.05, 
                               return_gain = FALSE,
                               track = TRUE)
  standardGeneric("gainAGC"))



#' @rdname gainAGC
#' @export
setMethod("gainAGC", "GPR", function(obj, sig = 10, p = 2, r = 0.5, floorquantile = 0.05, 
                                     return_gain = FALSE,
                                     track = TRUE){
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(sig,    msg, "NUMERIC1_SPOS", Inf)
  msg <- checkArg(p,    msg, "NUMERIC1_POS", Inf)
  msg <- checkArg(r,    msg, "NUMERIC1_POS", Inf)
  msg <- checkArg(floorquantile,    msg, "PERCENT1", Inf)
  msg <- checkArg(return_gain,      msg, "LOGICAL_LEN", 1)
  msg <- checkArg(track,            msg, "LOGICAL_LEN", 1)
  checkArgStop(msg)
  #-----------------------------------
  
  # xG <- .gainAgc(x@data, x@dz, w = w, p = p, r = r)
  
  G <- apply(obj@data, 2, .gainAGC, sig, p, r)
  
  if(isTRUE(track)) proc(obj) <- getArgs()
  if(isTRUE(return_gain)){
    return(G)
  }else{
    return(obj*G)
  }
  
  # h1 <- quantile(as.vector(abs(xG)), 0.99, na.rm = TRUE)
  # h2 <- quantile(as.vector(abs(x)), 0.99, na.rm = TRUE)
  # x@data <- xG / h1 * h2
  # 
  # if(isTRUE(track)) proc(x) <- getArgs()
  # return(x)
})

# .gainAgc <- function(A, dts, w = 10, p = 2, r = 0.5){
#   w <- w/dts
#   Anew <- apply(A, 2, .gainAgc0, w, p, r)
#   #s1 = ((max(A))-(min(A)));  # scale factor
#   #s2 = ((max(Anew))-(min(Anew)));  # scale factor
#   #return(Anew * s1/s2)
#   return(Anew)
# }

.gainAGC <- function(d, w = 10, p = 2 , r = 0.5, floorquantile = 0.05){
  # 1. convert NA into 0
  d[is.na(d)] <- 0
  # 2. subtract local mean
  dAmp   <- d - mmand::gaussianSmooth(d, w)
  # 3. Compute local power measure and root
  dGain <- (mmand::gaussianSmooth(abs(dAmp)^p, w))^r
  # 4. Define gain floor to avoid infinite or excessive gain
  gmin <- quantile(dGain[dGain > 0], floorquantile, na.rm = TRUE)
  test <- dGain >= gmin & dGain > 0
  # 5. compute gain
  d[test ] <- 1/dGain[test ]
  d[!test] <- 1
  return(d)
}