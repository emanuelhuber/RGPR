#' Gain Compensation Based on Envelope
#'
#' Applies a gain to a GPR object based on the envelope of the signal.
#' The gain is computed from a summary statistic (e.g., mean) of the
#' envelope across traces and normalized to compensate for amplitude decay.
#'
#' @param obj (`GPR`)] A GPR object containing traces to be gain-compensated.
#' @param FUN (`function`) A summary function applied to the envelope of each time sample across traces.
#'            Defaults to \code{mean}. Other options could be \code{median}, \code{max}, etc.
#' @param floorquantile (`numeric[1]`) Quantile of local gain used as minimum 
#'                       threshold to prevent extreme amplification 
#'                       (default = 0.05, value between 0 and 1.).
#' @param return_gain (`logical[1]`) Should the gain be returned (instead of the gained object)?
#' @param track (`logical[1]`) Should the processing step be tracked?  
#'
#' @return A GPR object with amplitude gain applied according to the envelope-based compensation.
#'
#' @details
#' The function computes the envelope of each trace (using \code{amplEnv}) and summarizes it across
#' traces using the specified \code{FUN}. The resulting vector is normalized to its maximum value
#' to produce a gain vector. The gain is set to 1 for all samples before the peak of the normalized
#' envelope to avoid boosting early-time noise. The reciprocal of the gain is applied to the original
#' GPR data.
#'
#' Mathematically:
#' \deqn{
#' g_0(t) = \frac{\text{FUN}(\text{Envelope}(obj))}{\max(\text{FUN}(\text{Envelope}(obj)))}, \quad
#' g(t) = 1/g_0(t)
#' }
#' where \(g_0(t) = 1\) for times before the peak of the envelope.
#'
#' @name gainEnv
#' @rdname gainEnv
#' @export
setGeneric("gainEnv", function(obj, FUN = mean, floorquantile = 0.05, return_gain = FALSE, track = TRUE) 
  standardGeneric("gainEnv"))




#' @rdname gainEnv
#' @export
setMethod("gainEnv", "GPR", function(obj, FUN = mean, floorquantile = 0.05, return_gain = FALSE,  track = TRUE){
  obj_mean <- apply(amplEnv(obj), 1, FUN = FUN)
  # plot(obj[, 10])
  # lines(obj_env[, 10], col="red")
  # lines(obj4_env_mean, col="green")
  # plot(maobj(obj4_env_mean) - obj4_env_mean)
  g0 <- as.vector(obj_mean/max(obj_mean))
  gmin <- quantile(g0[g0 > 0], floorquantile, na.rm = TRUE)
  
  test <- g0 >= gmin & g0 > 0
  # 5. compute gain
  G <- 1/g0[test ]
  G[!test] <- 1
  
  # sel <- seq_along(g0) > which.max(g0)
  # g0[!sel] <- 1
  # g <- 1/g0
  # g[!is.finite(g)] <- 1
  if(isTRUE(track)) proc(obj) <- getArgs()
  if(isTRUE(return_gain)){
    return(G)
  }else{
    return(obj*G)
  }
} 
)