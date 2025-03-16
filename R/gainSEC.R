
# @seealso [gainAGC()]

#' Spreading and Exponential Compensation (SEC) gain
#' 
#' `gainSEC` Applies a combination of a power and exponential time gain to compensate for 
#' the signal attenuation through spherical spreading losses and  
#' exponential ohmic dissipation of energy with depth. Usually, the power in
#' the power gain is set to zero.
#' 
#' Spreading and Exponential Compensation (SEC) gain can be written as 
#' \eqn{\exp(a \cdot t) \cdot t^b}, where \eqn{t^b} is the power gain
#' (set \eqn{b = 1} to get a linear gain) and \eqn{\exp(a \cdot t)} is the
#' exponential gain.
#' 
#' Modified slots
#'   `data`: trace gained.
#'   `proc`: updated with function name and arguments.
#' 
#' @param obj    (`GPR* object`) An object of the class GPR.
#' @param a    (`numeric[1]`) Parameter of the exponential filter
#'             (`a` \eqn{\geq} 0).
#' @param b    `numeric(1)` Parameter of the power filter
#'             (`b` \eqn{\geq} 0). Usually, `b = 1`.
#' @param t0   (`numeric[1]`) Start time of the gain filter
#'             (if `t0 = NULL`, `t0` is set equal to `time0(x)`).
#' @param tend (`numeric[1]`) End time of the gain filter (optional)
#' @param tcst (`numeric[1]`) Constant time: the gain before 
#'             `tcst` is set equal to the gain value at `tcst`.
#' @param return_gain (`logical[1]`) Should the gain be returned (instead of the gained object)?
#' @param track (`logical[1]`) Should the processing step be tracked?             
#' @return `GPR class` An object of the class GPR.
#' 
#'                            
#' @name gainSEC
#' @rdname gainSEC
#' @concept processing
#' @export
setGeneric("gainSEC", function(obj, 
                                    a = 0.01, 
                                    b = 1, 
                                    t0   = NULL, 
                                    tend = NULL, 
                                    tcst = NULL,
                                    return_gain = FALSE,
                                    track = TRUE) 
  standardGeneric("gainSEC")) 


#' @rdname gainSEC
#' @export
setMethod("gainSEC", "GPR", function(obj, 
                                     a           = 0.01, 
                                     b           = 1, 
                                     t0          = NULL, 
                                     tend        = NULL, 
                                     tcst        = NULL,
                                     return_gain = FALSE,
                                     track       = TRUE){
  
  if(is.null(t0)) t0 <- obj@z0
  if(length(t0) == 1) t0 <- rep(t0, ncol(obj))
  
  #------------------- check arguments ----
  msg <- checkArgInit()
  msg <- checkArg(a,           msg, "NUMERIC1_POS", Inf)
  msg <- checkArg(b,           msg, "NUMERIC1_POS", Inf)
  msg <- checkArg(t0,          msg, "NUMERIC_LEN", c(1, ncol(obj)))
  msg <- checkArg(tend,        msg, "NUMERIC1_NULL", Inf)
  msg <- checkArg(tcst,        msg, "NUMERIC1_NULL", Inf)
  msg <- checkArg(return_gain, msg, "LOGICAL_LEN", 1)
  checkArgStop(msg)
  #-----------------------------------#
  
  G <- .gainSEC(obj, 
                a  = a, 
                b  = b,
                t0 = t0,
                tend = tend,
                tcst = tcst)
  
  if(isTRUE(track)) proc(obj) <- getArgs()
  if(isTRUE(return_gain)){
    return(G)
  }else{
    return(obj*G)
  }
  # obj <- obj*G
  return(obj)
})



.gainSEC <- function(obj, 
                     a     = 0.01, 
                     b     = 1, 
                     t0    = NULL, 
                     tend  = NULL, 
                     tcst  = NULL,
                     track = TRUE){
  spls <- sapply(t0, function(y, d){floor(sum(d < y))}, 
                 obj@z)
  n <- nrow(obj)
  g <- (0 + obj@z^b) * exp(a * obj@z)
  G <- sapply(spls, function(x, g, n){c(rep(1, x), g[1:(n-x)])}, g, n )
  
  if(!is.null(tend)){
    test_tend <- obj@z >= tend
    if(any(test_tend)) G[test_tend, ] <- G[which(test_tend)[1],]
  }
  if(!is.null(tcst)){
    test_tcst <- obj@z <= tcst
    if(any(test_tcst)) G[test_tcst, ] <- G[tail(which(test_tcst),1),]
  }
  # xG <- x*G
  # scaling
  h1 <- quantile(as.vector(abs(obj*G)), 0.99, na.rm = TRUE)
  h2 <- quantile(as.vector(abs(obj)), 0.99, na.rm = TRUE)
  obj@data <- G / h1 * h2
  # xG <- xG / h1 * h2
  return(obj)
}