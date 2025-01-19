setGeneric("gainSEC", function(x, 
                                    a = 0.01, 
                                    b = 1, 
                                    t0   = NULL, 
                                    tend = NULL, 
                                    tcst = NULL,
                                    return_gain = FALSE,
                                    track = TRUE) 
  standardGeneric("gainSEC")) 


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
#' @param x    `GPR` An object of the class GPR.
#' @param a    `numeric[1]` Parameter of the exponential filter
#'             (`a` \eqn{\geq} 0).
#' @param b    `numeric(1)` Parameter of the power filter
#'             (`b` \eqn{\geq} 0). Usually, `b = 1`.
#' @param t0   `numeric` Start time of the gain filter
#'             (if `t0 = NULL`, `t0` is set equal to `time0(x)`).
#' @param tend `numeric[1]` End time of the gain filter (optional)
#' @param tcst `numeric[1]` Constant time: the gain before 
#'             `tcst` is set equal to the gain value at `tcst`.
#'             
#' @return `GPR class` An object of the class GPR.
#' 
#' @seealso [gainAGC()]
#'                            
#' @name gainSEC
#' @rdname gainSEC
#' @export
#' @concept processing
setMethod("gainSEC", "GPR", function(x, 
                                     a           = 0.01, 
                                     b           = 1, 
                                     t0          = NULL, 
                                     tend        = NULL, 
                                     tcst        = NULL,
                                     return_gain = FALSE,
                                     track       = TRUE){
  
  if(is.null(t0)) t0 <- x@z0
  if(length(t0) == 1) t0 <- rep(t0, ncol(x))
  
  #------------------- check arguments ----
  msg <- checkArgInit()
  msg <- checkArg(a,           msg, "NUMERIC1_POS", Inf)
  msg <- checkArg(b,           msg, "NUMERIC1_POS", Inf)
  msg <- checkArg(t0,          msg, "NUMERIC_LEN", c(1, ncol(x)))
  msg <- checkArg(tend,        msg, "NUMERIC1_NULL", Inf)
  msg <- checkArg(tcst,        msg, "NUMERIC1_NULL", Inf)
  msg <- checkArg(return_gain, msg, "LOGICAL_LEN", 1)
  checkArgStop(msg)
  #-----------------------------------#
  
  G <- .gainSEC(x, 
                a  = a, 
                b  = b,
                t0 = t0,
                tend = tend,
                tcst = tcst)
  
  if(isTRUE(track)) proc(x) <- getArgs()
  if(isTRUE(return_gain)){
    return(G)
  }else{
    return(x*G)
  }
  # x <- x*G
  return(x)
})



.gainSEC <- function(x, 
                     a     = 0.01, 
                     b     = 1, 
                     t0    = NULL, 
                     tend  = NULL, 
                     tcst  = NULL,
                     track = TRUE){
  spls <- sapply(t0, function(y, d){floor(sum(d < y))}, 
                 x@z)
  n <- nrow(x)
  g <- (0 + x@z^b) * exp(a * x@z)
  G <- sapply(spls, function(x, g, n){c(rep(1, x), g[1:(n-x)])}, g, n )
  
  if(!is.null(tend)){
    test_tend <- x@z >= tend
    if(any(test_tend)) G[test_tend, ] <- G[which(test_tend)[1],]
  }
  if(!is.null(tcst)){
    test_tcst <- x@z <= tcst
    if(any(test_tcst)) G[test_tcst, ] <- G[tail(which(test_tcst),1),]
  }
  # xG <- x*G
  # scaling
  h1 <- quantile(as.vector(abs(x*G)), 0.99, na.rm = TRUE)
  h2 <- quantile(as.vector(abs(x)), 0.99, na.rm = TRUE)
  x@data <- G / h1 * h2
  # xG <- xG / h1 * h2
  return(x)
}