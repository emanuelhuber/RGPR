setGenericVerif("gainSEC", function(x, a = 0.01, b = 1, 
                                    t0   = NULL, 
                                    tend = NULL, 
                                    tcst = NULL,
                                    track = TRUE) 
  standardGeneric("gainSEC")) 

setGenericVerif("getGainSEC", function(x, a = 0.01, b = 1, 
                                       t0   = NULL, 
                                       tend = NULL, 
                                       tcst = NULL,
                                       track = TRUE) 
  standardGeneric("getGainSEC"))




#' Spreading and Exponential Compensation (SEC) gain
#' 
#' \code{gainSEC} Applies a combination of a power and exponential time gain to compensate for 
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
#' \itemize{
#'   \item \code{data}: trace gained.
#'   \item \code{proc}: updated with function name and arguments.
#' }
#' 
#' @param x    [\code{GPR class}] An object of the class GPR.
#' @param a    [\code{numeric(1)}] Parameter of the exponential filter
#'             (\code{a} \eqn{\geq} 0).
#' @param b    [\code{numeric(1)}] Parameter of the power filter
#'             (\code{b} \eqn{\geq} 0). Usually, \code{b = 1}.
#' @param t0   [\code{numeric}] Start time of the gain filter
#'             (if \code{t0 = NULL}, \code{t0} is set equal to \code{time0(x)}).
#' @param tend [\code{numeric(1)}] End time of the gain filter (optional)
#' @param tcst [\code{numeric(1)}] Constant time: the gain before 
#'             \code{tcst} is set equal to the gain value at \code{tcst}.
#'             
#' @return [\code{GPR class}] An object of the class GPR.
#' 
#' @seealso \code{\link{gainAGC}}
#'                            
#' @name gainSEC
#' @rdname gainSEC
#' @export
setMethod("gainSEC", "GPR", function(x, a = 0.01, b = 1, 
                                     t0   = NULL, 
                                     tend = NULL, 
                                     tcst = NULL,
                                     track = TRUE){
  
  if(is.null(t0)) t0 <- x@time0
  if(length(t0) == 1) t0 <- rep(t0, ncol(x))
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(a,    msg, "NUMERIC1_POS", Inf)
  msg <- checkArg(b,    msg, "NUMERIC1_POS", Inf)
  msg <- checkArg(t0,   msg, "NUMERIC_LEN", c(1, ncol(x)))
  msg <- checkArg(tend, msg, "NUMERIC1_NULL", Inf)
  msg <- checkArg(tcst, msg, "NUMERIC1_NULL", Inf)
  
  checkArgStop(msg)
  #-----------------------------------
  
  G <- getGainSEC(x, 
                  a  = a, 
                  b  = b,
                  t0 = t0,
                  tend = tend,
                  tcst = tcst)
  x <- x*G
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
})

#' Spreading and Exponential Compensation (SEC) gain
#' 
#' \code{getGainSEC} returns the SEC gain as an object of the class \code{GPR}.
#'                   
#' @name getGainSEC
#' @rdname gainSEC
#' @export
setMethod("getGainSEC", "GPR", function(x, a = 0.01, b = 1, 
                                        t0   = NULL, 
                                        tend = NULL, 
                                        tcst = NULL,
                                        track = TRUE){
  
  if(is.null(t0)) t0 <- x@time0
  if(length(t0) == 1) t0 <- rep(t0, ncol(x))
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(a,    msg, "NUMERIC1_POS", Inf)
  msg <- checkArg(b,    msg, "NUMERIC1_POS", Inf)
  msg <- checkArg(t0,   msg, "NUMERIC_LEN", c(1, ncol(x)))
  msg <- checkArg(tend, msg, "NUMERIC1_NULL", Inf)
  msg <- checkArg(tcst, msg, "NUMERIC1_NULL", Inf)
  checkArgStop(msg)
  #-----------------------------------
  
  spls <- sapply(t0, function(y, d){floor(sum(d < y))}, 
                 x@depth)
  n <- nrow(x)
  g <- (0 + x@depth^b) * exp(a * x@depth)
  G <- sapply(spls, function(x, g, n){c(rep(1, x), g[1:(n-x)])}, g, n )
  
  if(!is.null(tend)){
    test_tend <- x@depth >= tend
    if(any(test_tend)) G[test_tend, ] <- G[which(test_tend)[1],]
  }
  if(!is.null(tcst)){
    test_tcst <- x@depth <= tcst
    if(any(test_tcst)) G[test_tcst, ] <- G[tail(which(test_tcst),1),]
  }
  # xG <- x*G
  # scaling
  h1 <- quantile(as.vector(abs(x*G)), 0.99, na.rm = TRUE)
  h2 <- quantile(as.vector(abs(x)), 0.99, na.rm = TRUE)
  x@data <- G / h1 * h2
  # xG <- xG / h1 * h2
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
})
