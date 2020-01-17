#' Direct-Current shift removal 
#' 
#' The direct-current offset (DC-shift) is estimated and removed from every 
#' trace individually. 
#' For a given trace, the DC-shift is estimated by a user supplied function 
#' applied on few trace samples, normally the samples before time-zero
#' (e.g., the average of the samples before time-zero).  Then, the
#' DC-shift is substracted from the trace.
#' 
#' The direct-current offset (or DC-shift) is a constant bias over time
#' that slightly shifts the signal amplitude. The DC-shift is best 
#' observed on the trace samples recorded before the signal was emitted. 
#' 
#' Modified slots
#' \itemize{
#'   \item \code{data}: DC-shift removed (data dimensions unchanged).
#'   \item \code{proc}: updated with function name and arguments.
#' }
#' 
#' @param x [\code{GPR class}] An object of the class \code{GPR}.
#' @param u [\code{integer}] Index of the trace samples used to evaluate for
#'          every trace the DC-shift. If \code{u = NULL}, the function takes
#'          for each trace 90\% of the samples before time-zero (the number
#'          of samples can vary from trace to trace).  
#' @param FUN [\code{function}] A function to apply on the \code{u} trace 
#'            samples (default is \code{mean}; alternatively, \code{median} 
#'            could be of interest because it is more robust but slower to 
#'            compute).
#' @param ... [\code{ANY}] Further arguments to be passed to \code{FUN}. 
#'          
#' @return [\code{GPR class}] An object of the class \code{GPR}.
#' 
#' @examples 
#' data("frenkeLine00")
#' x <- frenkeLine00
#' x1 <- dcshift(x, u = 1:100, FUN = median)
#' plot(x - x1)
#' x2 <- dcshift(x)
#' plot(x - x2)
#' 
#' @name dcshift
#' @rdname dcshift
#' @export
setMethod("dcshift", "GPR", function(x, u = NULL, FUN = mean, ...,
                                     track = TRUE){
  
  if(!is.null(u)) u <- as.integer(u)
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(u,   msg, "INDEX_VECTOR_NULL_UPPER", nrow(x))
  msg <- checkArg(FUN, msg, "FUNCTION")
  checkArgStop(msg)
  #    - ----------------------------------
  
  if(is.null(u)){
    if(all(time0(x) > x@depth[1])){
      # Dt <- min(time0(x)) - x@depth[1]  # time before time-zero
      # u <- x@depth[1] + 0:round((Dt*0.9)/x@dz)
      
      # 90% samples before time0 (computed individually for each trace)
      # computation independent of the time axis
      spls <- sapply(time0(x), function(y, d, a= 0.9){floor(a * sum(d <= y))}, 
                     x@depth)
      xDepth <- matrix(seq_along(x@depth), byrow = TRUE, 
                       nrow = ncol(x), ncol = nrow(x))
      # test which samples can be used for the computation
      test <- t(xDepth <= spls)
      f <- function(i, x, y, FUN, ...){
        FUN(x[,i][y[,i]], ...)
      }
      OUT <- sapply(1:ncol(x), f, x@data, test, FUN, ...)
    }else{
      warning("You must define 'u' or reset time-zero.\n",
              "not enough samples before time-zero...")
      return(x)
    }
  }else{
    OUT <- apply(x[u, ], 2, FUN, ...)
  }
  x_shift <- matrix(OUT, nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
  x <-  x - x_shift
  
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
})