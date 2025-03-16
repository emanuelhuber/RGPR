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
#' * `data`: DC-shift removed (data dimensions unchanged).
#' * `proc`: updated with function name and arguments.
#' 
#' @param x (`GPR`) An object of the class `GPR`.
#' @param u (`integer[1]`) Index of the trace samples used to evaluate for
#'          every trace the DC-shift. If `u = NULL`, the function takes
#'          for each trace 90\% of the samples before time-zero (the number
#'          of samples can vary from trace to trace).  
#' @param FUN (`function()`) A function to apply on the `u` trace 
#'            samples (default is `mean`; alternatively, `median` 
#'            could be of interest because it is more robust but slower to 
#'            compute).
#' @param ... (`ANY`) Further arguments to be passed to `FUN`. 
#' @param track (`logical[1]`) Should the processing step be tracked?          
#' @return (`GPR`) An object of the class `GPR`.
#' 
#' @name rmDCShift
#' @rdname rmDCShift
#' @export
setGeneric("rmDCShift", 
           function(x, u = NULL, FUN = mean, ...,
                    track = TRUE)
             standardGeneric("rmDCShift"))



#' @rdname rmDCShift
#' @export
setMethod("rmDCShift", "GPR", function(x, u = NULL, FUN = mean, ...,
                                     track = TRUE){
  
  if(!is.null(u)) u <- as.integer(u)
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(u,   msg, "INDEX_VECTOR_NULL_UPPER", nrow(x))
  msg <- checkArg(FUN, msg, "FUNCTION")
  checkArgStop(msg)
  #    - ----------------------------------
  
  if(is.null(u)){
    if(all(x@z0 > x@z[1])){
      # Dt <- min(x@z0) - x@z[1]  # time before time-zero
      # u <- x@z[1] + 0:round((Dt*0.9)/x@dz)
      
      # 90% samples before time0 (computed individually for each trace)
      # computation independent of the time axis
      spls <- sapply(x@z0, function(y, d, a= 0.9){floor(a * sum(d <= y))}, 
                     x@z)
      xDepth <- matrix(seq_along(x@z), byrow = TRUE, 
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
    OUT <- apply(x@data[u, ], 2, FUN, ...)
  }
  x_shift <- matrix(OUT, nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
  x <-  x - x_shift
  
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
})