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
#' @param obj (`GPR`) An object of the class `GPR`.
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
           function(obj, u = NULL, FUN = mean, ...,
                    track = TRUE)
             standardGeneric("rmDCShift"))



#' @rdname rmDCShift
#' @export
setMethod("rmDCShift", "GPR", function(obj, u = NULL, FUN = mean, ...,
                                     track = TRUE){
  
  if(!is.null(u)) u <- as.integer(u)
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(u,   msg, "INDEX_VECTOR_NULL_UPPER", nrow(obj))
  msg <- checkArg(FUN, msg, "FUNCTION")
  checkArgStop(msg)
  #    - ----------------------------------
  
  if(is.null(u)){
    if(all(obj@z0 > obj@z[1])){
      # Dt <- min(obj@z0) - obj@z[1]  # time before time-zero
      # u <- obj@z[1] + 0:round((Dt*0.9)/obj@dz)
      
      # 90% samples before time0 (computed individually for each trace)
      # computation independent of the time axis
      spls <- sapply(obj@z0, function(y, d, a= 0.9){floor(a * sum(d <= y))}, 
                     obj@z)
      xDepth <- matrix(seq_along(obj@z), byrow = TRUE, 
                       nrow = ncol(obj), ncol = nrow(obj))
      # test which samples can be used for the computation
      test <- t(xDepth <= spls)
      f <- function(i, x, y, FUN, ...){
        FUN(x[,i][y[,i]], ...)
      }
      OUT <- sapply(1:ncol(obj), f, obj@data, test, FUN, ...)
    }else{
      warning("You must define 'u' or reset time-zero.\n",
              "not enough samples before time-zero...")
      return(obj)
    }
  }else{
    OUT <- apply(obj@data[u, ], 2, FUN, ...)
  }
  obj_shift <- matrix(OUT, nrow = nrow(obj), ncol = ncol(obj), byrow = TRUE)
  obj <-  obj - obj_shift
  
  if(isTRUE(track)) proc(obj) <- getArgs()
  return(obj)
})