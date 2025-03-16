
#' Return time-zero
#' 
#' @name time0
#' @rdname time0
#' @export
setGeneric("time0",
           function(obj)
             standardGeneric("time0"))


#' Wrapper for \code{time0()<-}
#'
#' @name time0<-
#' @rdname time0
#' @export
setGeneric("time0<-", function(obj, value) standardGeneric("time0<-"))


#' 'time-zero' of every traces
#' 
#' \code{time0} returns the 'time-zero' of every traces. Generally, 
#' 'time-zero' corresponds to the first wave arrival (also called first wave
#' break).
#' 
#' @param obj (`GPR* object`)
#' @param t0 (`numeric[n]`) Time-zero with `n = 1` or `n = ncol(obj)`
#' @param track (`logical[1]`) Should the processing step be tracked? 
#' @param value (`numeric[n]`) Time-zero with `n = 1` or `n = ncol(obj)`
#' @return A vector containing the time-zero values of each traces.
#' @seealso [pickFirstBreak()]  to estimate the first wave break.
#' @name setTime0
#' @rdname time0
#' @export
# @aliases time0-methods time0<- time0<--methods
setGeneric("setTime0",
           function(obj, t0, track = TRUE)
             standardGeneric("setTime0"))


#' @rdname time0
#' @export
setMethod("time0", "GPR", function(obj){
  return(obj@z0)
} 
)




#' @rdname time0
#' @export
setReplaceMethod(
  f="time0",
  signature="GPR",
  definition=function(obj, value){
    
    value <- as.numeric(value)
    
    #------------------- check arguments
    msg <- checkArgInit()
    msg <- checkArg(value,     msg, "NUMERIC_LEN", c(1, ncol(obj)))
    checkArgStop(msg)
    #------------------- check arguments
    
    if(length(value) == 1) value <- rep(value, ncol(obj))
    obj@z0 <- value
    
    obj@proc <- c(obj@proc, "time0<-")
    return(obj)
  }
)


#' @rdname time0
#' @export
setMethod("setTime0", "GPR", function(obj, t0, track = TRUE){
  
  t0 <- as.numeric(t0)
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(t0,     msg, "NUMERIC_LEN", c(1, ncol(obj)))
  msg <- checkArg(track,   msg, "LOGICAL_LEN", 1)
  checkArgStop(msg)
  
  if(length(t0) == 1) t0 <- rep(t0, ncol(obj))
  obj@z0 <- t0
  #------------------- check arguments
  
  if(isTRUE(track)) proc(obj) <- getArgs()
  return(obj)
})