#' @name time0
#' @rdname time0
#' @export
setGeneric("time0",
           function(x)
             standardGeneric("time0"))

#' @name time0<-
#' @rdname time0
setGeneric("time0<-", function(x, value) standardGeneric("time0<-"))


#' @name setTime0
#' @rdname time0
#' @export
setGeneric("setTime0",
           function(x, t0, track = TRUE)
             standardGeneric("setTime0"))

#' 'time-zero' of every traces
#' 
#' \code{time0} returns the 'time-zero' of every traces. Generally, 
#' 'time-zero' corresponds to the first wave arrival (also called first wave
#' break).
#' 
#' @param x An object of the class GPR.
#' @return A vector containing the time-zero values of each traces.
#' @examples
#' data(frenkeLine00)
#' time0(frenkeLine00)
#' @seealso \code{\link{firstBreak}} to estimate the first wave break.
#' @name time0
#' @rdname time0
#' @export
# @aliases time0-methods time0<- time0<--methods
setMethod("time0", "GPR", function(x){
  return(x@z0)
} 
)


#' @name time0<-
#' @rdname time0
#' @export
setReplaceMethod(
  f="time0",
  signature="GPR",
  definition=function(x, value){
    
    value <- as.numeric(value)
    
    #------------------- check arguments
    msg <- checkArgInit()
    msg <- checkArg(value,     msg, "NUMERIC_LEN", c(1, ncol(x)))
    checkArgStop(msg)
    #------------------- check arguments
    
    if(length(value) == 1) value <- rep(value, ncol(x))
    x@z0 <- value
    
    x@proc <- c(x@proc, "time0<-")
    return(x)
  }
)

#' Wrapper for \code{time0()<-}
#'
#' @name setTime0
#' @rdname time0
#' @export
setMethod("setTime0", "GPR", function(x, t0, track = TRUE){
  
  t0 <- as.numeric(t0)
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(t0,     msg, "NUMERIC_LEN", c(1, ncol(x)))
  msg <- checkArg(track,   msg, "LOGICAL_LEN", 1)
  checkArgStop(msg)
  
  if(length(t0) == 1) t0 <- rep(t0, ncol(x))
  x@z0 <- t0
  #------------------- check arguments
  
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
})