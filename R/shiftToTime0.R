#' Time zero correction
#'
#' \code{shiftToTime0} shift the traces vertically such that they start at
#' time zero (time zero of the data can be modified with the function).
#' New traces are interpolated.
#'
#' This function is a wrapper for the following commands
#' \itemize{
#'   \item \code{x <- traceShift( x,  -time0(x), method = method, crop = crop)}
#'   \item \code{time0(x) <- 0}
#' }
#' 
#' Modified slots
#' \itemize{
#'   \item \code{data}: trace shifted. The number of rows of data may 
#'         be smaller if \code{crop = TRUE}.
#'   \item \code{time0}: set to 0.
#'   \item \code{proc}: updated with function name and arguments.
#' }
#'  
#' @param x      [\code{GPR* object}] An object of the class \code{GPR}
#' @param method [\code{character(1)}] Interpolation method to be applied
#'               (one of \code{pchip} \code{linear}, \code{nearest}, 
#'               \code{spline}, \code{cubic}, \code{none}, 
#'               see also \code{\link[signal]{interp1}}). 
#'                \code{"none"} means that the trace is shifted by the
#'               amount of trace samples the closest to \code{ts} without
#'               interpolation.
#' @param crop   [\code{logical(1)}] 
#'               If \code{TRUE} (default), remove the rows containing only 
#'               zero's (no data).
#' @param track [\code{logical(1)}] If \code{TRUE}, processing will be tracked.             
#' @return [\code{GPR class}] An object of the class \code{GPR}
#' @name shiftToTime0
# #' @examples
# #' data(frenkeLine00)
# #' tfb <- firstBreak(frenkeLine00)
# #' t0 <- firstBreakToTime0(tfb, frenkeLine00, c0 = 0.299)
# #' time0(frenkeLine00) <- t0
# #' frenkeLine00_2 <- shiftToTime0(frenkeLine00, method = "pchip")
# #' 
# #' @seealso \code{\link{firstBreak}} to estimate the first wave break;
# #'          \code{\link{firstBreakToTime0}} to convert the first wave break
# #'          into time zero.
# #'          \code{\link{time0}} and \code{\link{setTime0}} to set time-zero;
# #'          \code{\link{traceShift}} to shift the traces
setGeneric("shiftToTime0", function(x,
                                method = c("pchip", "linear", "nearest", 
                                           "spline", "cubic", "none"), 
                                crop = TRUE, track = TRUE) 
  standardGeneric("shiftToTime0"))



#' @rdname shiftToTime0
#' @export
setMethod("shiftToTime0", "GPR", function(x,
                                      method = c("pchip", "linear", "nearest", 
                                                 "spline", "cubic", "none"), 
                                      crop = TRUE, track = TRUE){
  method <- method[1]

  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(method, msg, "STRING_CHOICE", 
                  c("pchip", "linear", "nearest", "spline", "cubic", "none"))
  msg <- checkArg(crop,   msg, "LOGICAL_LEN", 1)
  msg <- checkArg(track,   msg, "LOGICAL_LEN", 1)
  checkArgStop(msg)
  #-----------------------------------
  
  # ts <- -x@z0 
  if(any(x@z0 != 0)){
    x <- .traceShift(x, z = x@z0, method = method, crop = crop)
    x@z0 <- rep(0, ncol(x@data))
    if(isTRUE(track)) proc(x) <- getArgs()
    return(x)
  }else{
    message("Nothing shifted because all 't0' values are equal to zero!")
    return(x)
  }
})
