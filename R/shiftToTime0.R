#' Time zero correction
#'
#' `shiftToTime0` shift the traces vertically such that they start at
#' time zero (time zero of the data can be modified with the function).
#' New traces are interpolated.
#'
#' This function is a wrapper for the following commands
#' \itemize{
#'   \item `obj <- traceShift( obj,  -time0(obj), method = method, crop = crop)`
#'   \item `time0(obj) <- 0`
#' }
#' 
#' Modified slots
#' \itemize{
#'   \item `data`: trace shifted. The number of rows of data may 
#'         be smaller if `crop = TRUE`.
#'   \item `time0`: set to 0.
#'   \item `proc`: updated with function name and arguments.
#' }
#'  
#' @param obj      (`GPR* object`) An object of the class `GPR`
#' @param method (`character[1]`) Interpolation method to be applied
#'               (one of `pchip` `linear`, `nearest`, 
#'               `spline`, `cubic`, `none`, 
#'               see also [signal::interp1()]). 
#'                `"none"` means that the trace is shifted by the
#'               amount of trace samples the closest to `ts` without
#'               interpolation.
#' @param crop   (`logical[1]`) 
#'               If `TRUE` (default), remove the rows containing only 
#'               zero's (no data).
#' @param track (`logical[1]`) If `TRUE`, processing will be tracked.             
#' @return (`GPR class`) An object of the class `GPR`
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
setGeneric("shiftToTime0", function(obj,
                                method = c("pchip", "linear", "nearest", 
                                           "spline", "cubic", "none"), 
                                crop = TRUE, track = TRUE) 
  standardGeneric("shiftToTime0"))



#' @rdname shiftToTime0
#' @export
setMethod("shiftToTime0", "GPR", function(obj,
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
  
  # ts <- -obj@z0 
  if(any(obj@z0 != 0)){
    obj <- .traceShift(obj, z = obj@z0, method = method, crop = crop)
    obj@z0 <- rep(0, ncol(obj@data))
    if(isTRUE(track)) proc(obj) <- getArgs()
    return(obj)
  }else{
    message("Nothing shifted because all 't0' values are equal to zero!")
    return(obj)
  }
})
