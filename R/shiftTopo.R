

#' Shift trace vertically according to topography
#'
#' Shift traces vertically according to topography. New traces 
#' are interpolated.
#' 
#' Modified slots
#' \itemize{
#'   \item `data`: trace shifted. The number of rows of data may 
#'         be smaller if `crop = TRUE`.
#'   \item `proc`: updated with function name and arguments.
#' }
#'
#' @param x      (`GPR`) An object of the class `GPR`
#' @param method (`character[1]`) Interpolation method to be applied:
#'               one of `pchip`, `linear`, `nearest`, 
#'               `spline`, `cubic`, `none` 
#'               (see also [signal::interp1()]). 
#'                `"none"` means that the trace is shifted by the
#'               amount of trace samples the closest to `z` without
#'               interpolation.
#' @param crop   (`logical[1]`) 
#'               If `TRUE` (default), remove the rows containing only 
#'               zero's (no data).
#' @param track (`logical[1]`) If `TRUE`, processing will be tracked.              
#' @return (`GPR class`) An object of the class GPR.
# #' @seealso \code{\link{time0Cor}} to shift the traces such that they start
# #'          at time-zero.
#' @name shiftTopo
setGeneric("shiftTopo", function(x, method = c("pchip", "linear", "nearest", 
                                               "spline", "cubic", "none"), 
                                    crop = FALSE, track = TRUE) 
  standardGeneric("shiftTopo"))

#' @rdname shift
#' @export
setMethod("shiftTopo", "GPR", 
          function(x, method = c("pchip", "linear", "nearest", "spline", 
                              "cubic", "none"), 
                   crop = FALSE, track = TRUE){
  
  method <- match.arg(method[1], c("spline", "linear", "nearest", "pchip", 
                                "cubic", "none"))
  if(isZTime(x))           stop(msg_do_timeToDepth)
  if(length(x@coord) == 0) stop(msg_set_coords)
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(method, msg, "STRING_CHOICE", 
                  c("pchip", "linear", "nearest", "spline", "cubic", "none"))
  msg <- checkArg(crop,   msg, "LOGICAL_LEN", 1)
  msg <- checkArg(track,   msg, "LOGICAL_LEN", 1)
  checkArgStop(msg)
  #------------------- end check
  
  z <- max(x@coord[, 3]) - x@coord[, 3]
  
  if(any(z != 0)){
    x <- .traceShift(x, z = z, method = method, crop = crop)
    if(isTRUE(track)) proc(x) <- getArgs()
  }else{
    warning("Nothing shifted because all 'z' values are equal to zero!")
  }
  
  return(x)
})

