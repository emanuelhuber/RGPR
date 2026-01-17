

#' Shift trace vertically
#'
#' Shift traces vertically by an amount of depth (time) units. New traces 
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
#' @param z     (`numeric`) Amount of time (or depth, depending on the
#'               trace unit) to shift the traces. 
#'               `z` is eiter a single value (all the traces are shifted by 
#'               the same amount `z`) or a vector with \eqn{m} elements 
#'               (\eqn{m} is equal to the number of traces).
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
#' @name shift
setGeneric("shift", function(x, z,
                                    method = c("pchip", "linear", "nearest", 
                                               "spline", "cubic", "none"), 
                                    crop = FALSE, track = TRUE) 
  standardGeneric("shift"))

#' @rdname shift
#' @export
setMethod("shift", "GPR", 
          function(x, z, 
                   method = c("pchip", "linear", "nearest", "spline", 
                              "cubic", "none"), 
                   crop = FALSE, track = TRUE){
  
  method <- match.arg(method[1], c("spline", "linear", "nearest", "pchip", 
                                "cubic", "none"))
  if(length(z) == 1){
    z <- rep(z, ncol(x))
  }
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(z,     msg, "NUMERIC_LEN", c(1, ncol(x)))
  msg <- checkArg(method, msg, "STRING_CHOICE", 
                  c("pchip", "linear", "nearest", "spline", "cubic", "none"))
  msg <- checkArg(crop,   msg, "LOGICAL_LEN", 1)
  msg <- checkArg(track,   msg, "LOGICAL_LEN", 1)
  checkArgStop(msg)
  #------------------- end check
  
  if(any(z != 0)){
    x <- .traceShift(x, z = z, method = method, crop = crop)
    if(isTRUE(track)) proc(x) <- getArgs()
  }else{
    warning("Nothing shifted because all 'z' values are equal to zero!")
  }
  
  return(x)
})


# private function - also used by shiftToTime0()
.traceShift <- function(x, z, method = c("pchip", "linear", "nearest", "spline", 
                                          "cubic", "none"), crop = TRUE){
  x@data <- .traceShiftMat(x@data, z = z, tt = x@z, method = method)
  if(crop == TRUE){
    testCrop <- apply(abs(x@data), 1, sum)
    x <- x[!is.na(testCrop), ]
  }
  return(x)
}

# FIXME > vectorise that!
.traceShiftMat <- function(A, z = 0, tt = NULL, method = "linear"){
  # ps <- z/dz
  Anew <- matrix(NA, nrow = nrow(A), ncol = ncol(A))
  # v0 <- 1:nrow(A)
  for(i in seq_len(ncol(A))){
    # relts <- floor(ps[i])*dz - z[i]
    if(method == "none"){
      stop("Methode 'none' not yet implemented!")
      ynew <- A[,i]
    }else{
      ttnew <- tt + z[i]
      # ynew <- signal::interp1(tt, A[,i], ttnew, method = method, extrap = NA)
      Anew[,i] <- signal::interp1(tt, A[,i], ttnew, method = method, 
                                  extrap = NA)
    }
    # plot(ynew, type = "o")
    # lines(A[,i], col ="red")
    # vs <- v0 + floor(ps[i])
    # test <- vs > 0 & vs <= nrow(A)
    # vs <- vs[test]
    # Anew[vs, i] <- ynew[test]
  }
  return(Anew)
}
