

#' Shift trace vertically
#'
#' Shift traces vertically by an amount of depth (time) units. New traces 
#' are interpolated.
#' 
#' Modified slots
#' \itemize{
#'   \item \code{data}: trace shifted. The number of rows of data may 
#'         be smaller if \code{crop = TRUE}.
#'   \item \code{proc}: updated with function name and arguments.
#' }
#'
#' @param x      [\code{GPR class}] An object of the class \code{GPR}
#' @param ts     [\code{numeric}] Amount of time (or depth, depending on the
#'               trace unit) to shift the traces. 
#'               \code{ts} is eiter a single value (all the traces are shifted by 
#'               the same amount \code{ts}) or a vector with \eqn{m} elements 
#'               (\eqn{m} is equal to the number of traces).
#' @param method [\code{character(1)}] Interpolation method to be applied:
#'               one of \code{pchip}, \code{linear}, \code{nearest}, 
#'               \code{spline}, \code{cubic}, \code{none} 
#'               (see also \code{\link[signal]{interp1}}). 
#'                \code{"none"} means that the trace is shifted by the
#'               amount of trace samples the closest to \code{ts} without
#'               interpolation.
#' @param crop   [\code{logical(1)}] 
#'               If \code{TRUE} (default), remove the rows containing only 
#'               zero's (no data).
#' @param track [\code{logical(1)}] If \code{TRUE}, processing will be tracked.              
#' @return [\code{GPR class}] An object of the class GPR.
# #' @seealso \code{\link{time0Cor}} to shift the traces such that they start
# #'          at time-zero.
#' @name shift
setGeneric("shift", function(x, ts,
                                    method = c("pchip", "linear", "nearest", 
                                               "spline", "cubic", "none"), 
                                    crop = TRUE, track = TRUE) 
  standardGeneric("shift"))

#' @rdname shift
#' @export
setMethod("shift", "GPR", 
          function(x, ts, 
                   method = c("pchip", "linear", "nearest", "spline", 
                              "cubic", "none"), 
                   crop = TRUE, track = TRUE){
  
  method <- match.arg(method[1], c("spline", "linear", "nearest", "pchip", 
                                "cubic", "none"))
  if(length(ts) == 1){
    ts <- rep(ts, ncol(x))
  }
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(ts,     msg, "NUMERIC_LEN", c(1, ncol(x)))
  msg <- checkArg(method, msg, "STRING_CHOICE", 
                  c("pchip", "linear", "nearest", "spline", "cubic", "none"))
  msg <- checkArg(crop,   msg, "LOGICAL_LEN", 1)
  msg <- checkArg(track,   msg, "LOGICAL_LEN", 1)
  checkArgStop(msg)
  #------------------- end check
  
  if(any(ts != 0)){
    x <- .traceShift(x, ts = ts, method = method, crop = crop)
    if(isTRUE(track)) proc(x) <- getArgs()
  }else{
    warning("Nothing shifted because all 'ts' values are equal to zero!")
  }
  
  return(x)
})


# private function
.traceShift <- function(x, ts, method = c("pchip", "linear", "nearest", "spline", 
                                          "cubic", "none"), crop = TRUE){
  x@data <- .traceShiftMat(x@data, ts = ts, tt = x@z, method = method)
  if(crop == TRUE){
    testCrop <- apply(abs(x@data), 1, sum)
    x <- x[!is.na(testCrop), ]
  }
  return(x)
}

# FIXME > vectorise that!
.traceShiftMat <- function(A, ts = 0, tt = NULL, method = "linear"){
  # ps <- ts/dz
  Anew <- matrix(NA, nrow = nrow(A), ncol = ncol(A))
  # v0 <- 1:nrow(A)
  for(i in seq_len(ncol(A))){
    # relts <- floor(ps[i])*dz - ts[i]
    if(method == "none"){
      ynew <- A[,i]
    }else{
      ttnew <- tt + ts[i]
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