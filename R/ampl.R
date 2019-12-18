#' Amplitude envelope
#'
#' Estimate for each trace the amplitude envelope with the Hilbert
#' transform (instataneous amplitude).
#' @param x An object of the class GPR.
#' @param method [\code{character}] Method to use. See details.
#' @param npad [\code{integer(1)}] Only for \code{method = "hilbert"}. Positive 
#'             value defining the number of values to
#'             pad \code{x} (the padding help to minimize the Gibbs effect at 
#'             the begining and end of the data caused by the Hilbert 
#'             transform).
#' @param threshold [\code{integer(1)}] Threshold value for peak detection. The
#'             larger the value the more time consuming is the algorithm.
#' @details Two methods:
#' \itemize{
#'  \item \code{hilbert} Envelope based on the Hilbert transform:
#'       \code{x_{ampl} = \sqrt(x^2 + \mathcal{H}(x)^2)}
#'   \item  \code{peak} The local maxima of the absolute values of the
#'          signal are first estimated. The envelope is determined using spline 
#'          interpolation over local maxima.
#'  }
#' @examples 
#' x <- frenkeLine00
#' 
#' plot(x[,10], lwd = 2)
#' lines(abs(x[,10]), col = "blue")
#' lines(envelope(x[,10], method = "hilbert"), col = "red")
#' lines(envelope(x[,10], method = "peak"), col = "green")
#' 
#' @name envelope
#' @rdname envelope
#' @export
setMethod("envelope", "GPR", function(x, method = c("hilbert", "peak"),
                                      npad = 100, threshold = 2){
  #if(is.null(FUN)){
  method <- match.arg(method, c("hilbert", "peak"))
  if(method == "hilbert"){
    xmax <- max(abs(x), na.rm = TRUE)
    # xH <- apply(x, 2, HilbertTransf, npad = npad)
    xH <- HilbertTransfMV(x@data, npad = npad)
    x2 <- sqrt(x^2 + base::Re(xH)^2)
    test <- abs(x2@data) > xmax
    x2@data[test] <- abs(x@data[test])
  }else if( method == "peak"){
    x2 <- getAmplLocalMax(x, threshold = threshold)
  }
  proc(x2) <- getArgs()
  return(x2)
}
)

#' Amplitude (deprecated)
#'
#' @name ampl
#' @rdname ampl
#' @export
setMethod("ampl", "GPR", function(x, npad = 100, FUN = NULL, ...){
  warning("Deprecated! Use 'envelope()' instead.")
  if(is.null(FUN)){
    xmax <- max(abs(x), na.rm = TRUE)
    xH <- apply(x, 2, HilbertTransf, npad = npad)
    x <- sqrt(x^2 + base::Re(xH)^2)
    x@data[abs(x@data) > xmax] <- xmax
  }else{
    #funName <- getFunName(FUN)
    x@data[] <- apply(x, 1, FUN, ...)
  }
  # proc(x) <- getArgs( addArgs = c('FUN' = funName))
  proc(x) <- getArgs()
  return(x)
}
)



getAmplLocalMax <- function(x, threshold = 2){
  xabs <- abs(x@data)
  x@data <- apply(xabs, 2, .intpLocalMaxAmpl, x = x@depth, threshold = threshold)
  return(x)
}

# https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
#' @export
localMax <- function(x, threshold = 2, addEnds = TRUE){
  up   <- sapply(1:threshold, function(n) c(x[-seq(n)], rep(NA, n)))
  down <-  sapply(-1:-threshold, function(n) c(rep(NA, abs(n)), x[-seq(length(x), length(x) - abs(n) + 1)]))
  #  a <- cbind(x, up)
  a    <- cbind(x, up, down)
  id <- which(apply(a, 1, max) == a[,1])
  if(addEnds){
    if(id[1] != 1) id <- c(1, id)
    if(tail(id, 1) != length(x)) id <- c(id, length(x))
  }
  return(id)
  # list(minima = which(apply(a, 1, min) == a[,1]), maxima = which(apply(a, 1, max) == a[,1]))
}

# alternative, not used
# # https://stats.stackexchange.com/questions/22974/how-to-find-local-peaks-valleys-in-a-series-of-data
# find_peaks <- function (x, m = 3){
#   shape <- diff(sign(diff(x, na.pad = FALSE)))
#   pks <- sapply(which(shape < 0), FUN = function(i){
#     z <- i - m + 1
#     z <- ifelse(z > 0, z, 1)
#     w <- i + m + 1
#     w <- ifelse(w < length(x), w, length(x))
#     if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
#   })
#   pks <- unlist(pks)
#   pks
# }


.intpLocalMaxAmpl <- function(y, x, threshold = 2){
  test <- localMax(as.numeric(y), threshold = threshold)
  signal::interp1(x = x[test],
                  y = y[test],
                  xi = x,
                  method = "pchip")
}
