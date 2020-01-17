setGenericVerif("conv1D", function(x, w, track = TRUE) standardGeneric("conv1D"))
setGenericVerif("conv2D", function(x, w, track = TRUE) standardGeneric("conv2D"))


#' Trace convolution (1D)
#'
#' Convolution of the GPR traces with a wavelet
#' @param x A GPR data
#' @param w A numeric vector defining a wavelet or a matrix with number of
#'           columns equal to the number of traces.
#' @return The convolved GPR data.
#' @name conv1D
#' @rdname conv1D
#' @export
setMethod("conv1D", "GPR", function(x, w, track = TRUE){
  # rotatePhase <- function(x,phi){
  x@data <- convolution(x@data, w)
  #     x@proc <- c(x@proc,"conv1D")
  if(isTRUE(track)) proc(x) <- getArgs()  #proc(x) <- "conv1D"
  return(x)
}
)
#' 2D onvolution
#'
#' Convolution of the GPR data with a kernel
#' @param x A GPR data
#' @param w A numeric matrix with smaller dimension than the GPR data.
#' @return The convolved GPR data.
#' @name conv2D
#' @rdname conv2D
#' @export
setMethod("conv2D", "GPR", function(x, w, track = TRUE){
  # rotatePhase <- function(x,phi){
  x@data <- convolution2D(x@data, w)
  # x@proc <- c(x@proc, "conv2D")
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
}
)

#' Two-dimensional convolution
#' 
#' The convolution is performed with 2D FFT
#' @name convolution2D
#' @rdname convolution2D
#' @export
convolution2D <- function(A,k){
  nA = nrow(A)
  mA = ncol(A)
  nk = nrow(k)
  mk = ncol(k)
  if(nk > nA || mk > mA){
    stop("Kernel 'k' should be smaller than the matrix 'A'\n")
  }
  A0 <- paddMatrix(A, nk, mk)
  nL <- nrow(A0)
  mL <- ncol(A0)
  k0 <- matrix(0, nrow=nL, ncol=mL)
  # h0[(nk-1) + 1:nh, (mk-1) + 1:mh] <- A
  #   A0[1:nA,  1:mA] <- A
  k0[1:nk, 1:mk] <- k
  g <- Re(stats::fft(stats::fft(k0)*stats::fft(A0),inverse=TRUE))/(nL * mL)
  g2 <- g[nk + nk/2  + (1:nA), mk +mk/2 + (1:mA)]
  # g2 <- g[nk + 1:nh, mk + 1:mh]
  return(g2)
}