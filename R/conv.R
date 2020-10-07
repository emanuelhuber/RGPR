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

# # linear convolution with fft
# # a = vector
# # b = vector
# convolution <- function(a,b){
#   na <- length(a)
#   nb <- length(b)
#   L <- na + nb - 1
#   a0 <- c(a,rep(0,nb-1))
#   b0 <- c(b, rep(0,na-1))
#   y <- Re(fft(fft(a0)*fft(b0),inverse=TRUE))/L
#   return(y[1:(max(na,nb))])
# }


#' Linear convolution based on FFT
#'
#' If x (or w) is a numeric vector, it is converted into a one-column 
#' matrix. Then if x and B do not have the same number of column, then the 
#' first column of the matrix with the smallest number of column is repeated to
#' match the dimension of the other matrix.
#' match the dimension of the other matrix.
#' @param x A numeric vector or matrix: the signal to be convolued with 
#'          \code{w}.
#' @param w A numeric vector or matrix: the wavelet.
#' @name convolution
#' @rdname convolution
#' @export
# gives the same results as
# y <- convmtx(w, n = n) %*% r         # y : observed data
convolution <- function(x, w){
  if(is.null(dim(x))){
    dim(x) <- c(length(x), 1)
  }
  if(is.null(dim(w))){
    dim(w) <- c(length(w), 1)
  }
  if(ncol(w) < ncol(x)){
    # FIXME -> mabye w <- w[, 1] would be enough
    w <- repmat(w[, 1, drop = FALSE], 1, ncol(x)) 
  }
  #   else if(ncol(B) > ncol(x)){
  #     x <- repmat(x[,1, drop = FALSE], 1, ncol(B)) 
  #   }
  nx <- nrow(x)
  nw <- nrow(w)
  xpad <- paddMatrix(x, nw, 0)
  w0 <- matrix(0, nrow = nrow(xpad), ncol= ncol(xpad))
  w0[1:nw, ] <- w
  Y <- Re(stats::mvfft(stats::mvfft(xpad) * stats::mvfft(w0), 
                       inverse=TRUE))/nrow(xpad)
  return(Y[nw + seq_len(nx), ])
}


# cf. matlab
# A convolution matrix is a matrix, formed from a vector, 
# whose product with another vector 
# is the convolution of the two vectors.

# A = convmtx(y, nf) returns the convolution matrix, A, 
# such that the product of A and a vector, x, 
# is the convolution of y and x. 
# If y is a column vector of length m, A is (m + nf)-by-nf and the 
# product of A and a column vector, x, of length n is the 
# convolution of y and x. 
# convmtx <- function(y, nf){
#   ny <- length(y)
#   L <- nf + ny #-1
#   # convolution matrix Y
#   # yext <- rep(c(y, rep(0, L - ny + 1)), nf)
#   yext <- rep(c(y, rep(0, nf + 1)), nf)
#   yext <- yext[1:(L * nf)]
#   return( matrix(yext, nrow = L, ncol = nf))
# }

#' Returns the convolution matrix
#'  
#' Returns the convolution matrix, A, 
#' such that the product of A and a vector, x, 
#' is the convolution of y and x. 
#' @return If y is a column vector of length m, A is m-by-nf.
#' @export
convmtx <- function(w, n){
  nw <- length(w)
  #L <- nf + nw #-1
  L <- n #-1
  # convolution matrix Y
  # yext <- rep(c(y, rep(0, L - nw + 1)), n)
  w0 <- numeric(L + 1)
  w0[1:nw] <- w
  # yext <- rep(c(y, rep(0, n - nw )), n)
  wext <- rep(w0, n)
  wext <- wext[1:(L * n)]
  W <- matrix(wext, nrow = L, ncol = n)
  W[upper.tri(W)] <- 0
  return(W )
}
