


setGeneric("analyticSignal", function(x, npad = 100) 
  standardGeneric("analyticSignal"))

#' Analytic signal (complex trace)
#'
#' @name analyticSignal
#' @rdname analyticSignal
#' @export
setMethod("analyticSignal", "GPR", function(x, npad = 100){
# trComplex <- function(x, npad = 100){
  xH <- HilbertTransfMV(x@data, npad = npad)
  x <- x + 1i*base::Re(xH)
  proc(x) <- getArgs()
  return(x)
})


setGeneric("instPhase", function(x, npad = 100, unwrapPhase = TRUE) 
  standardGeneric("instPhase"))

#' Instantaneous phase
#' 
#' @name instPhase
#' @rdname instPhase
#' @export
setMethod("instPhase", "GPR", function(x, npad = 100, unwrapPhase = TRUE){
  xH <- HilbertTransfMV(x@data, npad = npad)
  xphase <- atan2(base::Re(xH), as.matrix(x))
  if(isTRUE(unwrapPhase)){
    xphase <- apply(xphase, 2, signal::unwrap)
  }
  x[] <- xphase
  proc(x) <- getArgs()
  return(x)
})


setGeneric("instAmpl", function(x, npad = 100) 
  standardGeneric("instAmpl"))

#' Instantaneous amplitude
#' 
#' @name instAmpl
#' @rdname instAmpl
#' @export
setMethod("instAmpl", "GPR", function(x, npad = 100){
  xH <- HilbertTransfMV(x@data, npad = npad)
  x <- sqrt(x^2 + base::Re(xH)^2)
  proc(x) <- getArgs()
  return(x)
})


#' Phase rotation
#'       
#' shift the phase of signal by phi (in radian)
#' @export
phaseRotation <- function(x, phi){
  # # x <- as.numeric(x)
  # n <- length(x)
  # x <- c(rev(head(x, npad)), x, rev(tail(x, npad)))   # add MANU
  # nf <- length(x)
  # X <- stats::fft(x)
  # # phi2 <- numeric(nf)
  # # phi2[2:(nf/2)] <- phi
  # phi2 <- rep(phi, nf)
  # phi2[(nf/2+1):(nf)] <- -phi
  # Phase <- exp( complex(imaginary = 1) * phi2)
  # xcor <- stats::fft(X * Phase, inverse = TRUE)/nf
  # return(Re(xcor[npad + 1:n]))
  xH <- base::Re(HilbertTransf(x, npad = 20))
  return(x * cos(phi) - xH * sin(phi))
  # return(Re(xcor))
}


# Hilbert transform
# https://github.com/cran/spectral/blob/master/R/hilbert.R
#' @export
HilbertTransf <- function(x, npad = 10){
  x <- as.numeric(x)
  n <- length(x)
  x <- c(rev(head(x, npad)), x, rev(tail(x, npad)))   # add MANU
  # contrarily to spectral package we compute the FFT
  # (and not the normalized FFT)
  X <- fft(x) # /length(x)  # mod MANU
  
  # then we need a virtual spatial vector which is symmetric with respect to
  # f = 0. The signum function will do that. The advantage is, that we need not
  # take care of the odd-/evenness of the length of our dataset
  xf <- 0:(length(X) - 1)
  xf <- xf - mean(xf)
  
  # because the negative Frequencies are located in the upper half of the
  # FFT-data vector it is nesccesary to use "-sign". This will mirror the relation
  # The "-0.5" effect is that the Nyquist frequency, in case of odd data set lenghts,
  # is not rejected.
  Xh <- -1i * X * (-sign(xf - 0.5))
  xh <- fft(Xh, inverse = TRUE) / length(x)
  # return(xh[1:n])   # add MANU
  return(xh[npad + 1:n])   # add MANU
}
# from package 'hht'
# H <- function (xt) {
#   ndata <- length(xt)
#   h <- rep(-1, ndata)
#   if (ndata %% 2 == 0) {
#     h[c(1, ndata/2 + 1)] <- 0
#     h[2:(ndata/2)] <- 1
#   }
#   else {
#     h[1] <- 0
#     h[2:((ndata + 1)/2)] <- 1
#   }
#   xt <- fft(-1i * sign(h) * fft(xt), inverse = TRUE)/ndata
#   return(xt)
# }

# Hilbert transform
# https://github.com/cran/spectral/blob/master/R/hilbert.R
#' @export
HilbertTransfMV <- function(x, npad = 10){
  if(npad < 0) stop("'npad' must be larger than 0")
  if(is.null(dim(x))) dim(x) <- c(length(x), 1)
  n <- nrow(x)
  # x <- c(rev(head(x, npad)), x, rev(tail(x, npad)))   # add MANU
  if(npad > 0){
    x0 <- matrix(nrow = n + 2*npad, ncol = ncol(x))
    x0[1:npad, ] <- x[npad:1,]
    x0[1:n + npad, ] <- x
    x0[(n+npad+1):(n+2*npad), ] <- x[n:(n-npad+1),]
  }else{
    x0 <- x
  }
  h <- rep(-1, nrow(x0))
  if (nrow(x0) %% 2 == 0) {
    h[c(1, nrow(x0)/2 + 1)] <- 0
    h[2:(nrow(x0)/2)] <- 1
  } else {
    h[1] <- 0
    h[2:((nrow(x0) + 1)/2)] <- 1
  }
  xh <- mvfft(-1i * sign(h) * mvfft(x0), inverse = TRUE)/nrow(x0)
  
  # 
  # # contrarily to spectral package we compute the FFT
  # # (and not the normalized FFT)
  # X <- mvfft(x) # /length(x)  # mod MANU
  # 
  # # then we need a virtual spatial vector which is symmetric with respect to
  # # f = 0. The signum function will do that. The advantage is, that we need not
  # # take care of the odd-/evenness of the length of our dataset
  # xf <- 0:(length(X) - 1)
  # xf <- xf - mean(xf)
  # 
  # # because the negative Frequencies are located in the upper half of the
  # # FFT-data vector it is nesccesary to use "-sign". This will mirror the relation
  # # The "-0.5" effect is that the Nyquist frequency, in case of odd data set lenghts,
  # # is not rejected.
  # Xh <- -1i * X * (-sign(xf - 0.5))
  # xh <- fft(Xh, inverse = TRUE) / length(x)
  # # return(xh[1:n])   # add MANU
  return(xh[npad + 1:n,])   # add MANU
}

