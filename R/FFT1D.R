
setGeneric("FFT1D", function(x) 
  standardGeneric("FFT1D"))


#' Fast Fourier Transform applied to the traces (1D)
#' 
#' Based on the function `fft()` of R base
#' 
#' @export
setMethod("FFT1D", "GPR", function(x){
  X <- .trFFT(x@data, dT = x@dz, fac = 1000000)
  x <- x[1:nrow(X$X), ]
  x@data <- X$X
  x@depth <- X$freq
  x@depthunit <- "MHz"
  proc(x) <- getArgs()
  return(x)
})



# @param [matrix]/[vector]   A     (each column represent a trace / a trace)
# @param [double]       dT     (sampling time in nanoseconde)  
# @param [double]      fac    (result multiplied by a factor, e.g. to get MHz 
# instead of Hz)

.trFFT <- function(A, dT = 0.8, fac = 1000000){
  A0   <- as.matrix(A)
  nr  <- nrow(A0)
  nc  <- ncol(A0)
  # N   <- 2^(ceiling(log2(nr)))
  N <- nr
  if((N %% 2) != 0) N <- N + 1
  A <- matrix(0, nrow = N, ncol = nc)
  A[1:nr,1:nc] <- A0
  
  # samping interval GPR = 0.8 ns
  Ts    <- dT*(10^(-9))     # [s] Sample time
  Fs    <- 1/Ts             # [Hz] Sampling frequency
  Fc    <- 1/(2*Ts)         # Nyquist frequency
  nfreq <- N/2 + 1
  
  # if y <- fft(z), then z is 
  # fft(y, inverse = TRUE) / length(y).
  # each column = discrete Fourier transform. 
  fft_A <- stats::mvfft(A)[1:nfreq,,drop=FALSE]
  
  fre = Fs*seq(0, N/2)/N/fac
  
  return(list(X = fft_A, freq = fre))
}
