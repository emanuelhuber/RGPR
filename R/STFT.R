

setGeneric("STFT", function(x, w) 
  standardGeneric("STFT"))


#' Short-time Fourier Transform (STFT)
#' 
#' The STFT can be seen as a sliding window that takes at each sample-window 
#' a Fast-Fourier Transform of the windowed signal. 
#'
#' DFT with N = 128:
#' The  time  domain  signal  is Contained  in  the  array:  
#' x[1] to x[128]. The  frequency  domain  signals  are contained  in  the  
#' two  arrays: ReX[1] to ReX[65], and to ImX[1] to ImX[65].
#' Notice that 128 points in the time domain corresponds to 65 points in each 
#' of the frequency domain signals.
#' That is, N  points in the time domain corresponds to points in the
#' N/2 + 1 frequency domain (not N/2 points).
#'  Forgetting about this extra point is a common bug in DFT programs.
#' ReX[1] holds the average value of all the points.
#' ImX[1] is equal to zero.
#' In R, the fft returns: c(ReX[1:(N/2 + 1)], ImX[(N/2 + 1):2]) (??)
#' @export
setMethod("STFT", "GPR", function(x, w){
  Fs <- 1/x@dz
  X <- .STFT(x@data, w, Fs)
  dim(X$x)
  xs <- as.GPRset.GPR(x[seq_len(dim(X$x)[1]), ])
  xs@data <- X$x
  
  
  xs@sets <- X$freq * 1000
  xs@setnames <- paste0(X$freq, " MHz")
  xs@setunit <- "MHz"
  proc(xs) <- getArgs()
  return(xs)
})


.STFT <- function(x, w, Fs){
  w <- round(w * Fs)
  # print(w)
  # w <- nextpower2(w)
  if((w %% 2) == 1) w <- w +1
  n <-  w/2 + 1
  mvfftpower <- function(x, n = NULL){
    u <- Mod(stats::mvfft(x * hammingWindow(2*(n-1)))[1:n, ])
    return(u)
  }
  X <- wapplyRow(x = as.matrix(x), width = w, by = 1, FUN = mvfftpower, 
                 n = n)
  X <- aperm(X, perm = c(3, 2, 1))
  k <- dim(X)[3]
  # X[,,1:k] <- X[,,k:1]
  fre <- Fs * seq(0, w/2)/w
  return(list(x = X, freq = (fre)))
}
  



# After you have generated the spectral slices, there are a number of decisions 
# for displaying them. First the phase information is discarded and the energy 
# normalized:
#   
#   S = abs(S); S = S/max(S)
# 
# Then the dynamic range of the signal is chosen. Since information in speech 
# is well above the noise floor, it makes sense to eliminate any dynamic range 
# at the bottom end. This is done by taking the max of the magnitude and some 
# minimum energy such as minE=-40dB. Similarly, there is not much information 
# in the very top of the range, so clipping to a maximum energy such as 
# maxE=-3dB makes sense:
#   
#   S = max(S, 10^(minE/10)); S = min(S, 10^(maxE/10))
# 
# The frequency range of the FFT is from 0 to the Nyquist frequency of one half 
# the sampling rate. If the signal of interest is band limited, you do not need 
# to display the entire frequency range. In speech for example, most of the 
# signal is below 4 kHz, so there is no reason to display up to the Nyquist 
# frequency of 10 kHz for a 20 kHz sampling rate. In this case you will want to 
# keep only the first 40% of the rows of the returned S and f. More generally, 
# to display the frequency range [minF, maxF], you could use the following row 
# index:
#   
#   idx = (f >= minF & f <= maxF)


