#' Frequency spectrum the traces (1D)
#' 
#' Based on the function `fft()` of R base
#' 
#' @param obj (`GPR* object`)
# @param MARGIN (`integer[2]`) a vector giving the subscripts which the 
# function will be applied over. E.g., for a matrix 1 indicates rows, 
# 2 indicates columns. 
#' @param plotSpec (`logical[1]`) Plot spectrum?
#' @param unwrapPhase (`logical[1]`) Unwrap phase?
#' @param ... Additional parameters
#' @return (`GPRset`) Sets = Amplitude and Phase as a function of frequency
#' @name freqSpectrum
#' @rdname freqSpectrum
#' @export
setGeneric("freqSpectrum", function(obj, #MARGIN = 2, 
                                    plotSpec = TRUE,
                              unwrapPhase = TRUE, ...) 
  standardGeneric("freqSpectrum"))



#' @rdname freqSpectrum
#' @export
setMethod("freqSpectrum", "GPR", function(obj, # MARGIN = 2, 
                                          plotSpec = TRUE,
                                    unwrapPhase = TRUE, ...){
  # FIXME: don't use "spec()" but "trFFT()"
  # X <- spec(x, plotSpec = plotSpec, unwrapPhase = unwrapPhase)
  if(!isSamplingRegular(obj, axes = 2)) warning("Sampling along z-axis is not regular!!!")
  X <- powSpec(obj@data, dT = mean(diff(obj@z)), fac = 1000000, 
               plotSpec = plotSpec, titleSpec = obj@name)
  # names(X)
  # length(X$freq)
  # dim(X$pow)
  # dim(X$pha)
  # dim(x)
  
  objs <- as.GPRset.GPR(obj[seq_along(X$freq), ])
  objs@data <- array(0, dim(objs) + c(0,0,1 ))
  objs@data[,,1] <- X$pow
  objs@data[,,2] <- X$pha
  objs@z <- X$freq
  objs@zunit <- "MHz"
  objs@y <- 1:2
  objs@ylab <- c("power", "phase")
  objs@yunit <- c("mV", "rad")
  proc(objs) <- getArgs()
  return(objs)
})



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





# @return power spectrum (frequency, power, phase)
# -------------------------------------------
powSpec <- function(A, dT = 0.8, fac = 1000000, plotSpec = TRUE, 
                    titleSpec = NULL, unwrapPhase = TRUE){
  # A0   <- as.matrix(A)
  # nr  <- nrow(A0)
  # nc  <- ncol(A0)
  # # N   <- 2^(ceiling(log2(nr)))
  # N <- nr
  # if((N %% 2) != 0) N <- N + 1
  # A <- matrix(0, nrow = N - nr, ncol = nc)
  # A[1:nr,1:nc] <- A0
  # 
  # # samping interval GPR = 0.8 ns
  # Ts    <- dT*(10^(-9))     # [s] Sample time
  # Fs    <- 1/Ts             # [Hz] Sampling frequency
  # Fc    <- 1/(2*Ts)         # Nyquist frequency
  # nfreq <- N/2 + 1
  # 
  # # if y <- fft(z), then z is 
  # # fft(y, inverse = TRUE) / length(y).
  # # each column = discrete Fourier transform. 
  # fft_A <- stats::mvfft(A)
  # # extract the power spectrum (sometimes referred to as "magnitude")
  # pow <- as.matrix(Mod(fft_A))
  # pow <- pow[1:nfreq,,drop=FALSE]   # select only first half
  # # extract the phase which is atan(Im(fourier)/Re(fourier))
  # pha <- as.matrix(Arg(fft_A))
  # pha <- pha[1:nfreq,,drop=FALSE]    # # select only first half
  # 
  # pow_mean <- apply(pow, 1, mean, na.rm = TRUE)
  # if(unwrapPhase){
  #   pha <- apply(pha, 2, signal::unwrap)
  # }
  # pha_mean <- apply(pha, 1, mean, na.rm = TRUE)
  # 
  # # frequenceS
  # fre = Fs*seq(0, N/2)/N/fac
  # # fre <- seq(from = Fs/2,
  # #            to = nf*Fs,
  # #            by = Fs)
  
  fft_A <- .trFFT(A = A, dT = dT, fac = fac)
  pow <- Mod(fft_A$X)
  pha <- Arg(fft_A$X)
  if(unwrapPhase){
    pha <- apply(pha, 2, signal::unwrap)
  }
  fre <- fft_A$freq
  
  # plot the power spectrum
  if(plotSpec){
    pha_mean <- apply(pha, 1, mean, na.rm = TRUE)
    pow_mean <- apply(pow, 1, mean, na.rm = TRUE)
    op <- par(no.readonly=TRUE)
    m = seq(0, 10000, by = 50)
    par(mfrow=c(2,1))
    par(mar=c(0, 4, 4, 2) + 0.1, oma=c(1,1,1,1) )
    plot(fre,pow_mean, type="n",
         xaxt = "n",
         ylim=c(0,max(pow)),
         ylab="amplitude",xlab="")
    if(!is.null(dim(A))){
      invisible( apply(pow, 2, lines, x = fre, 
                       col=rgb(0.2,0.2,0.2,7/max(ncol(A),7))) )
    }
    lines(fre,pow_mean,col="red")
    axis(side = 1, tcl = +0.3,  labels = FALSE)
    if(!is.null(titleSpec)){
      title(titleSpec)
    }
    grid()
    par(mar=c(4, 4, 0.3, 2))
    plot(fre,pha_mean, type="n", 
         xaxt = "n",
         ylim=range(pha), 
         xlab = "frequency MHz", ylab="phase") 
    if(!is.null(dim(A))){
      invisible(  apply(pha, 2, lines, x = fre, 
                        col = rgb(0.2,0.2,0.2,7/max(ncol(A), 7))) )
    }
    lines(fre,pha_mean,col="red")
    axis(side = 1, tcl = +0.3,  labels = TRUE)
    grid()
    par(op)
  }
  return(list(freq = fre, pow = pow, pha = pha))
}
