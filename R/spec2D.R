

setGeneric("spec2D", function(x, plotSpec = TRUE,
                              unwrapPhase = TRUE, ...) 
  standardGeneric("spec2D"))


#' Frequency spectrum the traces (1D)
#' 
#' Based on the function `fft()` of R base
#' 
#' @export
setMethod("spec2D", "GPR", function(x, plotSpec = TRUE,
                                    unwrapPhase = TRUE, ...){
  # FIXME: don't use "spec()" but "trFFT()"
  S <- .FKSpectrum(x@data, dx = x@dx, dz = x@dz, plotSpec = plotSpec, ...)
  # X <- spec(x, plotSpec = plotSpec, unwrapPhase = unwrapPhase)
  names(X)
  length(X$freq)
  dim(X$pow)
  dim(X$pha)
  dim(x)
  
  xs <- as.GPRset.GPR(x[seq_along(X$freq), ])
  xs@data <- array(0, dim(xs) + c(0,0,1 ))
  xs@data[,,1] <- X$pow
  xs@data[,,2] <- X$pha
  xs@sets <- 1:2
  xs@setnames <- c("power", "phase")
  xs@setunit <- "-"
  xs@depth <- X$freq
  xs@depthunit <- "MHz"
  xs@pos <- X$wbn
  xs@posunit <- "m"
  proc(xs) <- getArgs()
  return(xs)
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


.FKSpectrum <- function(A, dx = 0.25, dz = 0.8, npad = 1, 
                        p = 0.01, plotSpec = TRUE){
  # A <- GPR$data    #[90:1000,]
  nr <- nrow(A)  # time  
  nc <- ncol(A)  # x  
  
  #============== PLOT F-K SPECTRUM ===============#
  # padding (try also 2*(2^nextpow2(nc))
  nk <- npad*(nextpower2(nc))
  nf <- npad*(nextpower2(nr))
  A1 <- matrix(0,nrow=nf,ncol=nk)
  A1[1:nr,1:nc] <- A
  
  # function to center the spectrum!! (no need of fttshift!)
  #centres spectrum: Gonzalez & Wintz (1977) Digital Image Processing p.53
  A1  <- A1 * (-1)^(row(A1) + col(A1))
  A1_fft <- stats::fft(A1)
  A1_fft_pow <- Mod(A1_fft)
  A1_fft_phase <- Arg(A1_fft)
  # plotGPR((A1_fft_phase[1:(nf/2),])^0.05)
  
  # Sampling frequency [Hz] = 1 / Sample time [s]
  Fs = 1/(dz*10^(-9))
  fac = 1000000
  fre = Fs*seq(0,nf/2)/nf/fac
  
  # wavenumber
  Ks <- 1/dx      # [1/m] Sampling frequency
  knu <- 1:(nk/2)/(2*(nk/2)) * Ks  #[1/m]
  knutot <- c(-rev(knu),knu)
  
  # labels: find a function between "xat" and "xLabels" and use "pretty()"
  xat   <- c(0,nk/2,nk)/nk
  xLabels <- c(min(knutot), 0, max(knutot))
  yat    <- c(0,nf/2,nf)/nf
  yLabels  <- c(0, max(fre)/2, max(fre))
  
  # Note: when plotting spectra (S)  use log(S) or S.^alpha (alpha=0.1-0.3) to
  #       increase the visibility of small events 
  # p = 0.05
  if(plotSpec){
    plot3D::image2D(x = knutot, y = fre, z = (t(A1_fft_pow[1:(nf/2),])^p), 
                    xlab="wavenumber (1/m)",
                    ylab="frequency MHz")
    #      axis(side=4, labels=TRUE)
    
  }
  return(list(pow=A1_fft_pow[1:(nf/2),], 
              pha=A1_fft_phase[1:(nf/2),],
              fre = fre,
              wnb = knutot))
}