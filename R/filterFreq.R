setGeneric("filterFreq1D", function(x, f = 100, 
                                    type = c('low', 'high', 'bandpass', 'bandpass-reject'),
                                    L = 257, plotSpec = FALSE, track = TRUE) 
  standardGeneric("filterFreq1D")) 

#' Frequency filter
#' 
#' The frequency filter alters the signal amplitude with respect to frequency.
#'   * Low-pass filter: low frequencies are passed, high frequencies are attenuated.
#'   * High-pass filter: high frequencies are passed, low frequencies are attenuated.
#'   * Band-pass filter: only frequencies in a frequency band are passed.
#'   * Band-pass-reject filter: a normally narrow band of frequencies is attenuated.
#'
#' For the low- and high-pass filter, only one cut-off frequency can be defined
#' while the argument `L` will define the filter length of the Hamming
#' window (necessary to reduce ringing artifacts from the Gibbs phenomenon). 
#' If two values are passed to the cut-off frequency argument `f`, 
#' the value of `L` will be ignored.
#' Example for low-pass filter: `f = c(150, 200)`.
#' Example for high-pass filter: `f = c(10, 20)`
#' 
#' For the band-pass filter and the band-pass-reject filter, 
#' only two cut-off frequency can be defined
#' while the argument `L` will define the filter length of the Hamming
#' window (necessary to reduce ringing artifacts from the Gibbs phenomenon). 
#' If four values (the two first corner frequencies followed by the two last
#' corner frequencies ) are passed to the cut-off frequency argument `f`, 
#' the value of `L` will be ignored. 
#' Example: `f = c(10, 20, 150, 200)`
#' 
#' Check this free book: The Scientist and Engineer's Guide to Digital Signal 
#' Processing By Steven W. Smith, Ph.D.
#'
#' @param x An object of the class GPR
#' @param f numeric vector: cut-off frequencies. Cutoff frequency is the 
#'          frequency beyond which the filter will not pass signals.
#'           See Details.
#' @param type length-one character vector: type of frequency vector. `low`
#'             for low-pass filter, `high` for high-pass filter and 
#'             `bandpass` for bandpass filter.
#' @param L length-one numeric defining the filter length. See Details.
#' @param plotSpec boolean. If `TRUE` plot the frequency spectrum as well.
#' @name filterFreq1D
#' @rdname filterFreq
#' @export
#' @concept processing
setMethod("filterFreq1D", "GPR", 
          function(x, f = 100, 
                   type = c('low', 'high', 'bandpass', 'bandpass-reject'),
                   L = 257, plotSpec = FALSE, track = TRUE){
            dz <- mean(diff(x@z))
            x@data <- .filterFreq1D1D(x@data, f = f,  type = type, L = L, dT = dz, 
                                 plotSpec = plotSpec)
            if(isTRUE(track)) proc(x) <- getArgs()
            #   x@proc <- c(x@proc, proc)
            return(x)
          } 
)

setGeneric("filterFreq2D", function(x, fk = NULL, L = c(5 , 5), npad = 1,
                                    track = TRUE) 
  standardGeneric("filterFreq2D")) 

#' Frequency-wavenumber filter
#'
#' @name filterFreq2D
#' @rdname filterFreq
#' @export
setMethod("filterFreq2D", "GPR", function(x, fk = NULL, L = c(5 , 5), npad = 1,
                                      track = TRUE){
  if(is.null(fk)) stop("fk argument has to be specified")
  # if polygon
  if(is.list(fk) && length(fk) == 2){
    areaunclosed <- t(do.call("rbind", fk))
    
    dx <- mean(diff(x@x))
    dz <- mean(diff(x@z))
    
    
    nk <- npad*(nextpower2(ncol(x@data)))
    nf <- npad*(nextpower2(nrow(x@data)))
    # frequency
    Ts = dz*10^(-9)    # [s] Sample time
    fac = 1000000
    fre = (1/Ts)*seq(0,nf/2)/nf/fac
    # wavenumber
    Ks <- 1/dx      # [1/m] Sampling frequency
    knu <- 1:(nk/2)/(2*(nk/2)) * Ks  #[1/m]
    knutot <- c(-rev(knu),knu)
    fk <- outer(fre, knutot, inPoly,
                vertx = areaunclosed[,2],
                verty = areaunclosed[,1])
  }else if(!is.matrix(fk)){
    stop("fk should be either of type 'list' or 'matrix'\n")
  }else if(is.matrix(fk)){
    cat("# FIXME! function to transform matrix into polygon\n")
  }
  x@data <- .FKFilter(x@data, fk = fk, L = L, npad = npad)
  if(isTRUE(track)) proc(x) <- getArgs()
  #   x@proc <- c(x@proc, proc)
  return(x)
  
} 
)


.filterFreq1D1D <- function(A, f = c(100), type = c('low', 'high', 'bandpass',
                                               'bandpass-reject'), 
                       L = 257, dT = 0.8, plotSpec = FALSE, fac = 1000000){
  type <- match.arg(type)
  # A <- as.matrix(A)
  nr <- nrow(A)      # signal length
  
  # samping interval GPR = 0.8 ns
  Ts    <- dT*(10^(-9))     # [s] Sample time
  Fs    <- 1/Ts             # [Hz] Sampling frequency
  
  # cut-off frequency/ies fc in (MHz)
  f <- sort(f) * 10^6    # cut-off frequency in Hz
  
  # FIXME > write a function setfilterFreq1D(f, type=..., Fs)
  if(type == "low" || type == "high"){
    # Design the filter using the window method:
    if(length(f) > 1) {
      BW <- (f[2] - f[1])/Fs          # filter bandwidth
      fc <- f[1] + (f[2] - f[1])/2    # cut frequency
      L <- round(4 / BW)
      if(L %% 2 == 0){
        L <- L + 1  
      }
    }else if(length(f) == 1){
      fc <- f[1]
    }
    h <- winSincKernel(L, fc/Fs, type)
  }else if(grepl("bandpass", type)){
    if(length(f)==2 ) {
      if(L %% 2 == 0){
        L <- L + 1
      }
      h1 <- winSincKernel(L, f[1]/Fs, "low")
      h2 <- winSincKernel(L, f[2]/Fs, "high")
    }else if(length(f) == 4 ){
      BW <- (f[2] - f[1])/Fs
      fc <- f[1] + (f[2] - f[1])/2
      L <- round(4 / BW)
      if(L %% 2 == 0){
        L <- L + 1
      }
      h1 <- winSincKernel(L, fc/Fs, "low")
      BW <- (f[4] - f[3])/Fs
      fc <- f[3] + (f[4] - f[3])/2
      L <- round(4 / BW)
      if(L %% 2 == 0){
        L <- L + 1
      }
      h2 <- winSincKernel(L, fc/Fs, "high")
    }
    L = max(length(h1), length(h2))
    if(length(h2) < L ){
      h2 = c(rep(0, (L-length(h2))/2), 
             h2, 
             rep(0, (L-length(h2))/2))
    }
    if(length(h1) < L ){
      h1 = c(rep(0, (L-length(h1))/2),
             h1,
             rep(0,(L-length(h1))/2))
    }
    if(type == "bandpass"){
      # change the band-reject filter kernel into a band-pass 
      h = -h1 - h2
    }else{
      h = h1 + h2
    }
    h[(L+1)/2] = h[(L+1)/2] + 1
  }
  
  # L <- length(h)
  
  # Choose the next power of 2 greater than L+nr-1 
  Nfft = 2^(ceiling(log2(L + nr - 1)))
  # Zero pad the signal and impulse response:
  h_long = c( h, rep(0, Nfft - L) )
  A = rbind(A , matrix(0, nrow = Nfft - nr, ncol = ncol(A)) )
  
  fft_A = stats::mvfft(A)    # signal
  fft_h = stats::fft(h_long)        # filter
  
  # Now we perform cyclic convolution in the time domain using 
  # pointwise multiplication in the frequency domain:
  Y = fft_A * fft_h
  
  pow_A = Mod(fft_A)
  pow_h = Mod(fft_h)
  pow_y = Mod(Y)
  # si matrix -> moyenne sur les colonnes
  if(!is.null(dim(A))){
    pow_A = apply(pow_A, 1, mean, na.rm=TRUE)
    pow_y = apply(pow_y, 1, mean, na.rm=TRUE)
  }
  # select only first half of vectors
  pow_A = pow_A[1:(Nfft/2+1)] 
  pow_y = pow_y[1:(Nfft/2+1)] 
  pow_h = pow_h[1:(Nfft/2+1)] 
  
  fre = Fs*(0:(Nfft/2))/Nfft/fac  #[MHz]
  
  if(plotSpec == TRUE){
    op <- par(no.readonly=TRUE)
    # plot the power spectrum
    m = seq(0,900,by=50)
    #par(mfrow=c(2,1), mar=c(5, 4, 4, 6) + 0.1 )
    par( mar=c(0, 4, 0.3, 2) + 0.1, oma=c(3,2,1,2) )
    plot(fre, pow_A, type="l",
         ylim=c(0,max(pow_A,pow_y)),
         ylab="power",lwd=2)
    lines(fre,pow_y, type="l",col="blue",lwd=2)
    par(new=TRUE)
    plot(fre, pow_h, type = "l", col = "red", yaxt = "n", ylab = "")
    legend("topright", c("input signal", "filter", "filtered signal"),
           col = c("black", "red", "blue"), lwd = c(2, 1, 2), bg = "white")
    abline(v = f/1000000, col = "grey", lty = 2)
    par(op)
  }
  a = (L-1)/2
  y = stats::mvfft(Y, inverse = TRUE)
  y = y[a:(a+nr-1), ,drop = FALSE]/nrow(y)
  return(Re(y))
}

winSincKernel <- function(L, f, type = c("low", "high")){
  type <- match.arg(type)  
  # if L is even (because L - filter length - must be odd)
  x = (-(L-1)/2):((L-1)/2)
  # low-pass
  h = hammingWindow(L) * sincMod(x, 2 * pi * f)  # h is our filter
  h = h/sum(h)
  # high-pass
  if(type == "high"){
    h = -h
    h[(L+1)/2] = h[(L+1)/2] + 1
  }
  return(h)
}

sincMod <- function(x, ff){
  r = length(x)
  n0 = which(x == 0)
  v = rep(0,r)
  ww <- c(1:(n0 - 1), (n0 + 1):r)
  #x = x[c(1:(n0-1),(n0+1):r)]
  v[ww] = sin(ff*x[ww])/(x[ww])
  v[n0] = ff
  return(v)
}
hammingWindow <- function(L){
  N = L-1
  n <- 0:N
  return(0.54 - 0.46*cos(2*pi*n/N))
}

# Choose the next power of 2 greater than L+M-1 
# Nfft = 2^(ceiling(log2(L+M-1)))  # -1)))    # or 2^nextpow2(L+M-1)
nextpower2 <- function(x){
  return(2^(ceiling(log2(x))))
}
