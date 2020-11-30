

# crewes/seismic/deconw.m -> Wiener deconvolution (time domain)
#                  y_dec <- conv(fmin, y)
# crewes/seismic/deconf.m -> frequency domain spiking deconvolution

# Schmelzbach
# f_min
# w_min <- real(ifft(1. ./(fft(f_min'))));
# w_min = cs_taper(w_min,'cos' ,TaperLength,'bottom');


#-------------
# ressources/existing_codes/Processing of Seismic Reflection Data Using MATLAB/Book_codes/Codes/spiking_decon.m
# function Ds=spiking_decon(Data,max_lag,mu,dt)
# % This code is written to perform spiking deconvolution for seismic traces.
# % It uses the auto-correlation generated based on the whole record time.
# %
# % The required inputs are:
#   % Data: seismic shot gather(s)
# % max_lag: maximum lag value(Note that the min lag is set to be the first sample)
# % mu: White noise percentage
# % dt: time sampling interval
# % 
# % The output is:
#   % Ds: the deconvolved seismic data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# % The code if for the book titled: Processing Surface Seismic Reflection
# % Data Using MATLAB by Wail A. Mousa & Abdullatif A. Al-Shuhail
# % September 2011.
# 
# N=round(max_lag/dt);
# p_noise=mu/100;
# [nt,nx]=size(Data);
# Dauto=zeros(N,nx);
# Ds=zeros(nt+N-1,nx);
# Rxd=zeros(1,N);
# 
# for i=1:nx
# Dauto(:,i)=my_xcorr(Data(:,i)',N)'; 
#                     Rxd=Rxd+my_xcorr(Data(:,i)',N,1);
# end
# Rxd=abs(Rxd);
# 
# DDauto=sum(Dauto,2);
# Rxx=toeplitz(DDauto);
# Rxx(1,1)=Rxx(1,1)*(p_noise);
# 
# 
# h_opt=(inv(Rxx+eps))*Rxd';
#                                      for i=1:nx
#                                      Ds(:,i)=conv(Data(:,i),h_opt);
#                                      end
#                                      
#                                      Ds=Ds(1:nt,:);
                                     
                                     
setGenericVerif("deconv", function(x, method=c("spiking", "wavelet",
                                               "min-phase", "mixed-phase"), 
                                   ...,
                                   track = TRUE) standardGeneric("deconv"))


#' Deconvolution
#'
#' A generic function to perform different types of convolution
#' 
#' @section Spiking and mixed-phase deconvolution:
#' The required arguments for \code{method = "spiking"} and 
#' \code{method = "mixed-phase"} are:
#' \itemize{
#'   \item \code{W}: A length-two numeric vector defining the time/depth window
#'                   for which the wavelet is estimated
#'   \item \code{wtr}: A length-one numeric vector defining the number of 
#'                     neighorough traces to be combine into a "super trace"
#'                     (the total number of traces is \code{2*wtr + 1}).
#'   \item \code{nf}: A length-one numeric vector defining the filter length.
#'   \item \code{mu}: A length-one numeric vector defining the amount of noise.
#' }
#' @section Wavelet deconvolution:
#' The required arguments for \code{method = "wavelet"} are:
#' \itemize{
#'   \item \code{h}: A numeric vector corresponding to the wavelet used to
#'                   deconvolve the GPR data.
#'   \item \code{mu}: A length-one numeric vector defining the amount of noise.
#' }
#'
#' @param method Type of deconvolution method.
#' @param ... additional arguments, see \code{details}.
#' @return A list containing the deconvolued GPR data (and possibly other
#' variables.
#' @name deconv
#' @rdname deconv
#' @export
setMethod("deconv", "GPR", function(x, 
                                    method=c("spiking", "wavelet", 
                                             "min-phase", "mixed-phase"),
                                    ...,
                                    track = TRUE){
  method <- match.arg(method, c("spiking", "wavelet", "min-phase",
                                "mixed-phase"))
  toReturn <- list()
  dots <- list(...)
  if(method == "spiking" || method == "mixed-phase"){
    if(length(dots) == 0){
      stop(paste0("spiking deconvolution requires the following arguments:",
                  "W, wtr, nf, mu\n"))
    }
    # deconvSpiking <- function(x,W,wtr,nf,mu){
    if(is.null(dots$W)){
      stop(paste0("W must be defined\n"))
    }else{
      if(length(dots$W)!= 2) stop("'W' must have equal to 2")
      W <- sort(dots$W)
    }
    if(is.null(dots$wtr)){
      stop(paste0("wtr must be defined\n"))
    }else{
      wtr <- dots$wtr
    }
    if(is.null(dots$nf)){
      stop(paste0("nf must be defined\n"))
    }else{
      nf <- dots$nf
    }
    if(is.null(dots$mu)){
      stop(paste0("mu must be defined\n"))
    }else{
      mu <- dots$mu
    }
    if(is.null(dots$shft)){
      shft <- 1
    }else{
      shft <- dots$shft
    }
    W <- which(depth(x) > W[1] & depth(x) < W[2])
    # W <- seq(W[1], W[2])
    if(nf < length(W)){
      message("'nf' is longer that the window length and therefore\n",
              "I set 'nf' as long as the window length")
      nf <- length(W)
    }
    X <- traceScaling(x, type = "rms")@data
    # X <- X / apply(as.matrix(X),2,RMS)
    Xdec <- matrix(nrow = nrow(X), ncol = ncol(X))
    Fmin <- matrix(nrow = nf, ncol = ncol(X))
    # Wmin <- matrix(nrow = nf, ncol = ncol(X))
    for(i in 1:ncol(X)){
      ww <- (i-wtr):(i+wtr)
      ww <- ww[ww <= ncol(X) & ww >= 1]
      supertrace <- as.vector(X[W, ww])
      # inverse minimum-phase wavelet estimation # variante 1 (Emanuel)
      # Fmin[,i] <- .spikingFilter(supertrace, nf = nf , mu = mu, shft = shft)
      Fmin[,i] <- deconvolveSpikingInvFilter(supertrace, n = nf, i = shft, mu = mu, taperType = "hamming")$fmin
      # Wmin[,i] <- deconv(c(1,rep(0,nf-1)),Fmin[,i], nf=nf,mu=mu)
      # Wmin[,i] <- deconvolve(c(1,rep(0, nf-1)), Fmin[,i], mu = mu)
      # minimum-phase deconvolued data
      Xdec[,i] <- convolution(X[,i], Fmin[,i])[1:nrow(X)]
    }
    Wmin <- Re(mvfft( 1 / mvfft(Fmin), inverse = TRUE))/nrow(Fmin)
    # estimated min-phase wavelet
    w_0 <- matrix(0, nrow=round(nf/3),ncol=ncol(Wmin))
    w_min <- list(x = seq(-round(nf/3),to =  nf , by = 1)*x@dz,
                  y = rbind(w_0, Wmin, rep(0, ncol(Wmin))))
    x@data <- Xdec
    toReturn <- list("fmin" = Fmin, "wmin" = w_min)
  }else if(method == "wavelet"){
    if(length(dots) == 0 || (is.null(dots[["h"]]) || is.null(dots[["mu"]]))){
      stop(paste0("wavelet deconvolution requires the following arguments:",
                  "h and mu\n"))
    }
    # x <- traceScaling(x, type="rms")
    x@data <- apply(x@data, 2, deconvolve, dots[["h"]], dots[["mu"]])      
  }else if(method== "min-phase"){
    stop("min-phase deconvolution has to be first written!!!\n")
  }
  if(method == "mixed-phase"){
    # optimal phase shift
    phi <- optPhaseRotation(x@data[W,], rot = 0.05, plot=TRUE)
    # phase rotation
    x@data <-  apply(x@data, 2, phaseRotation, phi)
    # mixed phase wavelet
    w_mix <- w_min
    w_mix$y <- apply(w_min$y, 2 , phaseRotation, - phi)
    toReturn[["optRot"]] <- phi
    toReturn[["wmix"]] <- w_mix
  }
  if(isTRUE(track)) proc(x) <- getArgs()
  #     x@proc <- c(x@proc, proc)
  toReturn[["x"]] <- x
  return(toReturn)
}
)



#---------------- DECONVOLUTION --------------------#
# x ~ H^h * Y / (H^h * H + mu)
# deconvolve <- function(y, h, mu = 0.0001){
#   ny <- length(y)
#   nh <- length(h)
#   L  <- ny + ny
#   H  <- stats::fft(c(h, rep(0, ny)))
#   Y  <- stats::fft(c(y, rep(0, nh)))
#   Re(stats::fft( t(Conj(H))*Y/(t(Conj(H))*H + mu) ,inverse=TRUE))[1:ny]/L
#   # Re(fft( Y/(H + mu) ,inverse=TRUE))[1:ny]/L
# }

#' Deconvolution with known wavelet
#' 
#' convolution model: \eqn{y = h \times x} where h and y are known, 
#' x is unknown. Spectral or matrix-based.
#' @return Vector with same length as \code{y} 
#' @export
deconvolve <- function(y, h, mu = 0.0001, type = c("matrix", "FFT"),
                       regDeriv = FALSE){
  type <- match.arg(type[1], c("FFT", "matrix"))
  if(type == "FFT"){
    deconvolveFFT(y = y, h = h, mu = mu)
  }else if(type == "matrix"){
    if(is.null(dim(y)) && is.null(dim(h))){
      deconvolveMtx(y = y, h = h, lambda = mu, regDeriv = regDeriv)
    }else{
      if(is.null(dim(y))){
        dim(y) <- c(length(y), 1)
      }
      if(is.null(dim(h))){
        dim(h) <- c(length(h), 1)
      }
      if(ncol(h) < ncol(y)){
        # FIXME -> mabye w <- w[, 1] would be enough
        h <- repmat(h[, 1, drop = FALSE], 1, ncol(y)) 
      }
      if(nrow(h) > nrow(y)){
        h <- h[1:nrow(y), ]
        warning("'h' is longer than '...' and is therefore shortened.")
      }
      y_dec <- y
      for(i in 1:ncol(y)){
        y_dec[, i] <- deconvolveMtx(y = y[, i], h = h[, i], lambda = mu, regDeriv = regDeriv)
      }
      return(y_dec)
    }
  }
}



# Wiener deconvolution in the frequency domain

# convolution model: \eqn{y = h \times x} where h and y are known,
# x is unknown
# @return Vector with same length as \code{y}
# @export
# Checked: padding helps!!!

deconvolveFFT <- function(y, h, mu = 0.0001){
  if(is.null(dim(y))){
    dim(y) <- c(length(y), 1)
  }
  if(is.null(dim(h))){
    dim(h) <- c(length(h), 1)
  }
  if(ncol(h) < ncol(y)){
    # FIXME -> mabye w <- w[, 1] would be enough
    h <- repmat(h[, 1, drop = FALSE], 1, ncol(y)) 
  }
  if(nrow(h) > nrow(y)){
    h <- h[1:nrow(y), ]
    warning("'h' is longer than '...' and is therefore shortened.")
  }
  L  <- 2*(nrow(y) + nrow(h) ) + 1
  
  y0 <- paddMatrix(y, nrow(h), 0, zero = TRUE)
  h0 <- matrix(0, nrow = nrow(y0), ncol = ncol(y0))
  h0[1:nrow(h), ] <- h
  H  <- stats::mvfft( h0)
  Y  <- stats::mvfft( y0)
  
  y_dec <- Re(stats::mvfft( Y * Conj((H))/ (Mod(H)^2 + abs(mu)), inverse = TRUE))
  return(y_dec[nrow(h) + 1:nrow(y), ]/nrow(y0))
}

# deconvolveFFT <- function(y, h, mu = 0.0001){
#   ny <- length(y)
#   nh <- length(h)
#   if(nh > ny){
#     h <- h[1:ny]
#     nh <- ny
#     warning("'h' is longer than '...' and is therefore shortened.")
#   }
#   L  <- 2*(ny + nh ) + 1
#   # simply extend the response function h and the signal y to length L 
#   # by padding it with zeros
#   h0 <- numeric(L)
#   y0 <- h0
#   h0[1:nh] <- h
#   y0[1:ny] <- y
#   H  <- stats::fft( h0)
#   Y  <- stats::fft( y0)
#   # Note: Mod(H)^2 = Conj(tH) * H
#   y_dec <- Re(stats::fft( Y * Conj(H)/ (Mod(H)^2 + abs(mu)), inverse = TRUE))
#   return(y_dec[1:ny]/L)
#   # Re(fft( Y/(H + mu) ,inverse=TRUE))[1:ny]/L
# }

# deconvolveFFT <- function(y, h, mu = 0.0001){
#   ny <- length(y)
#   nh <- length(h)
#   # L  <- ny + ny
#   if(nh > ny){
#     h <- h[1:ny]
#     warning("'h' is longer than 'y' and is therefore shortened.")
#   }
#   h0 <- numeric(ny)
#   h0[1:nh] <- h
#   # H  <- stats::fft(c(h, rep(0, ny)))
#   # Y  <- stats::fft(c(y, rep(0, nh)))
#   H  <- stats::fft(h0)
#   Y  <- stats::fft(y)
#   y_dec <- Re(stats::fft( Y * Conj(H)/ (H * Conj(H) + abs(mu)), inverse = TRUE))/(ny )
#   return(y_dec)
#   # Re(fft( Y/(H + mu) ,inverse=TRUE))[1:ny]/L
# }

# Matrix based deconvolution
# 
# same as \code{deconvolveF} but matrix based (same results)
# @return Vector with same length as \code{y} 
# @export
# Checked: no need for padding
deconvolveMtx <- function(y, h, lambda, regDeriv = FALSE){
  ny <- length(y)
  nh <- length(h)
  if(nh > ny){
    h <- h[1:ny]
    warning("'h' is longer than 'y' and is therefore shortened.")
  }
  W <- convmtx(w = h, n = ny)
  if(isTRUE(regDeriv)){
    # See Least Squares with Examples in Signal Processing
    # Ivan Selesnic, kMarch 7, 2013, NYU-Poly
    # To improve the deconvolution result in the presence of noise, we can 
    # minimize the energy of the derivative (or second-order derivative) of 
    # y instead
    D <- derivativeMtx2d(ny)
    solve(t(W) %*% W + lambda * t(D) %*% D, t(W) %*% y)
  }else{
    solve(t(W) %*% W + lambda * diag(ny), t(W) %*% y)
  }
}

#' @export
deconvolveSpikingInvFilter <- function(x, n = 35, i = 1, mu = 0.001, taperType = "hamming"){ #, returnDelay = FALSE ){
  if(is.null(dim(x))){
    dim(x) <- c(length(x),1)
  }
  xdeconv <- apply(x, 2, .deconvolveSpikingInvFilter, n = n, i = i, mu = mu, taperType = taperType) #, returnDelay = returnDelay)
  # if(isTRUE(returnDelay)){
  #   return(xdeconv[[1]])
  # }
  return(xdeconv[[1]])
}



.deconvolveSpikingInvFilter <- function(x, n = 35, i = 1, mu = 0.001, taperType = "hamming"){ #, returnDelay = FALSE){
  x_acf <- as.numeric(acf(x, lag.max = n - 1, plot= FALSE)[[1]])
  n <- length(x_acf)  # because length(x_acf) can be < lag.max
  x_acf <- x_acf * taper(n, type = taperType, half = TRUE, reverse = TRUE, a = 0.1) # tapering
  # print(x_acf)
  x_acf <- x_acf/x_acf[1]
  x_acf[1] <- x_acf[1] + mu^2
  XtX <- toeplitz(x_acf)
  v <- numeric(n)
  if(is.null(i)){
    X <- convmtx(x, n)
    Fmin <- chol2inv(chol(XtX)) %*% t(X) 
    # v <- numeric(L)
    # performance matrix: all spiking filter outputs
    P <- X %*% Fmin
    # optimal delay (smallest error)
    i <- which.max(diag(P))[1]
    v[i] <- 1
    fmin <- Fmin %*% v
    print(i)
    # if(isTRUE(returnDelay)){
    # }else{
    #   return(fmin)
    # }
  }else{
    v[i] <- 1
    fmin <- chol2inv(chol(XtX)) %*% v 
    # return(fmin)
  }
  return(list("fmin" = fmin, "delay" = i))
}

# y is the wavelet and we want 
# a filter f s.t. f*y = d 
# with d = [0 ... 0 1 0 ...0]
# 1 at the postion i = shft
# if shft = NULL, the shift is chosen by the
#     algorithm and also returned
# if shift is not NULL, case of wavelet estimation
#     from the trace via the autocorrelation matrix.
# mu = percent of pre-whitening
deconvolveSpikes <- function(y, nf = 32, mu = 0.1, shft = 1){
  # R = t(Y)%*%Y = Toepliz matrix of ACF
  y_acf <- as.numeric(acf(y, lag.max = nf, plot= FALSE)[[1]])
  nf <- length(y_acf)  # because length(y_acf) can be < lag.max
  taper <- hammingWindow(2*nf)
  y_acf <- y_acf*taper[(nf+1):(2*nf)] 
  y_acf[1] <- y_acf[1] + mu
  y_acf <- y_acf/y_acf[1] 
  YtY <- toeplitz(y_acf)
  # all the spiking filters
  if(is.null(shft)){
    ny <- length(y)
    L <- nf + ny -1
    # convolution matrix Y
    Y <- convmtx(y, nf)
    # H <- solve(YtY) %*% t(Y) 
    H <- chol2inv(chol(YtY)) %*% t(Y) 
    v <- numeric(L)
    # performance matrix: all spiking filter outputs
    P <- Y %*% H
    # optimal delay (smallest error)
    i <- which.max(diag(P))
    v[i] <- 1
    h <- H %*% v
    return(list("h"=h,"delay"=i))
  }else{
    v <- numeric(nf)
    v[shft] <- 1
    # h <- solve(YtY) %*% v 
    h <- chol2inv(chol(YtY)) %*% v 
    return(h)
  }
}



#--- l1 constrained
# sparse deeoncolution
# %  Reference: Penalty and Threshold Functions for Sparse Signal Processing
# % Ivan Selesnick, NYU-Poly, selesi@poly.edu, 2012
# %
# % http://eeweb.poly.edu/iselesni/lecture_notes/

#' Sparse deconvolution
#'
#' Reference: Penalty and Threshold Functions for Sparse Signal Processing
#' Ivan Selesnick, NYU-Poly, selesi@poly.edu, 2012
#' http://eeweb.poly.edu/iselesni/lecture_notes/
#' @export
deconvolveSparse <- function(y, w, Nit = 50, eps = 0.01, sigma = 0.1,
                             thr = NULL, phifun, wfun, ...){
  if(is.null(thr))  thr <- 3 * sigma * sqrt(sum(abs(w)^2))   
  
  if(missing(phifun)){
    phifun  <- function(x){
      thr * abs(x)
    }
  }
  if(missing(wfun)){
    wfun <- function(x) abs(x) / thr
  }
  y    <- as.vector(y)                    # convert to column vector
  W <- convmtx(w, length(y))
  cost <- rep(0, Nit)                    # cost function history
  M_N  <- dim(W)
  x    <- y                               # initialization
  g    <- t(W) %*% y                      # W'*y
  for(k in seq_len(Nit)){
    XLam <- diag(wfun(as.vector(x), ...), M_N[2], M_N[2])   # Lam : diagonal matrix    
    FF  <- diag(1, M_N[1], M_N[1]) + W %*% XLam %*% t(W)    # F : banded matrix    
    # cost function value
    # update x (solve banded system)
    x   <- XLam %*% (g - (t(W) %*% solve(FF, (W %*% (XLam %*% g)))))            
    cost[k] <- 0.5 * sum(abs(y - W %*% x)^2) + sum(phifun(x, ...)) 
    if(!is.null(eps) && k > 1 && (cost[k-1] - cost[k])/cost[1] < eps) break
  }
  return(list(x = x, cost = cost[1:k]))
}



# # TO CHECK!!!!
# # deconvolution with known wavelet
# # convolution model: y = h*x 
# # h and y are known, x is unknown
# # x ~ H^h * Y / (H^h * H + mu)
# deconvolutionMtx <- function(y, h, nf, mu = 0.0001){
#   # ny <- length(y)
#   # nh <- length(h)
#   # L  <- ny + ny - 1
#   H  <- convmtx(h,nf)
#   y_acf <- as.numeric(acf(y, lag.max = nf - 1, plot = FALSE)[[1]])
#   y_acf[1] <- y_acf[1] + mu
#   HtH <- toeplitz(y_acf)
#   x <-  solve(HtH) %*% (t(H[1:nf,1:nf]) %*% y)
#   return(x)
# }

