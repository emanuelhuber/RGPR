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
    Wmin <- matrix(nrow = nf, ncol = ncol(X))
    for(i in 1:ncol(X)){
      ww <- (i-wtr):(i+wtr)
      ww <- ww[ww <= ncol(X) & ww >= 1]
      supertrace <- as.vector(X[W, ww])
      # inverse minimum-phase wavelet estimation # variante 1 (Emanuel)
      Fmin[,i] <- .spikingFilter(supertrace, nf = nf , mu = mu, shft = shft)
      # Wmin[,i] <- deconv(c(1,rep(0,nf-1)),Fmin[,i], nf=nf,mu=mu)
      Wmin[,i] <- deconvolve(c(1,rep(0, nf-1)), Fmin[,i], mu = mu)
      # minimum-phase deconvolued data
      Xdec[,i] <- convolution(X[,i], Fmin[,i])[1:nrow(X)]
    }
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
    x <- traceScaling(x, type="rms")
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
# spectral deconvolution with known wavelet
# convolution model: y = h*x 
# h and y are known, x is unknown
# x ~ H^h * Y / (H^h * H + mu)
deconvolve <- function(y, h, mu = 0.0001){
  ny <- length(y)
  nh <- length(h)
  L  <- ny + ny
  H  <- stats::fft(c(h, rep(0, ny)))
  Y  <- stats::fft(c(y, rep(0, nh)))
  Re(stats::fft( t(Conj(H))*Y/(t(Conj(H))*H + mu) ,inverse=TRUE))[1:ny]/L
  # Re(fft( Y/(H + mu) ,inverse=TRUE))[1:ny]/L
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

