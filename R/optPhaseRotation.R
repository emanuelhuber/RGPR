# setGenericVerif("rmsScaling", function(x) standardGeneric("rmsScaling"))
#' Optimum Phase Rotation
#'
#' @param x any data that can be converted into a numeric vector with 
#'          as.vector.
#' @param rot The phase rotation increment.
#' @param plot A lenth-one boolean vector. If TRUE, the kurtosis as a function
#'             of phase angle is plotet.
#' @name optPhaseRotation
#' @rdname optPhaseRotation
#' @export
optPhaseRotation <- function(x, rot = 0.01, plot = TRUE){
  # x_dec <- as.vector(gpr/apply(as.matrix(gpr),2,RMS))
  x_dec <- as.vector(x)
  pi_seq <- seq(0, pi, by = rot)
  kurt <- numeric(length(pi_seq))
  nx <- length(x_dec)
  for(i in seq_along(pi_seq)){
    xrot <- phaseRotation(x_dec, pi_seq[i])
    # xrot_scaled2 <- (xrot -   mean(xrot))^2
    # kurt[i] <- ((1/nx) * sum( xrot_scaled2^2)) / 
    # ( (1/nx) *sum( xrot_scaled2))^2 
    kurt[i] <- e1071::kurtosis( xrot)
  }
  phi_max <- pi_seq[which.max(kurt)]
  message("rotation angle = ", phi_max/pi*180, " degree")
  # dev.off(); windows()
  if(plot==TRUE){
    plot(pi_seq/pi*180,kurt,type="l")
    abline(v=phi_max/pi*180,col="red")
  }
  return(phi_max)
  # x_dec <- phaseRotation(x_dec, phi_max)
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
.spikingFilter <- function(y, nf = 32, mu = 0.1, shft = 1){
  # R = t(Y)%*%Y = Toepliz matrix of ACF
  y_acf <- as.numeric(acf(y, lag.max = nf - 1, plot= FALSE)[[1]])
  taper <- hammingWindow(2*nf)
  y_acf <- y_acf*taper[(nf+1):(2*nf)] 
  y_acf[1] <- y_acf[1] + mu
  YtY <- toeplitz(y_acf)
  # all the spiking filters
  if(is.null(shft)){
    ny <- length(y)
    L <- nf + ny -1
    # convolution matrix Y
    Y <- convmtx(y,nf)
    H <- solve(YtY) %*% t(Y) 
    v <- numeric(L)
    # performance matrix: all spiking filter outputs
    P <- Y %*% H
    # optimal delay (smallest error)
    i <- which.max(diag(P))
    v[i] <- 1
    h <- H%*%v
    return(list("h"=h,"delay"=i))
  }else{
    v <- numeric(nf)
    v[shft] <- 1
    h <- solve(YtY) %*% v 
    return(h)
  }
}

