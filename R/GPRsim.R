
# GPR wavelet:
#   Check chap 11 of Anan in the book Near-Surface Geophysics
# "The radiated wavelet from a GPR system is a compli-
# cated function of the antenna construction and the electronics 
# drive circuitry.For impulse style ultra wideband systems using 
# short electric dipole antennas, a simple mathematical model is 
# helpful for numerical simulation."

vt <- function(xt, xT){
  test <- xt > 0 & xt < xT
  vt <- numeric(length(xt))
  vt[test] <-  0.5 * ( 1 + cos(pi * (xt[test]- xT/2)/(xT/2)))
  return(vt)
}

# xt = time in ns
# where 0 < q < 1 (damping factor)
# fc = center frequency in MHz

#' Simulate GPR wavelet
#' 
#' GPR wavelet: Check chap 11 of Annan in the book Near-Surface Geophysics
#' "The radiated wavelet from a GPR system is a complicated function of the 
#' antenna construction and the electronics drive circuitry. For impulse 
#' style ultra wideband systems using short electric dipole antennas, 
#' a simple mathematical model is helpful for numerical simulation."
#' @export
wavGPR <- function(xt, q, fc){
  xt <- xt / 1000
  xT <- (2/3 + (1-q)/7)/fc
  vt(xt, xT) - (2 - q)* vt(xt - xT/2, xT) + (1 - q)*vt(xt - xT, xT)
}



#' Simulate Hat wave
#' 
#' Simulate Hat wave
#' @export 
wavHat <- function(x, sig){
  (2/((pi)^(1/4)*sqrt(3*sig)))*(1- (x^2)/(sig^2))*exp( - (x^2)/(2*sig^2))
}
