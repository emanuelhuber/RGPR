# GPR wavelet:
#   Check chap 11 of Anan in the book Near-Surface Geophysics
# "The radiated wavelet from a GPR system is a compli-
# cated function of the antenna construction and the electronics
# drive circuitry.For impulse style ultra wideband systems using
# short electric dipole antennas, a simple mathematical model is
# helpful for numerical simulation."

# twt = time in ns
# where 0 < q < 1 (damping factor)
# fc = center frequency in MHz

#' Simulates a GPR wavelet
#' 
#' Simulates a GPR wavelet with specified center frequency. 
#' 
#' @param twt [\code{numeric(n)}] Two-way travel time (generally positive if 
#'            \code{lag = 0})
#' @param type [\code{character(1)}] The Annan wavelet 
#'              (\code{type = "annan"}) with dampling factor or the classical
#'              Ricker wavelet (\code{type = "ricker"})
#' @param fc [\code{numeric(1)}] Center frequency in MHz
#' @param lag [\code{numeric(1)}] Used to shift the wavelet in time. For the
#'            Ricker wavelet, \code{lag = -3e3/(pi * fc)} centers the wavelet
#'            at \code{twt = 0}.
#' @param q [\code{numeric(1)}] Damping factor, where 0 < q < 1
#'                              (only for \code{type = "annan"})
#' @return [\code{numeric(n)}] The wavelet
#' @export
#' @examples 
#' fc <- 100 # MHz 
#'
#' xt <- seq(-50, by = 0.1, to = 50) # ns 
#'
#' plot(xt, simWavelet(xt, fc = fc, q = 0.9), type = "l")
#' plot(xt, simWavelet(xt, fc = fc, q = 0.9, lag = 5), type = "l")
#'
#' plot(xt, simWavelet(xt, type = "ricker", fc = fc, q = 0.9), type = "l")
#' plot(xt, simWavelet(xt, type = "ricker", fc = fc, q = 0.9, lag = 5), type = "l")
#' plot(xt, simWavelet(xt, type = "ricker", fc = fc, q = 0.9, lag = -10), type = "l")
#' plot(xt, simWavelet(xt, type = "ricker", fc = fc, q = 0.9, lag = -3e3/(pi * fc)), type = "l")
#' abline(v = 0)
simWavelet <- function(twt, type = c("annan", "ricker"), fc = 100, lag = 0, 
                       q = 1){
  type <- match.arg(type, c("annan", "ricker"))
  if(type == "annan"){
  twt <- (twt - lag) / 1000
    xT <- (2/3 + (1-q)/7)/fc
    .vttt(twt, xT) - (2 - q) * .vttt(twt - xT/2, xT) + (1 - q) * 
      .vttt(twt - xT, xT)
  }else if(type == "ricker"){
    twt <- (twt - lag) / 10^9  # second
    fc <- fc * 10^6    # MHz
    twt <- twt - 3/(pi * fc)
    piftwt2 <- (pi * fc * twt)^2
    (1 - 2 * piftwt2 ) * exp(-piftwt2)
  }
}

.vttt <- function(twt, xT){
  test <- twt > 0 & twt < xT
  vt <- numeric(length(twt))
  vt[test] <-  0.5 * ( 1 + cos(pi * (twt[test]- xT/2)/(xT/2)))
  return(vt)
}
