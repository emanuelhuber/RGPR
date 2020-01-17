
#' @name CMPAnalysis
#' @rdname CMPAnalysis-methods
#' @exportMethod CMPAnalysis
setGenericVerif("CMPAnalysis", function(x, method = c("semblance", 
                                                      "winsemblance", "wincoherence", "wincoherence2"), v = NULL, 
                                        w = NULL) standardGeneric("CMPAnalysis"))





winsemblance <- function(x){
  S <- sum(rowSums(x, na.rm = TRUE)^2) / 
    sum(rowSums(x^2, na.rm = TRUE)) * nrow(x)
  #S <- sum((apply(x, 1, sum, na.rm = TRUE))^2) /
  #  sum(apply((x)^2, 1, sum, na.rm = TRUE)) * nrow(x)
  return(S)
}

semblance <- function(x){
  S <- rowSums(x, na.rm = TRUE)^2 / rowSums(x^2, na.rm = TRUE)
  #x_velAna@data[,i] <- (apply(y@data, 1, sum, na.rm = TRUE))^2 / 
  #                    apply((y@data)^2, 1, sum, na.rm = TRUE)
  return(S)
}


signalNoiseRatio <- function(x){
  ysvd <- svd(x)
  n <- length(ysvd$d)
  # estimator of the noise variance
  x_sig2 <- sum(ysvd$d[-1])/(n-1)
  # estimator of the signal energy
  P <- (ysvd$d[1] - x_sig2)/n
  return( P/x_sig2 )
}

signalNoiseRatio2 <- function(x){
  ysvd <- svd(x)
  m <- nrow(x)
  n <- length(ysvd$d)
  W <- m * log( 0 + (sum(ysvd$d)/n)^n / prod(ysvd$d) )^n
  # estimator of the noise variance
  x_sig2 <- sum(tail(ysvd$d, n-1))/(n-1)
  # estimator of the signal energy
  P <- (ysvd$d[1] - x_sig2)/n
  return( W * P/x_sig2 )
}


#' Velocity Analysis of CMP Gather
#' 
#' Transform the space-time domain of the radargram into a velocity-time domain to obtain 
#' the velocity spectrum (i.e. change in wave velocity with depth or time). This is achieved by applying
#' Normal Move-Out (NMO) corrections to the radargram for the range of selected velocities 
#' and computing a coherency measure for each result. In RGPR, the coherency measure can be defined using 
#' different functions: "semblance", "winsemblance", "wincoherence", "wincoherence2".   
#' 
#' either use 'rec' and 'trans' to compute the distance between the antennas
#' or give the distance between the antennas (asep)
#' or seq(x@antsep, by = x@dx, length.out = length(x))
#'
#' \describe{
#'   \item{semblance}{also described as the ratio of input to output
#'         energy (Niedell and Taner, 1971)}
#'   \item{winsemblance}{windowed semblance}       
#'   \item{wincoherence}{Windowed coherence measure based on 
#'         eigen-decomposition that estimates the 
#'         signal-to-noise ratio for high resolution velocity analysis
#'         (Sacchi, 2002)}
#'   \item{wincoherence2}{Windowed coherence measure based on a log-generalized
#'         likelihood ratio which tests the hypothesis of equality of 
#'         eigenvalues (Key and Smithson, 1990)}
#' }
#' @param x An object of the class \code{GPR}
#' @param method A length-one character vector 
#' @param v A numeric vector defining at which velocities the analysis is
#'          performed
#' @param w A length-one numeric vector defining the window length for the
#'          methods 'wincoherence' and 'wincoherence2'.           
#' @rdname CMPAnalysis-methods
#' @aliases CMPAnalysis,GPR-method
#' @references
#' \itemize{
#'   \item{Neidell and Taner (1971) Semblance and other coherency measures
#'         for multichannel data. 
#'         Geophysics, 36(3):482-497.}
#'   \item{Key and Smithson (1990) New approach to seismic-reflection
#'         event detection and velocity determination. 
#'         Geophysics, 55(8):1057-1069.}
#'   \item{Textbook: Sacchi (2002) Statistical and Transform Methods
#'         in Geophysical Signal Processing}
#' }
#' @export
setMethod("CMPAnalysis", "GPR", 
          function(x, method = c("semblance","winsemblance",   
                                 "wincoherence", "wincoherence2"), 
                   v = NULL, w = NULL){
            method <- match.arg(method, c("semblance", "winsemblance", 
                                          "wincoherence", "wincoherence2"))
            if(is.null(v)){
              vlim <- x@vel[[1]] * c(0.5, 1.5)
              v <- seq(vlim[1], vlim[2], length = 50)
            }
            if(any(x@time0 != 0)){
              x <- time0Cor(x, method = "pchip")
            }
            #FIXME: create a new object > check slot consistency
            # as(matrix(0, nrow = nrow(x), ncol = length(v)), "GPR")
            x_velAna <- .NMOCor(x, v = max(v), asep = x@antsep)
            x_velAna <- x_velAna[, rep(1, length(v))]
            x_velAna@data[] <- 0
            # x_velAna <- matrix(0 nrow=nrow(test), ncol=length(vv))
            x_velAna@pos <- v
            if(method %in% c("wincoherence", "wincoherence2", "winsemblance")){
              if(is.null(w)){
                w <- ceiling(nrow(x))/20
              }
              wi <- round(w/x@dz)
              if(wi > nrow(x) || wi < 0 ) stop("w too large or too small")
              vabove <- seq_len(ceiling(w/2)) + 1
              # FIXME > use apply(x, i, FUN)
              if(method == "wincoherence"){
                for(i in seq_along(v)){
                  y <- .NMOCor(x, v = v[i], asep = x@antsep)
                  x_velAna@data[,i] <- wapplyRowC(y@data, width = wi, by = 1, 
                                                  FUN = signalNoiseRatio)
                  # x_velAna@data[floor(w/2) + seq_along(test),i] <- test
                }
              }else if(method == "wincoherence2"){
                for(i in seq_along(v)){
                  y <- .NMOCor(x, v = v[i], asep = x@antsep)
                  x_velAna@data[,i] <- wapplyRowC(y@data, width = wi, by = 1, 
                                                  FUN = signalNoiseRatio2)
                  # x_velAna@data[floor(w/2) + seq_along(test),i] <- test
                }
              }else if(method == "winsemblance"){
                for(i in seq_along(v)){
                  y <- .NMOCor(x, v = v[i], asep = x@antsep)
                  x_velAna@data[,i] <- wapplyRowC(y@data, width = wi, by = 1, 
                                                  FUN = winsemblance)
                  # x_velAna@data[floor(w/2) + seq_along(test),i] <- test
                }
              }
              x_velAna@data[vabove,] <- 0
            }else if(method == "semblance"){
              for(i in seq_along(v)){
                y <- .NMOCor(x, v = v[i], asep = x@antsep)
                x_velAna@data[,i] <- semblance(y@data)
              }
            }
            x_velAna@data[is.na(x_velAna@data)] <- 0
            x_velAna@data[is.infinite(x_velAna@data)] <- 0
            x_velAna@surveymode <- "CMPANALYSIS"
            x_velAna@antsep <- 0
            x_velAna@time0 <- 0
            proc(x_velAna) <- getArgs()
            return(x_velAna)
          }
)