#' Velocity spectrum (CMP Analysis)
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
#' @name velSpectrum
setGeneric("velSpectrum", 
           function(x, method = c("semblance", "winsemblance", "minsemblance",
                                  "wincoherence", "wincoherence2"), 
                    v = NULL, w = NULL) standardGeneric("velSpectrum"))


#' @rdname velSpectrum
#' @export
setMethod("velSpectrum", "GPR", 
          function(x, method = c("semblance","winsemblance", "minsemblance",  
                                 "wincoherence", "wincoherence2"), 
                   v = NULL, w = NULL){
            method <- match.arg(method, c("semblance", "winsemblance", "minsemblance",
                                          "wincoherence", "wincoherence2"))
            if(is.null(v)){
              # vlim <- x@vel[[1]] * c(0.5, 1.5)
              # vlim[vlim > 0.299] <- 0.299
              # v <- seq(vlim[1], vlim[2], length = 50)
              v <- exp(seq(log(0.02), log(0.3), length = 100))
            }
            if(any(x@time0 > 0)){
              stop(msg_do_shiftToTime0)
            }
            if(!isDepthTime(x)){
              stop(msg_set_zunitToDepth)
            }
            if(anyNA(x@antsep)){
              stop(msg_set_antsep)
            }
            if(length(x@antsep) != ncol(x)){
              stop("The length of the antenna separation distances must equal",
                   " to the number of columns of x. Use\n",
                   "'antsep(x) <- ...")
            }
            # as(matrix(0, nrow = nrow(x), ncol = length(v)), "GPR")
            x_tv <- x
            x_tv <- x_tv[, rep(1, length(v))]
            x_tv@data[] <- 0
            
            
            if(method %in% c("wincoherence", "wincoherence2", "winsemblance",
                             "minsemblance")){
              if(is.null(w)){
                w <- ceiling(nrow(x))/20
              }
              wi <- round(w/mean(diff(x@depth)))
              # FIXME: when wi = 0 -> not windowed!
              if(wi %% 2 == 0) wi <- wi + 1
              # print(wi)
              if(wi > nrow(x) || wi < 0 ) stop("w too large or too small")
              vabove <- seq_len(ceiling(w/2)) + 1
              
              if(method == "wincoherence"){
                for(i in seq_along(v)){
                  y <- .NMOCor(x, v = v[i], asep = x@antsep)
                  x_tv@data[,i] <- wapplyRowC(y@data, width = wi, by = 1, 
                                              FUN = signalNoiseRatio)
                }
                x_tv@data[vabove,] <- 0
              }else if(method == "wincoherence2"){
                for(i in seq_along(v)){
                  y <- .NMOCor(x, v = v[i], asep = x@antsep)
                  x_tv@data[,i] <- wapplyRowC(y@data, width = wi, by = 1, 
                                              FUN = signalNoiseRatio2)
                }
                x_tv@data[vabove,] <- 0
              }else if(method == "winsemblance"){
                for(i in seq_along(v)){
                  y <- .NMOCor(x, v = v[i], asep = x@antsep)
                  x_tv@data[,i] <- wapplyRowC(y@data, width = wi, by = 1, 
                                              FUN = winsemblance)
                }
              }else if(method == "minsemblance"){
                for(i in seq_along(v)){
                  y <- .NMOCor(x, v = v[i], asep = x@antsep)
                  SS <- semblance(y@data)/ncol(x)
                  x_tv@data[,i] <- wapplyC(SS, width = wi, by = 1, 
                                           FUN = min)
                  
                }
              }
            }else if(method == "semblance"){
              for(i in seq_along(v)){
                y <- .NMOCor(x, v = v[i], asep = x@antsep)
                x_tv@data[,i] <- semblance(y@data)/ncol(x)
              }
            }
            x_tv@data[is.na(x_tv@data)] <- 0
            x_tv@data[is.infinite(x_tv@data)] <- 0
            x_tv@surveymode <- "velSpectrum"
            x_tv@antsep <- 0
            x_tv@vel <- list()
            x_tv@pos <- v
            x_tv@posunit <- paste0(x_tv@posunit, "/", x_tv@depthunit)
            # x_tv@xlab <- "NMO velocity"
            x_tv@time0 <- numeric()
            if(method == "semblance"){
              x_tv@name <- "Semblance analysis"
            }else if(method == "winsemblance"){
              x_tv@name <- "Windowed semblance analysis"
            }else if(method == "minsemblance"){
              x_tv@name <- "Minimum semblance analysis"
            }else if(method == "wincoherence"){
              x_tv@name <- "Windowed coherence analysis"
            }else if(method == "wincoherence2"){
              x_tv@name <- "Windowed coherence analysis 2"
            }
            # x_tv@time0 <- 0
            proc(x_tv) <- getArgs()
            return(x_tv)
          }
)

winsemblance <- function(x){
  S <- sum(rowSums(x, na.rm = TRUE)^2) / 
    sum(rowSums(x^2, na.rm = TRUE)) / ncol(x)
  return(S)
}

semblance <- function(x){
  S <- rowSums(x, na.rm = TRUE)^2 / rowSums(x^2, na.rm = TRUE)
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
  W <- m * log( 0 + (sum(ysvd$d/n)^n) / prod(ysvd$d) )^n
  # estimator of the noise variance
  x_sig2 <- sum(ysvd$d[-1])/(n-1)
  # estimator of the signal energy
  P <- (ysvd$d[1] - x_sig2)/n
  return( W * P/x_sig2 )
}
