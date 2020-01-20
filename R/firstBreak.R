setGenericVerif("firstBreak", function(x, method = c("coppens",
                                                     "threshold",  "MER"), thr = 0.12, w = 11, ns = NULL, 
                                       bet = NULL)
  standardGeneric("firstBreak"))


#----------------- FIRST-BREAK
#' Time of first wave break
#'
#' Pick the time corresponding to the first break of each trace in the GPR profile.
#' Return a vector containing the first break times.
#' 
#' @param x [\code{GPR class}] An object of the class \code{GPR}
#' @param method [\code{character(1)}] Method to be applied (either
#'              \code{coppens}, \code{threshold} or \code{MER}). 
#'              \code{"coppens"} corresponds to the modified Coppens method, 
#'              \code{"threshold"} to the threshold method, 
#'              and \code{"MER"} to the modified energy ratio method.
#' @param thr [\code{numeric(1)}] Threshold for the signal 
#'              amplitude (in \%) at which time zero is picked (only for the
#'              threshold method). \code{thr} ranges between 0 and 1.
#' @param w [\code{numeric(1)}] Length of the leading window in unit of time
#'          (only for the modified Coppens and modified energy ratio 
#'          methods). Recommended value: about one period of the first-arrival 
#'          waveform.
#' @param ns [\code{numeric(1)}] Length of the edge preserving smoothing 
#'           window in unit of time (only for the modified Coppens 
#'           method). Recommended value: between one and two signal periods.
#'           When \code{ns = NULL} the value of \code{ns} is set to 
#'           \code{1.5 * w}.
#' @param bet [\code{numeric(1)}] Stabilisation constant (only for the 
#'            modified Coppens method). Not critical. 
#'            When \code{bet = NULL} the value of \code{bet} is set to 
#'            20\% of the maximal signal amplitude.
#'            
#' @return [\code{numeric(n)}] The time of the first wave break for every
#'         traces in unit of time (\code{n = ncol(x) =} number of traces).
#'         
#' @seealso \code{\link{firstBreakToTime0}} to convert time of first wave break
#'          into time-zero; 
#'          \code{\link{time0}} and \code{\link{setTime0}} to set time-zero;
#'          \code{\link{estimateTime0}} to estimate first wave break, convert
#'          it to time-zero and set time zero (all in one step);
#'          \code{\link{time0Cor}} to shift the traces such that they start
#'          at time-zero.
#'          
#' @references
#' \describe{
#'   \item{Modified Coppens method}{Sabbione J.I. and Velis D. (2010) 
#'        Automatic first-breaks picking: New strategies and algorithms. 
#'        Geophysics, 75(4): 67-76.}
#'   \item{Modified Energy Ratio (MER) method}{Han L., Wong J., and John C. 
#'        (2010) Time picking on noisy microseismograms. In: Proceedings of the
#'        GeoCanada 2010 Convention - Working with the Earth, Calgary, AB, 
#'        Canada, p. 4}
#' }
#'
#' #' @examples 
#' data("frenkeLine00")
#' fb <- firstbreak(frenkeLine00, w = 10)
#' plot(seq_along(frenkeLine00), fb)
#' 
#' @name firstBreak
#' @rdname firstBreak
#' @export
setMethod("firstBreak", 
          "GPR",
          function(x, method = c("coppens","threshold",  "MER"), 
                   thr = 0.12, w = 11, ns = NULL, bet = NULL){
            #method <- match.arg(method, c("coppens", "threshold", "MER"))
            method <- method[1]
            
            # shorten the file -> computation only up to the max value
            nmax <- nrow(x)
            # tst <- which(as.matrix(x) == max(x), arr.ind = TRUE)
            tst <- max(apply(as.matrix(x), 2, which.max))
            
            if(length(tst) > 0 ){
              # nmax <- max(tst[,"row"])
              nmax <- tst
            }
            
            #------------------- check arguments
            msg <- checkArgInit()
            msg <- checkArg(method, msg, "STRING_CHOICE", 
                            c("coppens", "threshold",  "MER"))
            msg <- checkArg(thr   , msg, "PERCENT1")
            # msg <- checkArg(w     , msg, "NUMERIC1_SPOS", round((nmax - 1) * x@dz/1.5))
            msg <- checkArg(w     , msg, "NUMERIC1_SPOS", max(x@depth)/2)
            # msg <- checkArg(ns    , msg, "NUMERIC1_SPOS_NULL", round((nmax - 1) * x@dz))
            msg <- checkArg(ns    , msg, "NUMERIC1_SPOS_NULL", max(x@depth))
            msg <- checkArg(bet   , msg, "NUMERIC1_SPOS_NULL", Inf)
            checkArgStop(msg)
            #-----------------------------------
            
            
            w <- round(w / x@dz)
            
            if( (nmax + 2 * w ) < nrow(x) ){
              nmax <- nmax + 2 * w
            }else{
              nmax <- nrow(x)
            }
            
            if(method == "coppens"){
              if( (w %% 2) == 0 ) w <- w + 1
              xs <- x@data[1:nmax, , drop = FALSE]^2
              
              if(is.null(ns)){
                ns <- round(1.5 * w)
              }else{
                ns <- round(ns / x@dz)
              }
              # ns <- if(is.null(ns)) round(1.5 * w) else ns
              
              if(ns > nmax) ns <- nmax - 1
              if( (ns %% 2) == 0 )  ns <- ns + 1 
              if(is.null(bet))      bet <- 0.2 * max(xs)
              
              # the vectorized version of "coppens" (though not really faster)
              #  fb <- .firstBreakModCoppens2(xs, w = w, ns = ns, bet = bet)
              # below: the not vectorised version of Coppens...
              fb <- apply(xs, 2, .firstBreakModCoppens, w = w, ns = ns, bet = bet)
              fb <- x@depth[fb] # fb * x@dz
            }else if(method == "threshold"){
              thres <- thr * max(x)
              fb <- apply(abs(x@data), 2, .firstBreakThres, thr = thres, x@depth)
            }else if(method == "MER"){
              # w <- round(w / x@dz)
              fb <- .firstBreakMER(x@data[1:nmax, , drop = FALSE], w)
              fb <- x@depth[fb]
            }
            if(any(is.na(fb))){
              warning("First break could not be picked for some traces. \n",
                      "That's no luck, but good news is that you can try with another ",
                      "method.\n", "This is probably because your traces have a ",
                      "too low S/N ratio." )
            }
            return(fb)
          } 
)



#----------------------- FIRST WAVE BREAK -------------------------------------#

# Modified Energy ratio method
.firstBreakMER <- function(x, w){
  E <- wapplyMat2(x, width = w, by = 1, FUN = function(x) sum(x^2), 
                  MARGIN = 2)
  v1 <- 1:(nrow(x) - 2*(w-1))
  v2 <- v1 + (w-1)
  E1 <- E[v1,]
  E2 <- E[v2,]
  ER <- E2/E1
  MER <- (ER * abs( x[v1 + w - 1,]) )^3
  fb <- apply(MER, 2, function(x) which.max(x)) + (w - 1)
  return(fb)
}

# Threshold method for first breack picking
.firstBreakThres <- function(x, thr = 0.12, tt){
  #   first_breacks <- rep(NA, ncol(x))
  #   thres <- thr * max(x)
  #   for(j in seq_len(ncol(x))){
  if( max(x) > thr){
    fb <- which(x > thr)
    if(length(fb) > 0){
      i <- fb[1]
      if(i > 1){
        w <- (x[i] - thr) / (x[i] - x[i-1])
        return( w * tt[i-1] + (1- w) * tt[i] )
      }else{
        return(tt[i])
      }
    } else{
      return(NA)
    }
  } else{
    return(NA)
  }
  #   }
  
  #   return(first_breacks)
}

# Jaun I. Sabbione and Danilo Velis (2010). Automatic first-breaks picking: 
# New strategies and algorithms. Geophysics, 75 (4): v67-v76
# -> modified Coppens's Method
# w = length leading window: about one period of the first-arrival waveform
# ns = length eps (edge preserving smoothing) window: good results with ns 
# between one and two signal periods
#        -> default values ns= 1.5*w
# bet = stabilisation constant, not critical, set to 0.2*max(amplitude) 
.firstBreakModCoppens2 <- function(x, w = 11, ns = NULL, bet = 0.2){
  if(is.null(ns)){
    ns <- 1.5 * w
  }
  
  E1all <- matrix(0, nrow=nrow(x), ncol= ncol(x))
  E1all[1:(nrow(E1all) - w +1),] <- wapplyMat2(x, width = w, by = 1, 
                                               FUN = sum, MARGIN=2)
  E2all <- apply(x, 2, cumsum)
  Erall <- E1all/(E2all + bet)
  
  xmeanall <- wapplyMat(Erall, width = ns, by = 1, FUN = mean, MARGIN=2)
  xsdall <- wapplyMat(Erall, width = ns, by = 1, FUN = sd, MARGIN=2)
  xtestall <- wapplyMat2(xsdall, width = ns, by = 1, FUN = which.min, MARGIN=2)
  xtestall <- xtestall + seq_len(nrow(xtestall))
  meantstall <- matrix(xmeanall[xtestall],nrow=nrow(xtestall), 
                       ncol=ncol(xmeanall), byrow=FALSE)
  meantstall2 <- matrix(0, nrow=nrow(x), ncol= ncol(x))
  meantstall2[seq_len(nrow(meantstall)) + (ns-1)/2,] <- meantstall
  fb <- apply(meantstall2, 2, function(x) which.max(abs(diff(x))))
  return(fb)
}

.firstBreakModCoppens <- function(x, w = 11, ns = NULL, bet = 0.2){
  if(is.null(ns)){
    ns <- 1.5 * w
  }
  
  E1 <- c(wapply(x, width = w, by = 1, FUN = sum), rep(0, 2*floor(w/2)))
  E2 <- cumsum(x)
  Er <- E1/(E2 + bet)
  Er_fil <- .eps(Er, ns = ns)
  fb <- which.max(abs(diff(Er_fil)))
  return(fb)
}

# edge preserving smoothing
# luo et al. (2002): Edge preserving smoothing and applications: 
# The Leading edge, 21: 136-158
.eps <- function(x, ns){
  xmean <-  c(rep(0, floor(ns/2)), 
              wapply(x, width = ns, by = 1, FUN = mean),
              rep(0, floor(ns/2)))
  xsd <- c(rep(0, floor(ns/2)), 
           wapply(x, width = ns, by = 1, FUN = sd),
           rep(0, floor(ns/2)))
  xtest <- wapply(xsd, width = ns, by = 1, FUN = which.min) + 
    (0):(length(xmean)- 2*floor(ns/2)-1)
  return(c(rep(0, floor(ns/2)), xmean[xtest], rep(0, floor(ns/2))))
}