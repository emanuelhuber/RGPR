

setGeneric("localExtrema", function(x, threshold, values = TRUE) 
  standardGeneric("localExtrema"))


#' Local extrema
#' 
#' Returns the local extrema (either values or binary)
#' @param x An object of the class GPR.
#' @param threshold [\code{numeric(1)}] Threshold value for local extremum 
#'        detection. The larger the value, the longer the computation time.
#' @param If \code{values = TRUE},  a GPR data with the local extrema 
#'         (maxima and minima) is returned. If \code{values = FALSE}, 
#'         a binary GPR data with 1 for local extremum (else 0) is returned.
#' @param x An object of the class GPR.
#' @return GPR data of extremum values 
#' @examples 
#' x <- frenkeLine00
#' z <- localExtrema(x)
#' plot(z)
#' 
#' @export
setMethod("localExtrema", "GPR", function(x, threshold, values = TRUE){
# localExtrema <- function(x, threshold){
  if(isTRUE(values)){
    Xext <- apply(x@data, 2, .localExtrema, threshold = threshold)
    xdata <- x@data
    xdata[] <- NA
    for(i in seq_along(Xext)){
      xdata[Xext[[i]]$min,i] <- x@data[Xext[[i]]$min, i]
      xdata[Xext[[i]]$max,i] <- x@data[Xext[[i]]$max, i]
    }
  }else{
    xdata <- apply( abs(x@data), 2, .extVal, threshold = 0.1)
  }
  x@data <- xdata
  proc(x) <- getArgs()
  return(x)
})
    
  


.extVal <- function(z, threshold){
  idx <- localMax(z, threshold = threshold)
  z[] <- 0
  z[idx] <- 1
  return(z)
}
  
  # @name	localExtrema (find the local extrema relative to a given threshold)
# @description This function find the local extrema (min/max) of a vector given a threshold

# @date 24.02.2012 19:45
# @auteur Eli Billauer, 3.4.05 (Explicitly not copyrighted).
# @copyright This function is released to the public domain; Any use is allowed.
# @source http://www.billauer.co.il/peakdet.html

# @param [numeric(n)] 	v 			(vector of n numerical values, e.g. time series)
# @param [numeric(1)] 	threshold 		(threshold for computing the local extrema, e.g. threshold = sd(v)/2)

# @return list(mini = local minima, maxi = local maxima)
# -------------------------------------------

.localExtrema <- function(v, threshold){
  maxtab = c()
  mintab = c()
  x <- c(1:length(v))
  
  if(length(threshold)>1){
    cat('Input argument threshold must be a scalar\n')
  }
  
  if(threshold <= 0){
    cat('Input argument threshold must be positive\n')
  }
  
  mn    <-  Inf
  mx    <- -Inf
  mnpos <-  NA
  mxpos <-  NA
  
  lookformax = 1
  
  for(i in 1:length(v)){
    this = v[i];
    if(this > mx){ mx = this; mxpos = x[i]}
    if(this < mn){ mn = this; mnpos = x[i] }
    
    if(lookformax){
      if(this < mx - threshold){
        maxtab = c(maxtab, mxpos)
        mn = this; mnpos = x[i]
        lookformax = 0
      }
    }else{
      if(this > mn + threshold){
        mintab = c(mintab , mnpos)
        mx = this; mxpos = x[i]
        lookformax = 1
      }
    }
  }
  return(list(min = mintab, max = maxtab))
}


