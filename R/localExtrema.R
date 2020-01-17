

setGeneric("localExtrema", function(x, delta) 
  standardGeneric("localExtrema"))


#' Returns the local extrema
#' 
#' Local extrema
#' 
#' @export
setMethod("localExtrema", "GPR", function(x, delta){
# localExtrema <- function(x, delta){
  Xext <- apply(x@data, 2, .localExtrema, delta = delta)
  xdata <- x@data
  xdata[] <- NA
  
  for(i in seq_along(Xext)){
    xdata[Xext[[i]]$min,i] <- x@data[Xext[[i]]$min, i]
    xdata[Xext[[i]]$max,i] <- x@data[Xext[[i]]$max, i]
  }
  x@data <- xdata
  proc(x) <- getArgs()
  return(x)
})
    
  
  
  
  # @name	localExtrema (find the local extrema relative to a given threshold)
# @description This function find the local extrema (min/max) of a vector given a threshold

# @date 24.02.2012 19:45
# @auteur Eli Billauer, 3.4.05 (Explicitly not copyrighted).
# @copyright This function is released to the public domain; Any use is allowed.
# @source http://www.billauer.co.il/peakdet.html

# @param [numeric(n)] 	v 			(vector of n numerical values, e.g. time series)
# @param [numeric(1)] 	delta 		(threshold for computing the local extrema, e.g. delta = sd(v)/2)

# @return list(mini = local minima, maxi = local maxima)
# -------------------------------------------

.localExtrema <- function(v, delta){
  maxtab = c()
  mintab = c()
  x <- c(1:length(v))
  
  if(length(delta)>1){
    cat('Input argument DELTA must be a scalar\n')
  }
  
  if(delta <= 0){
    cat('Input argument DELTA must be positive\n')
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
      if(this < mx - delta){
        maxtab = c(maxtab, mxpos)
        mn = this; mnpos = x[i]
        lookformax = 0
      }
    }else{
      if(this > mn + delta){
        mintab = c(mintab , mnpos)
        mx = this; mxpos = x[i]
        lookformax = 1
      }
    }
  }
  return(list(min = mintab, max = maxtab))
}


