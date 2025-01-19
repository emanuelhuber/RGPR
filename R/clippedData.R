
#' Return the clipped values of the GPR signal as GPR object
#' @rdname clippedData   
#' @name clippedData  
setGeneric("clippedData", function(obj, dlim = NULL, verbose = TRUE)
  standardGeneric("clippedData"))


#' Return the clipped values of the GPR signal as GPR object
#'
#' Max and min values
#' 
#' @param obj (`GPR`)
#' @param dlim (`numeric[2]`) The min and max clipped values if they are
#'             a priori known. If `dlim = NULL`, the function tries to
#'             get the the clipped values from `metadata(obj)$clip`. If
#'             there are no clipped values, the function tries to estimate 
#'             them.
#' @param verbose (`logical[1]`) If `FALSE`, all messages and warnings are
#'                suppressed (use with care).
#' @return (`GPR`) The object with the clipped values.
#' @rdname clippedData   
#' @export
#' @concept signal processing
setMethod("clippedData", "GPR", function(obj, dlim = NULL, verbose = TRUE){
  # compute the clipped values based on dlim
  if(!is.null(dlim)){
    objclip <- list(min = apply(obj, 2, .getClippedData, cl = min(dlim)),
                    max = apply(obj, 2, .getClippedData, cl = max(dlim)) )
  }else{
    # get the clipped values from obj@md$clip
    if(!is.null(obj@md[["clip"]]) && any(sapply(obj@md[["clip"]], length) > 0)){
      if(verbose) message("I take the already estimated clipDataped values in obj@md$clip.")
      objclip <- obj@md[["clip"]]
    }else{
      # estimate xclipData from the data (plateau)
      if(verbose) message("I estimate the clipped values")
      xmax <- (obj@data == max(obj))
      xmin <- (obj@data == min(obj))
      testmin <- apply(xmin, 2, function(x) any(rle(diff(x))$lengths > 1 ) )
      testmax <- apply(xmax, 2, function(x) any(rle(diff(x))$lengths > 1 ) )
      objclip <- list()
      if(any(testmin)){
        objclip[["min"]] <- apply(obj, 2, .getClippedData, cl = min(obj))
      }
      if(any(testmax)){
        objclip[["max"]] <- apply(obj, 2, .getClippedData, cl = max(obj))
      }
    }
  }
  obj@data[] <- 0
  if(any(sapply(objclip, length) != 0)){
    obj@data <- .clipDataMat(objclip, n = nrow(obj))
  }
  proc(obj) <- getArgs()
  return(obj)
})

.getClippedData <- function(x, cl){
  which(x == cl)
}

# return list of clipDataped values: min and max
clippedBits <- function(x, nbits){
  xclipDatamin <- apply(x, 2, .getClippedData, cl = -2^nbits/2 )
  xclipDatamax <- apply(x, 2, .getClippedData, cl = 2^nbits/2 -1 )
  if(length(xclipDatamin) == 0 && length(xclipDatamax) == 0){
    return(NULL)
  }else{
    return(list(min = xclipDatamin, max = xclipDatamax))
  }
}

.clipDataTrace <- function(x, n){
  u <- rep(FALSE, n)
  u[x] <- TRUE
  return(u)
}

# n = number of rows
.clipDataMat <- function(x, n){
  # xclipDatamax - xclipDatamin
  if(length(x[["min"]]) > 0){
    xclipDatamin <- sapply(x[["min"]], .clipDataTrace, n = n)
  }else{
    xclipDatamin <- rep(0, n)
  }
  if(length(x[["max"]]) > 0){
    xclipDatamax <- sapply(x[["max"]], .clipDataTrace, n = n)
  }else{
    xclipDatamax <- rep(0, n)
  }
  return(xclipDatamax - xclipDatamin)
}

