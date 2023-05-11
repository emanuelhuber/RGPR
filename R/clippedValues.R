# #' Return the clipDataped values of the GPR signal
# #'
# #' Max and min values
# #' 
# #' @export
# setGeneric("clipDatapedValues", function(x, nbits = NULL, xlim = NULL) 
#   standardGeneric("clipDatapedValues"))
# 
# setMethod("clipDatapedValues", "GPR", function(x, nbits = NULL, xlim = NULL){
#   if(is.null(nbits) && !is.null(xlim)){
#     # print("1")
#     xclipData <- list(clipDatamin = apply(x, 2, .getclipDataped, xclipData = min(xlim)),
#                   clipDatamax = apply(x, 2, .getclipDataped, xclipData = max(xlim)) )
#   }else if(!is.null(nbits) && is.null(xlim)){
#     # print("2")
#     # xclipData <- apply(x, 2, .getclipDataped, xmin = -2^nbits/2, xmax = 2^nbits/2 -1 )
#     xclipData <- getclipDatapedBits(x@data, nbits = nbits)
#   }else if (is.null(nbits) && is.null(xlim)){
#     # print("3")
#     if(!is.null(x@hd[["clipData"]]) && any(sapply(x@hd[["clipData"]], length) > 0)){
#       message("I take the already estimated clipDataped values in x@hd[['clipData']].")
#       xclipData <- x@hd[["clipData"]]
#     }else{
#       # print("3.1")
#       # estimate xclipData from the data (plateau)
#       message("I estimate the clipDataped values")
#       xmax <- (x@data == max(x))
#       xmin <- (x@data == min(x))
#       xclipDatamin <- -Inf
#       xclipDatamax <- Inf
#       testmin <- apply(xmin, 2, function(x) any(rle(diff(x))$lengths > 1 ) )
#       testmax <- apply(xmax, 2, function(x) any(rle(diff(x))$lengths > 1 ) )
#       xclipData <- list()
#       if(any(testmin)){
#         xclipData[["clipDatamin"]] <- apply(x, 2, .getclipDataped, xclipData = min(x))
#       }
#       if(any(testmax)){
#         xclipData[["clipDatamax"]] <- apply(x, 2, .getclipDataped, xclipData = max(x))
#       }
#       # xclipData <- apply(x, 2, .getclipDataped, xmin = xclipDatamin, xmax = xclipDatamax)
#     }
#   }else{
#     stop("lkj")
#   }
#   x@data[] <- 0
#   if(any(sapply(xclipData, length) != 0)){
#     x@data <- .clipDataMat(xclipData, n = nrow(x))
#   }
#   proc(x) <- getArgs()
#   return(x)
# })

.getclipDataped <- function(x, xclipData){
  which(x == xclipData)
}

# return list of clipDataped values: min and max
clippedBits <- function(x, nbits){
  xclipDatamin <- apply(x, 2, .getclipDataped, xclipData = -2^nbits/2 )
  xclipDatamax <- apply(x, 2, .getclipDataped, xclipData = 2^nbits/2 -1 )
  if(length(xclipDatamin) == 0 && length(xclipDatamax) == 0){
    return(NULL)
  }else{
    return(list(clipDatamin = xclipDatamin, clipDatamax = xclipDatamax))
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
  if(length(x[["clipDatamin"]]) > 0){
    xclipDatamin <- sapply(x[["clipDatamin"]], .clipDataTrace, n = n)
  }else{
    xclipDatamin <- rep(0, n)
  }
  if(length(x[["clipDatamax"]]) > 0){
    xclipDatamax <- sapply(x[["clipDatamax"]], .clipDataTrace, n = n)
  }else{
    xclipDatamax <- rep(0, n)
  }
  return(xclipDatamax - xclipDatamin)
}

