# #' Return the clipped values of the GPR signal
# #'
# #' Max and min values
# #' 
# #' @export
# setGeneric("clippedValues", function(x, nbits = NULL, xlim = NULL) 
#   standardGeneric("clippedValues"))
# 
# setMethod("clippedValues", "GPR", function(x, nbits = NULL, xlim = NULL){
#   if(is.null(nbits) && !is.null(xlim)){
#     # print("1")
#     xclip <- list(clipmin = apply(x, 2, .getClipped, xclip = min(xlim)),
#                   clipmax = apply(x, 2, .getClipped, xclip = max(xlim)) )
#   }else if(!is.null(nbits) && is.null(xlim)){
#     # print("2")
#     # xclip <- apply(x, 2, .getClipped, xmin = -2^nbits/2, xmax = 2^nbits/2 -1 )
#     xclip <- getClippedBits(x@data, nbits = nbits)
#   }else if (is.null(nbits) && is.null(xlim)){
#     # print("3")
#     if(!is.null(x@hd[["clip"]]) && any(sapply(x@hd[["clip"]], length) > 0)){
#       message("I take the already estimated clipped values in x@hd[['clip']].")
#       xclip <- x@hd[["clip"]]
#     }else{
#       # print("3.1")
#       # estimate xclip from the data (plateau)
#       message("I estimate the clipped values")
#       xmax <- (x@data == max(x))
#       xmin <- (x@data == min(x))
#       xclipmin <- -Inf
#       xclipmax <- Inf
#       testmin <- apply(xmin, 2, function(x) any(rle(diff(x))$lengths > 1 ) )
#       testmax <- apply(xmax, 2, function(x) any(rle(diff(x))$lengths > 1 ) )
#       xclip <- list()
#       if(any(testmin)){
#         xclip[["clipmin"]] <- apply(x, 2, .getClipped, xclip = min(x))
#       }
#       if(any(testmax)){
#         xclip[["clipmax"]] <- apply(x, 2, .getClipped, xclip = max(x))
#       }
#       # xclip <- apply(x, 2, .getClipped, xmin = xclipmin, xmax = xclipmax)
#     }
#   }else{
#     stop("lkj")
#   }
#   x@data[] <- 0
#   if(any(sapply(xclip, length) != 0)){
#     x@data <- .clipMat(xclip, n = nrow(x))
#   }
#   proc(x) <- getArgs()
#   return(x)
# })

.getClipped <- function(x, xclip){
  which(x == xclip)
}

# return list of clipped values: min and max
clippedBits <- function(x, nbits){
  xclipmin <- apply(x, 2, .getClipped, xclip = -2^nbits/2 )
  xclipmax <- apply(x, 2, .getClipped, xclip = 2^nbits/2 -1 )
  if(length(xclipmin) == 0 && length(xclipmax) == 0){
    return(NULL)
  }else{
    return(list(clipmin = xclipmin, clipmax = xclipmax))
  }
}

.clipTrace <- function(x, n){
  u <- rep(FALSE, n)
  u[x] <- TRUE
  return(u)
}

# n = number of rows
.clipMat <- function(x, n){
  # xclipmax - xclipmin
  if(length(x[["clipmin"]]) > 0){
    xclipmin <- sapply(x[["clipmin"]], .clipTrace, n = n)
  }else{
    xclipmin <- rep(0, n)
  }
  if(length(x[["clipmax"]]) > 0){
    xclipmax <- sapply(x[["clipmax"]], .clipTrace, n = n)
  }else{
    xclipmax <- rep(0, n)
  }
  return(xclipmax - xclipmin)
}

