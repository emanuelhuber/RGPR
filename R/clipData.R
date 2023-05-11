

#' clipData the amplitude
#' @name clipData
#' @rdname clipData
setGeneric("clipData", function(x, xclipData = NULL, track = TRUE) 
  standardGeneric("clipData"))


#' clipData the amplitude
#' @param x [\code{GPR}] GPR object.
#' @param xclipData [\code{numeric(1|2)}] Value above and below which the signal has 
#'                                to be clipDataped. If \code{xclipData} is a length-one
#'                                vector, the signal outside the range 
#'                                \code{-xclipData}, \code{xclipData} will be clipDataped.
#'                                If \code{xclipData} is a length-one
#'                                vector, the signal outside the range 
#'                                \code{xclipData[1]}, \code{xclipData[2]} will be 
#'                                clipDataped.
#' @param track [\code{logical(1)}] If \code{TRUE}, processing will be tracked.
#' @return [\code{GPR class}] clipDataped GPR object.
#' @rdname clipData
#' @export
#' @concept signal processing
setMethod("clipData", "GPRvirtual", function(x, xclipData = NULL, track = TRUE){
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(xclipData, msg, "NUMERIC_LEN", c(1, 2))
  msg <- checkArg(track, msg, "LOGICAL_LEN", 1)
  checkArgStop(msg)
  #    - ----------------------------------
  
  x@data <- .clipData(x@data, xclipData)
  if(isTRUE(track)) proc(x) <- getArgs()
  #   x@proc <- c(x@proc, proc)
  return(x)
} 
)


.clipData <- function(x, xclipData = NULL){
  sel <- !is.na(x) & !is.infinite(x)
  x1 <- x[sel]
  if(!is.null(xclipData)){
    if(length(xclipData) == 1){
      x1[x1 > xclipData] <- xclipData
      x1[x1 < -xclipData] <- -xclipData
    }else if(length(xclipData) == 2){
      xclipData <- sort(xclipData)
      x1[x1 > xclipData[2]] <- xclipData[2]
      x1[x1 < xclipData[1]] <- xclipData[1]
    }
  }
  x[sel] <- x1
  return(x)
}
