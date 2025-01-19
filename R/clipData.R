

#' Clip the amplitude
#' @name clipData
#' @rdname clipData
setGeneric("clipData", function(x, cl = NULL, track = TRUE) 
  standardGeneric("clipData"))


#' Clip the amplitude
#' @param x (`GPR`) GPR object.
#' @param cl (`numeric[1|2]`) Value above and below which the signal has 
#'                                to be clipDataped. If `cl` is a length-one
#'                                vector, the signal outside the range 
#'                                `-cl`, `cl` will be clipDataped.
#'                                If `cl` is a length-one
#'                                vector, the signal outside the range 
#'                                `cl[1]`, `cl[2]` will be 
#'                                clipDataped.
#' @param track (`logical[1]`) If `TRUE`, processing will be tracked.
#' @return (`GPR class`) clipDataped GPR object.
#' @rdname clipData
#' @export
#' @concept signal processing
setMethod("clipData", "GPRvirtual", function(x, cl = NULL, track = TRUE){
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(cl, msg, "NUMERIC_LEN", c(1, 2))
  msg <- checkArg(track, msg, "LOGICAL_LEN", 1)
  checkArgStop(msg)
  #    - ----------------------------------
  
  x@data <- .clipData(x@data, cl)
  if(isTRUE(track)) proc(x) <- getArgs()
  #   x@proc <- c(x@proc, proc)
  return(x)
} 
)


.clipData <- function(x, cl = NULL){
  sel <- !is.na(x) & !is.infinite(x)
  x1 <- x[sel]
  if(!is.null(cl)){
    if(length(cl) == 1){
      x1[x1 > cl] <- cl
      x1[x1 < -cl] <- -cl
    }else if(length(cl) == 2){
      cl <- sort(cl)
      x1[x1 > cl[2]] <- cl[2]
      x1[x1 < cl[1]] <- cl[1]
    }
  }
  x[sel] <- x1
  return(x)
}
