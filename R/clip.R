

#' Clip the amplitude
#' @name clip
#' @rdname clip
setGeneric("clip", function(x, xclip = NULL, track = TRUE) 
  standardGeneric("clip"))


#' Clip the amplitude
#' @param x [\code{GPR}] GPR object.
#' @param xclip [\code{numeric(1|2)}] Value above and below which the signal has 
#'                                to be clipped. If \code{xclip} is a length-one
#'                                vector, the signal outside the range 
#'                                \code{-xclip}, \code{xclip} will be clipped.
#'                                If \code{xclip} is a length-one
#'                                vector, the signal outside the range 
#'                                \code{xclip[1]}, \code{xclip[2]} will be 
#'                                clipped.
#' @param track [\code{logical(1)}] If \code{TRUE}, processing will be tracked.
#' @return [\code{GPR class}] Clipped GPR object.
#' @rdname clip
#' @export
setMethod("clip", "GPRvirtual", function(x, xclip = NULL, track = TRUE){
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(xclip, msg, "NUMERIC_LEN", c(1, 2))
  msg <- checkArg(track, msg, "LOGICAL_LEN", 1)
  checkArgStop(msg)
  #    - ----------------------------------
  
  x@data <- .clip(x@data, xclip)
  if(isTRUE(track)) proc(x) <- getArgs()
  #   x@proc <- c(x@proc, proc)
  return(x)
} 
)


.clip <- function(A, Aclip = NULL){
  if(!is.null(Aclip)){
    if(length(Aclip) == 1){
      A[A > Aclip] <- Aclip
      A[A < -Aclip] <- -Aclip
    }else if(length(Aclip) == 2){
      Aclip <- sort(Aclip)
      A[A > Aclip[2]] <- Aclip[2]
      A[A < Aclip[1]] <- Aclip[1]
    }
  }
  return(A)
}