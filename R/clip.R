

setGenericVerif("clip", function(x, Amax = NULL, Amin = NULL, track = TRUE) 
  standardGeneric("clip"))

#' Clip the amplitude
#' 
#' @name clip
#' @rdname trClip
#' @export
setMethod("clip", "GPR", function(x, Amax = NULL, Amin = NULL,
                                  track = TRUE){
  x@data <- .clip(x@data,Amax,Amin)
  if(isTRUE(track)) proc(x) <- getArgs()
  #   x@proc <- c(x@proc, proc)
  return(x)
} 
)


.clip <- function(A, Amax = NULL, Amin = NULL){
  if(!is.null(Amin)){
    A[(A)< Amin] <- Amin
  }
  if(!is.null(Amax)){
    A[(A) > Amax] <- Amax
  }
  if(!is.null(Amax) && is.null(Amin)){
    A[(A)< -Amax] <- -Amax
  }
  return(A)
}