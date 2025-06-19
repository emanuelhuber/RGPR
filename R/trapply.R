#' Trace statistics
#'
#' `trapply` is a generic function used to produce results defined
#' by an user function. The user function is applied accross the samples (vertical) 
#' using a moving window. Note that if the moving window length is not defined, 
#' all samples are averaged into one single vector (the results is similar to
#' `apply(x, 2, FUN, ...)`.
#' 
#' @param x An object of the class GPR
#' @param w A length-one integer vector equal to the window length of the 
#'          average window. If `w = NULL` similar to `apply(x, MARGIN = 2, FUN, ...)`
#' @param FUN A function to compute the average (default is `mean`)
#' @param ... Additional parameters for the FUN functions
#' @param track (`logical[1]`) Should the processing step be tracked? 
#' @return An object of the class GPR. When `w = NULL`, this function 
#'         returns a GPR object with a as many trace as the original GPR object
#'         but with potentially a different number of samples.
#' @name trapply
#' @rdname trapply
#' @export
#' @concept processing
setGeneric("trapply", 
           function(x, w = NULL, FUN = mean, ...,
                    track = TRUE)
             standardGeneric("trapply"))

#' @rdname trapply
#' @export
setMethod("trapply", "GPR", function(x, w = NULL, FUN = mean, ...,
                                       track = TRUE){
  FUN <- match.fun(FUN)
  # xdata <- x@data
  if(is.null(w)){
    xdata <- apply(x@data, 2, FUN, ...)
    if(is.null(dim(xdata))){
      dim(xdata) <- c(1, length(xdata))
    }
  }else{
    xdata <- wapplyMat(x@data, width = w, by = 1, FUN = FUN, MARGIN = 2, ...)
  }
  x <- x[1:nrow(xdata), ]
  x@data <- xdata
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
}
)
