#' Trace statistics
#'
#' `trapply` is a generic function used to produce results defined
#' by an user function. The user function is applied accross traces (horizontal) 
#' using a moving window. Note that if the moving window length is not defined, 
#' all traces are averaged into one single trace (the results is similar to
#' `apply(x, 1, FUN, ...)`.
#' 
#' @param x An object of the class GPR
#' @param w A length-one integer vector equal to the window length of the 
#'          average window. If `w = NULL` similar to `apply(x, MARGIN = 2, FUN, ...)`
#' @param FUN A function to compute the average (default is `mean`)
#' @param ... Additional parameters for the FUN functions
#' @param track (`logical[1]`) Should the processing step be tracked? 
#' @return An object of the class GPR. When `w = NULL`, this function 
#'         returns a GPR object with a single trace corresponding to the 
#'         average trace of the whole radargram. When `w` is equal to a
#'         strictly positive interger this function returns a GPR object with
#'         a size identical to x where each trace corresponds to the average
#'         of the `w` neighbouring traces centered on the considered trace.
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
