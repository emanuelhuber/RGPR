
#' Return length of a GPR object (number of traces)
#' @param x [\code{GPR}]
#' @aliases length,GPR-method
#' @export
setMethod("length", "GPR", function(x) ncol(x@data))

#' Summary of a GPR object 
#' 
#' Summary of all the values contained in \code{object}.
#' @param object [\code{GPR}]
#' @param ... Additional parameters to be passed (see \code{\link{summary}}).
#' @aliases summary,GPR-method
#' @export
setMethod("summary", "GPR", function(object, ...) summary(as.vector(object@data)))

#' Mean of a GPR object
#' 
#' Mean of all the values contained in \code{x}.
#' @param x [\code{GPR}]
#' @param ... Additional parameters to be passed (see \code{\link{mean}}).
#' @aliases mean,GPR-method
#' @export
setMethod("mean", "GPR", function(x, ...) mean(as.vector(x@data)))

#' Median of a GPR object
#' 
#' Median of all the values contained in \code{x}.
#' @param x [\code{GPR}]
#' @param na.rm [\code{logical}]  If \code{TRUE} NA values are stripped before 
#'                                the computation proceeds.
#' @aliases median,GPR-method
#' @export
setMethod("median", "GPR", function(x, na.rm = FALSE) 
  median(as.vector(x@data),na.rm = FALSE))
# setMethod("range", "GPR", function(..., na.rm=FALSE) 
# range(as.matrix(...),na.rm=na.rm))


#' Apply a function along the rows (samples per trace) or columns (traces)
#' @param X [\code{GPR}]
#' @param MARGIN [\code{integer}] A vector giving the subscripts which the 
#'                                function will be applied over
#'                                (see \code{\link{apply}}).
#' @param FUN [\code{function}] The function to be applied
#'                                (see \code{\link{apply}}).
#' @param ... Additional parameters to be passed (see \code{\link{apply}}).
#' @aliases apply,GPR-method
#' @export
setMethod("apply", "GPR", definition = function(X, MARGIN, FUN, ...){
    x_apply <- apply(X@data, MARGIN, FUN,...)
    if(MARGIN == 1 && is.null(dim(x_apply)) && length(x_apply) == nrow(X)){
      X[, 1:ncol(x_apply)] <- x_apply
    }
    return(x_apply)
})

#' Number of rows (samples per trace)
#' @param x [\code{GPR}]
#' @aliases nrow,GPR-method
#' @export
setMethod("nrow", "GPR", definition=function(x)  nrow(x@data))

#' Number of columns (samples per trace)
#' @param x [\code{GPR}]
#' @aliases ncol,GPR-method
#' @export
setMethod("ncol", "GPR", function(x)  ncol(x@data))

#' Dimensions of a GPR object 
#' Return the number of rows (samples per trace) and 
#' columns (samples per trace).
#' @param x [\code{GPR}]
#' @aliases dim,GPR-method
#' @export
setMethod("dim", "GPR", function(x)   dim(x@data))


