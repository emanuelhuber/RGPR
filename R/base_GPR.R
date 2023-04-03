
#' Return length of a GPR object (number of traces)
#' @param x [\code{GPR}]
#' @aliases length,GPR-method
#' @export
#' @concept array
setMethod("length", "GPR", function(x) ncol(x@data))

#' Return length of a GPRcube/GPRslice object (number of slices)
#' @param x [\code{GPR}]
#' @aliases length,GPRcube-method
#' @export
setMethod("length", "GPRcube", function(x) dim(x@data)[3])

#' Summary of a GPR object 
#' 
#' Summary of all the values contained in \code{object}.
#' @param object [\code{GPR}]
#' @param ... Additional parameters to be passed (see \code{\link{summary}}).
#' @aliases summary,GPRvirtual-method
#' @export
setMethod("summary", "GPRvirtual", function(object, ...) summary(as.vector(object@data), ...))

#' Mean of a GPR object
#' 
#' Mean of all the values contained in \code{x}.
#' @param x [\code{GPR}]
#' @param ... Additional parameters to be passed (see \code{\link{mean}}).
#' @aliases mean,GPRvirtual-method
#' @export
setMethod("mean", "GPRvirtual", function(x, ...) mean(as.vector(x@data), ...))

#' Median of a GPR object
#' 
#' Median of all the values contained in \code{x}.
#' @param x [\code{GPR}]
#' @param na.rm [\code{logical}]  If \code{TRUE} NA values are stripped before 
#'                                the computation proceeds.
#' @aliases median,GPRvirtual-method
#' @export
setMethod("median", "GPRvirtual", function(x, na.rm = FALSE) 
  median(as.vector(x@data), na.rm = FALSE))
# setMethod("range", "GPR", function(..., na.rm=FALSE) 
# range(as.matrix(...),na.rm=na.rm))


#' Apply a function along the rows (samples per trace) or columns (traces)
#' @param X [\code{GPR}]
#' @param MARGIN [\code{integer(1|2)}] A vector giving the subscripts which the 
#'                                function will be applied over
#'                                (see \code{\link{apply}}).
#' @param FUN [\code{function}] The function to be applied
#'                                (see \code{\link{apply}}).
#' @param ... Additional parameters to be passed (see \code{\link{apply}}).
#' @param simplify [\code{logical(1)}] If \code{TRUE} the results should be simplified if possible.
#' @aliases apply,GPRvirtual-method
#' @export
setMethod("apply", "GPRvirtual", definition = function(X, MARGIN, FUN, ..., simplify = TRUE){
    x_apply <- apply(X@data, MARGIN, FUN,...)
    if(MARGIN == 1 && is.null(dim(x_apply)) && length(x_apply) == nrow(X)){
      X[, 1:ncol(x_apply)] <- x_apply
    }
    return(x_apply)
})

#' Number of rows (samples per trace)
#' @param x [\code{GPR}]
#' @aliases nrow,GPRvirtual-method
#' @export
#' @concept array
setMethod("nrow", "GPRvirtual", definition=function(x)  nrow(x@data))

#' Number of columns (samples per trace)
#' @param x [\code{GPR}]
#' @aliases ncol,GPRvirtual-method
#' @export
#' @concept array
setMethod("ncol", "GPRvirtual", function(x)  ncol(x@data))

#' Dimensions of a GPR object 
#' Return the number of rows (samples per trace) and 
#' columns (samples per trace).
#' @param x [\code{GPR}]
#' @aliases dim,GPRvirtual-method
#' @export
#' @concept array
setMethod("dim", "GPRvirtual", function(x)   dim(x@data))


#---------------------------- COLSUMS -----------------------------------------#

#' Form Row and Column Sums and Means
#' 
#' Form row and column sums and means 
#' @param x [\code{GPR}]
#' @param na.rm	[\code{logical(1)}]. Should missing values (including 
#' \code{NaN}) be omitted from the calculations?
#' @param dims [\code{integer(1)}]  Which dimensions are regarded as ‘rows’ or 
#'             ‘columns’ to sum over.(see \code{\link{colSums}}).
#' @aliases colSums,GPRvirtual-method
#' @rdname colSums
#' @export
setMethod("colSums", "GPRvirtual", function(x, na.rm = FALSE, dims = 1){
  x@data[1, ] <- colSums(x@data, na.rm = na.rm, dims = dims)
  x <- x[1,]
  return(x)
})

#' @aliases rowSums,GPRvirtual-method
#' @rdname colSums
#' @export
setMethod("rowSums", "GPRvirtual", function(x, na.rm = FALSE, dims = 1){
  x@data[, 1] <- rowSums(x@data, na.rm = na.rm, dims = dims)
  x <- x[,1]
  return(x)
})

#' @aliases colMeans,GPRvirtual-method
#' @rdname colSums
#' @export
setMethod("colMeans", "GPRvirtual", function(x, na.rm = FALSE, dims = 1){
  x@data[1, ] <-colMeans(x@data, na.rm = na.rm, dims = dims)
  x <- x[1,]
  return(x)
})

#' @aliases rowMeans,GPRvirtual-method
#' @rdname colSums
#' @export
setMethod("rowMeans", "GPRvirtual", function(x, na.rm = FALSE, dims = 1){
  x@data[, 1] <- rowMeans(x@data, na.rm = na.rm, dims = dims)
  x <- x[,1]
  return(x)
})

# colSums (x, na.rm = FALSE, dims = 1)
# rowSums (x, na.rm = FALSE, dims = 1)
# colMeans(x, na.rm = FALSE, dims = 1)
# rowMeans(x, na.rm = FALSE, dims = 1)


#---------------------------- FINITE ------------------------------------------#
#' Finite, Infinite and NAN Numbers
#' 
#' is.finite and is.infinite return an object of the same dimension as x, 
#' indicating which elements are finite (not infinite and not missing) or 
#' infinite.
#' @param x [\code{GPR*}]
#' @return  [\code{GPR*}] With logical values (\code{TRUE} is the value is 
#'          finite, \code{FALSE} if not.)
#' @name is.finite
#' @aliases is.finite,GPRvirtual-method
#' @rdname is.finite-GPRvirtual
#' @export
setMethod("is.finite", "GPRvirtual", function(x){
  x@data <- is.finite(x@data)
  return(x)
}
)

#' @name is.infinite
#' @aliases is.infinite,GPRvirtual-method
#' @rdname is.finite-GPRvirtual
#' @export
setMethod("is.infinite", "GPRvirtual", function(x){
  x@data <- is.infinite(x@data)
  return(x)
}
)

#' @name is.nan
#' @aliases is.nan,GPRvirtual-method
#' @rdname is.finite-GPRvirtual
#' @export
setMethod("is.nan", "GPRvirtual", function(x){
  x@data <- is.nan(x@data)
  return(x)
}
)

#------------------------------- NA -------------------------------------------#

#' Logical negation
#' 
#'  Indicates which elements are missing.
#' @param x [\code{GPR*}]
#' @return  [\code{GPR*}] With logical values (\code{TRUE} is the value is 
#'          \code{NA}, \code{FALSE} if not.)
#' @rdname is.na
setMethod("is.na", "GPRvirtual", function(x){
  x@data <- is.na(x@data)
  return(x)
}
)

