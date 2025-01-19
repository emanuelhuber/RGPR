
#' Return length of a GPR object (number of traces)
#' @param x (`GPR`)
#' @aliases length,GPR-method
#' @export
#' @concept array
setMethod("length", "GPR", function(x) ncol(x@data))

#' Return length of a GPRcube/GPRslice object (number of slices)
#' @param x (`GPR`)
#' @aliases length,GPRcube-method
#' @export
#' @concept array
setMethod("length", "GPRcube", function(x) dim(x@data)[3])

#' Summary of a GPR object 
#' 
#' Summary of all the values contained in `object`.
#' @param object (`GPR`)
#' @param ... Additional parameters to be passed (see [summary()]).
#' @aliases summary,GPRvirtual-method
#' @export
#' @concept statistics
setMethod("summary", "GPRvirtual", function(object, ...) summary(as.vector(object@data), ...))

#' Mean of a GPR object
#' 
#' Mean of all the values contained in `x`.
#' @param x (`GPR`)
#' @param ... Additional parameters to be passed (see [mean()]).
#' @aliases mean,GPRvirtual-method
#' @export
#' @concept statistics
setMethod("mean", "GPRvirtual", function(x, ...) mean(as.vector(x@data), ...))

#' Median of a GPR object
#' 
#' Median of all the values contained in `x`.
#' @param x (`GPR`)
#' @param na.rm [`logical`]  If `TRUE` NA values are stripped before 
#'                                the computation proceeds.
#' @aliases median,GPRvirtual-method
#' @export
#' @concept statistics
setMethod("median", "GPRvirtual", function(x, na.rm = FALSE) 
  median(as.vector(x@data), na.rm = FALSE))
# setMethod("range", "GPR", function(..., na.rm=FALSE) 
# range(as.matrix(...),na.rm=na.rm))


#' Apply a function along the rows (samples per trace) or columns (traces)
#' @param X (`GPR`)
#' @param MARGIN (`integer[1|2]`) A vector giving the subscripts which the 
#'                                function will be applied over
#'                                (see [apply()]).
#' @param FUN (`function`) The function to be applied
#'                                (see [apply()]).
#' @param ... Additional parameters to be passed (see [apply()]).
#' @param simplify (`logical[1]`) If `TRUE` the results should be simplified if possible.
#' @aliases apply,GPRvirtual-method
#' @export
#' @concept array
setMethod("apply", "GPRvirtual", definition = function(X, MARGIN, FUN, ..., simplify = TRUE){
    x_apply <- apply(X@data, MARGIN, FUN,...)
    if(MARGIN == 1 && is.null(dim(x_apply)) && length(x_apply) == nrow(X)){
      X[, 1:ncol(x_apply)] <- x_apply
    }
    return(x_apply)
})

#' Number of rows (samples per trace)
#' @param x (`GPR`)
#' @aliases nrow,GPRvirtual-method
#' @export
#' @concept array
setMethod("nrow", "GPRvirtual", definition=function(x)  nrow(x@data))

#' Number of columns (samples per trace)
#' @param x (`GPR`)
#' @aliases ncol,GPRvirtual-method
#' @export
#' @concept array
setMethod("ncol", "GPRvirtual", function(x)  ncol(x@data))

#' Dimensions of a GPR object 
#' 
#' Return the number of rows (samples per trace) and 
#' columns (samples per trace).
#' @param x (`GPR`)
#' @aliases dim,GPRvirtual-method
#' @export
#' @concept array
setMethod("dim", "GPRvirtual", function(x)   dim(x@data))


#---------------------------- COLSUMS -----------------------------------------#

#' Form Row and Column Sums and Means
#' 
#' Form row and column sums and means 
#' @param x (`GPR`)
#' @param na.rm	(`logical[1]`). Should missing values (including 
#' `NaN`) be omitted from the calculations?
#' @param dims (`integer[1]`)  Which dimensions are regarded as ‘rows’ or 
#'             ‘columns’ to sum over.(see [colSums()]).
#' @aliases colSums,GPRvirtual-method
#' @rdname colSums
#' @export
#' @concept arithmetic
setMethod("colSums", "GPRvirtual", function(x, na.rm = FALSE, dims = 1){
  x@data[1, ] <- colSums(x@data, na.rm = na.rm, dims = dims)
  x <- x[1,]
  return(x)
})

#' @aliases rowSums,GPRvirtual-method
#' @rdname colSums
#' @export
#' @concept arithmetic
setMethod("rowSums", "GPRvirtual", function(x, na.rm = FALSE, dims = 1){
  x@data[, 1] <- rowSums(x@data, na.rm = na.rm, dims = dims)
  x <- x[,1]
  return(x)
})

#' @aliases colMeans,GPRvirtual-method
#' @rdname colSums
#' @export
#' @concept arithmetic
setMethod("colMeans", "GPRvirtual", function(x, na.rm = FALSE, dims = 1){
  x@data[1, ] <-colMeans(x@data, na.rm = na.rm, dims = dims)
  x <- x[1,]
  return(x)
})

#' @aliases rowMeans,GPRvirtual-method
#' @rdname colSums
#' @export
#' @concept arithmetic
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
#' @param x (`GPR*`)
#' @return  (`GPR*`) With logical values (`TRUE` is the value is 
#'          finite, `FALSE` if not.)
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
#' @param x (`GPR*`)
#' @return  (`GPR*`) With logical values (`TRUE` is the value is 
#'          `NA`, `FALSE` if not.)
#' @rdname is.na
setMethod("is.na", "GPRvirtual", function(x){
  x@data <- is.na(x@data)
  return(x)
}
)

