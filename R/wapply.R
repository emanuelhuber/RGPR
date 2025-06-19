
#' Accross trace computations
#'
#' `wapply` is a generic function used to produce results defined
#' by an user function. The user function is applied accross traces (horizontal) 
#' using a moving window. Note that if the moving window length is not defined, 
#' all traces are averaged into one single trace (the results is similar to
#' `apply(x, 1, FUN, ...)`.
#' 
#' @param x An object of the class GPR
#' @param w A length-one integer vector equal to the window length of the 
#'          average window. If `w = NULL` a single trace corresponding to
#'          the average trace of the whole profile is returned.
#' @param FUN A function to compute the average (default is `mean`)
#' @param ... Additional parameters for the FUN functions
#' @param track (`logical[1]`) Should the processing step be tracked? 
#' @return An object of the class GPR. When `w = NULL`, this function 
#'         returns a GPR object with a single trace corresponding to the 
#'         average trace of the whole radargram. When `w` is equal to a
#'         strictly positive interger this function returns a GPR object with
#'         a size identical to x where each trace corresponds to the function `FUN`
#'         applied to `w` neighboring traces centered on the considered trace.
#' @name wapply
#' @rdname wapply
#' @export
#' @concept processing
setGeneric("wapply", 
           function(x, w = NULL, FUN = mean, ...,
                    track = TRUE)
             standardGeneric("wapply"))

#' @rdname wapply
#' @export
setMethod("wapply", "GPR", function(x, w = NULL, FUN = mean, ...,
                                     track = TRUE){
  FUN <- match.fun(FUN)
  if(is.null(w)){
    xdata <- x@data
    x <- x[,1]
    x@data <- as.matrix(apply(xdata, 1, FUN, ...))
    x@z0 <- mean(x@z0)
    x@time <- mean(x@time)
    x@coord <- matrix(ncol = 0, nrow = 0)
    x@rec <- matrix(ncol = 0, nrow = 0)
    x@trans <- matrix(ncol = 0, nrow = 0)
  }else{
    x@data <- wapplyMat(x@data, width = w, by = 1, FUN = FUN, MARGIN = 1, ...)
  }
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
}
)

#' @rdname wapply
#' @export
setMethod("wapply", "numeric", function(x, w = NULL, FUN = mean, ...,
                                     track = TRUE){
  .wapply(x = x, width = w, by = 1, FUN = FUN, ...)
}
)

# wapply: A faster (but less functional) "rollapply" for vector setups

# April 23, 2013.
# By A.N. Spiess, senior scientist at the Department of Andrology at the 
# University Hospital Hamburg-Eppendorf.
# This is what turned out (wapply for "window apply").
# @export
# x = vector!
.wapply <- function(x=NULL, width = NULL, by = NULL, FUN = NULL, ...){
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width
  lenX <- length(x)
  SEQ1 <- seq(1, lenX - width + 1, by = by)
  SEQ2 <- lapply(SEQ1, function(x) x:(x + width - 1))
  
  OUT <- lapply(SEQ2, function(a) FUN(x[a], ...))
  OUT <- base::simplify2array(OUT, higher = TRUE)
  return(OUT)
}

# centered wapply
# @export
# x = vector!
wapplyC <- function(x=NULL, width = NULL, by = NULL, FUN = NULL, ...){
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width
  lenX <- length(x)
  # SEQ1 <- seq(1, lenX - width + 1, by = by)
  SEQ1 <- seq(-(width - 1)/2 + 1, lenX -(width - 1)/2, by = by)
#  SEQ2 <- lapply(SEQ1, function(x) x:(x + width - 1))
  SEQ2 <- lapply(SEQ1, function(x){ 
                          xnew <- x:(x + width - 1)
                          xnew <- xnew[xnew > 0]
                          xnew <- xnew[xnew <= lenX]})
  
  OUT <- lapply(SEQ2, function(a) FUN(x[a], ...))
  OUT <- base::simplify2array(OUT, higher = TRUE)
  return(OUT)
}


# x = matrix
# width uneven!! 3, 5, 7 or ...
# Wapply on the row of a matrix (windowed + CENTERED)
wapplyRowC <- function(x = NULL, width = NULL, by = NULL, FUN = NULL, ...){
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width
  lenX <- nrow(x)
  SEQ1 <- seq(-(width - 1)/2 + 1, lenX -(width - 1)/2, by = by)
  SEQ2 <- lapply(SEQ1, function(x){ 
                                   xnew <- x:(x + width - 1)
                                   xnew <- xnew[xnew > 0]
                                   xnew <- xnew[xnew <= lenX]})
  
  OUT <- lapply(SEQ2, function(a) FUN(x[a,,drop=FALSE], ...))
  OUT <- base::simplify2array(OUT, higher = TRUE)
  return(OUT)
}

# FIXME CHECK MARGIN
# windowing with centered window
#
# based on wapply and modified by Manu.
# centered moving window.
# return a matrix of the same dimension than x.
# some border effect at a distance < width/2 at the first and last col/row
# @export
wapplyMat <- function(x = NULL, width = NULL, by = NULL, FUN = NULL, 
                      MARGIN = 1, ...){
  #warning("MARGIN are corrected, use 1 instead of 2 and vice versa!!")
  FUN <- match.fun(FUN)
  width <- ifelse(width %% 2 == 0, width + 1, width)
  if (is.null(by)) by <- width
  lenX <- ifelse(MARGIN == 2, nrow(x), ncol(x))
  SEQ1 <- seq(-(width-1)/2 + 1, lenX -(width-1)/2, by = by)
  SEQ2 <- lapply(SEQ1, function(x){ 
      xnew <- x:(x + width - 1)
      xnew <- xnew[xnew > 0]
      xnew <- xnew[xnew <= lenX]
    })
  if(MARGIN == 1){
    OUT <- lapply(SEQ2, function(a) apply(x[, a, drop = FALSE], MARGIN, FUN, ...))
  }else if( MARGIN == 2) {
    OUT <- lapply(SEQ2, function(a) apply(x[a,, drop = FALSE], MARGIN, FUN, ...))
  }
  OUT <- base::simplify2array(OUT, higher = TRUE)
  if(MARGIN == 2){
    return(t(OUT))
  }else{
    return(OUT)
  }
}

# apply(x[, SEQ2[[10]], drop = FALSE], MARGIN, FUN)
