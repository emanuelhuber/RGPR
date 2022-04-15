



# wapply: A faster (but less functional) "rollapply" for vector setups

# April 23, 2013.
# By A.N. Spiess, senior scientist at the Department of Andrology at the 
# University Hospital Hamburg-Eppendorf.
# This is what turned out (wapply for "window apply").
# @export
# x = vector!
wapply <- function(x=NULL, width = NULL, by = NULL, FUN = NULL, ...){
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
  warning("MARGIN are corrected, use 1 instead of 2 and vice versa!!")
  FUN <- match.fun(FUN)
  width <- ifelse(width %% 2 == 0, width + 1, width)
  if (is.null(by)) by <- width
  lenX <- ifelse(MARGIN == 1, nrow(x), ncol(x))
  SEQ1 <- seq(-(width-1)/2 + 1, lenX -(width-1)/2, by = by)
  SEQ2 <- lapply(SEQ1, function(x){ xnew <- x:(x + width - 1)
  xnew <- xnew[xnew > 0]
  xnew <- xnew[xnew <= lenX]})
  if(MARGIN == 2){
    OUT <- lapply(SEQ2, function(a) apply(x[, a, drop = FALSE], MARGIN, FUN, ...))
  }else if( MARGIN == 1) {
    OUT <- lapply(SEQ2, function(a) apply(x[a,, drop = FALSE], MARGIN, FUN, ...))
  }
  OUT <- base::simplify2array(OUT, higher = TRUE)
  if(MARGIN == 1){
    return(t(OUT))
  }else{
    return(OUT)
  }
}
