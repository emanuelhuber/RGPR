
setGeneric("declip", function(x, 
                                xclip = NULL, 
                                xrange = NULL, 
                                lambda = 1) 
  standardGeneric("declip"))

#' Declip the GPR signal
#' 
#' Use constrained least squares. Based on the code of Ivan Selesnick:
#'  "we minimize the energy of the the third derivative. This encourages the 
#'  filled in data to have the form of a parabola (second order polynomial), 
#'  because the third derivative of a parabola is zero.". A constraint on the
#'  declipped data range was added (i.e., constrained least squares) like in
#'  http://www.cs.cmu.edu/~robust/Papers/HarvillaStern15a.pdf
#' 
#' @param x [\code{GPR}]
#' @param xclip [\code{matrix(m,n)}] The clipped values, a matrix 
#'              with the Same dimension as \code{x}, with \code{1} for 
#'              the positively clipped values, \code{-1} for the negatively
#'              clipped values and \code{0} everywhere else. If 
#'              \code{xclip = NULL}, the clipped values are estimated with
#'              the function \code{\link{clippedValues}}.
#' @param xrange [\code{numeric(2)}] the desired range of the declipped values
#'               (for the negatively and positively clipped values) used as 
#'               a constraint whose strength depends on the value of 
#'               \code{lambda}. If \code{xrange = NULL}, the desired range is
#'               set equal to the clipped data range +/- 10\%.
#' @param lambda [\code{numeric(1)}] Positive value defining the strength of
#'               signal range constraint. The larger \code{lamda} is the larger
#'               the constraint on the signal range. 
#'               If \code{lambda = 0}, there is no constraint on the data range
#'               and \code{xrange} is not used.
#' @return [\code{GPR}]              
#' @export
setMethod("declip", "GPR", function(x, 
                                      xclip = NULL, 
                                      xrange = NULL, 
                                      lambda = 1){
  if(is.null(xclip)){
    #try to estimate it
    xclip <- clippedValues(x)
    xclip <- xclip@data
  }
  xclipmax <- xclip == 1
  xclipmin <- xclip == -1
  # if(is.null(xrange)) stop("'NULL' value for 'xrange' not allowed!")
  # x@data <- apply(x@data, 2, rLSClip, 
  #                 xclip  = xclip, 
  #                 xrange = xrange, 
  #                 lambda = lambda)
  if(is.null(xrange)){
    dx <- diff(range(x)) * 0.1
    xrange <- c(min(x), max(x)) + c(- dx, + dx)
  }else{
    sort(xrange)
  }
  xdata_old <- x@data
  # if(is.null(xrange)) stop("'NULL' value for 'xrange' not allowed!")
  x@data <- sapply(seq_len(ncol(x)), function(i, a, amin, amax){
    rLSClip(a[, i], 
            xclipmin = amin[,i], 
            xclipmax = amax[,i], 
            xrange = xrange, 
            lambda = lambda)
  },
  a = x@data,  amin = xclipmin, amax = xclipmax)
  proc(x) <- getArgs()
  testmax <- sum(x@data[xclipmax] < xdata_old[xclipmax])
  testmin <- sum(x@data[xclipmin] > xdata_old[xclipmin])
  if(testmax > 0){
    message(testmax, " declipped value(s) are smaller than upper clip value")
  }
  if(testmin > 0){
    message(testmin, " declipped value(s) are larger than lower clip value")
  }
  return(x)
})

#--------------------- LEAST-SQUARES DECLIPPING -------------------------------#
# based on the code of Ivan Selesnick:
#  "we minimize
#   the energy of the the third derivative. This encourages the filled in data 
#   to have the form of a parabola (second order polynomial), because the third
#   derivative of a parabola is zero."
# + constraint (constrained least-square) like in
# http://www.cs.cmu.edu/~robust/Papers/HarvillaStern15a.pdf

# xclip = boolean matrix (TRUE for clipped values)
# xrange = range of the values of the unclipped signal
# lambda = ...
rLSClip <- function(x, xclipmin = NULL, xclipmax = NULL, xrange = NULL, lambda = 5){
  xclip <- xclipmin | xclipmax
  
  if(any(xclip)){
    test <- range(which(xclip))
    D <- diff(test)
    if(D <  30) D <- 30
    testi <- test + round(c(-D/2, D/2))
    i <- seq(from = testi[1], to = testi[2], by = 1L)
    i <- i[i > 0 & i < length(x)]
    x0 <- x
    x <- x[i]
    xclip <- xclip[i]
    xclipmin <- xclipmin[i]
    xclipmax <- xclipmax[i]
    
    
    if(is.null(xrange)){
      dx <- diff(range(x)) * 0.15
      xrange <- c(min(x), max(x)) + c(- dx, + dx)
    }
    
    N <- length(x)
    
    D <- matrix(0, nrow = N-3, ncol = N)
    D[ col(D) == row(D)  ] <- 1
    D[ col(D) == row(D) +1  ] <- -2
    D[ col(D) == row(D) +2  ] <- 1
    
    S <- matrix(0L, N, N)
    diag(S) <- 1L
    Sc <- S
    Sm <- S
    Sp <- S
    
    S <- S[!xclip,, drop = FALSE]   # S : sampling matrix
    Sc <- Sc[xclip,, drop = FALSE]   # S : complement of S (zmin & zmax)
    Sm <- Sm[xclipmin, xclip, drop = FALSE]   # S : complement of S (zmin)
    Sp <- Sp[xclipmax, xclip, drop = FALSE]   # S : complement of S (zmax)
    
    L = sum(xclip)                        # L : number of missing values
    
    x_lsq <- x
    I_L <- matrix(0L,L,L)
    # diag(I_L) <- 1L
    # lambda <- 1
    
    
    x_lsq[xclip] <- solve(-(Sc %*% (t(D) %*% D) %*% t(Sc) + 
                              lambda * (t(Sm) %*% Sm + t(Sp) %*% Sp) ), 
                          ( Sc %*% t(D) %*% D %*% t(S) %*% x[!xclip] -
                              lambda * ( t(Sm) %*% rep(xrange[1], sum(xclipmin)) + 
                                           t(Sp) %*% rep(xrange[2], sum(xclipmax))))
    )
    
    # least-square without regularization for min/max
    # x_lsq[xclip] <- solve(-(Sc %*% (t(D) %*% D) %*% t(Sc) + lambda*I_L ), 
    #                       ( Sc %*% t(D) %*% D %*% t(S) %*% x[!xclip]))
    
    x0[i] <- x_lsq
    # print(range(i))
    return(x0)
  }else{
    return(x)
  }
}
