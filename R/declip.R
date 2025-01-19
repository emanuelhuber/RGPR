
#' @rdname declip   
#' @name declip     
setGeneric("declip", function(obj, 
                              drange = NULL, 
                              lambda = 1,
                              mu     = 0,
                              objclip  = NULL) 
  standardGeneric("declip"))

#' Declip the GPR signal
#' 
#' Use constrained least squares. Based on the code of Ivan Selesnick:
#' we minimize the energy of the the third derivative. This encourages the 
#' filled in data to have the form of a parabola (second order polynomial), 
#' because the third derivative of a parabola is zero.". 
#' 
#' M. J. Harvilla and R. M. Stern, "Efficient audio declipping using regularized least squares," 2015 IEEE International Conference on Acoustics, Speech and Signal Processing (ICASSP), South Brisbane, QLD, Australia, 2015, pp. 221-225, doi: 10.1109/ICASSP.2015.7177964.
#' 
#' @param obj (`GPR`)
#' @param drange (`numeric[2]`) the desired range of the declipped values
#'               (for the negatively and positively clipped values) used as 
#'               a soft constraint whose strength depends on the value of 
#'               `lambda`. If `drange = NULL`, the desired range is
#'               set equal to the clipped data range +/- 10 percent.
#' @param lambda (`numeric[1]`) Positive value defining the strength of
#'               signal range constraint. The larger `lamda` is the larger
#'               the constraint on the signal range. 
#'               If `lambda = 0`, there is no constraint on the data range
#'               and `drange` is not used.
#' @param mu     (`numeric[1]`) Positive value that add some noise to
#'               stabilize the matrix inversion used in the least-square 
#'               approach (could be useful if the matrix to invert is singular,
#'               use small value, e.g., 0.00001 of the signal amplitude).
#' @param objclip (`matrix[m,n]`) The clipped values, a matrix 
#'              with the Same dimension as `obj`, with `1` for 
#'              the positively clipped values, `-1` for the negatively
#'              clipped values and `0` everywhere else. If 
#'              `objclip = NULL`, the clipped values are estimated with
#'              the function [clippedData()].
#' @return (`GPR`)    
#' @rdname declip      
#' @export
#' @concept signal processing
setMethod("declip", "GPR", function(obj, 
                                    drange = NULL, 
                                    lambda = 1,
                                    mu = 0,
                                    objclip = NULL){
  if(is.null(objclip)){
    #try to estimate it
    objclip <- clippedData(obj)
  }
  if(inherits(objclip, "GPR")) objclip <- objclip@data
  xclipmax <- objclip == 1
  xclipmin <- objclip == -1
  if(is.null(drange)){
    dx <- diff(range(obj)) * 0.1
    drange <- c(min(obj), max(obj)) + c(- dx, + dx)
  }else{
    sort(drange)
  }
  xdata_old <- obj@data
  # if(is.null(drange)) stop("'NULL' value for 'drange' not allowed!")
  obj@data <- sapply(seq_len(ncol(obj)), FUNrslClip, a = obj@data,  
                   amin = xclipmin, amax = xclipmax, xrange = drange, 
                   lambda = lambda, mu = mu)
  proc(obj) <- getArgs()
  testmax <- sum(obj@data[xclipmax] < xdata_old[xclipmax])
  testmin <- sum(obj@data[xclipmin] > xdata_old[xclipmin])
  if(testmax > 0){
    message(testmax, " declipped value(s) are smaller than upper clip value")
  }
  if(testmin > 0){
    message(testmin, " declipped value(s) are larger than lower clip value")
  }
  return(obj)
})

FUNrslClip <- function(i, a, amin, amax, xrange, lambda, mu){
  rLSClip(a[, i], 
          xclipmin = amin[,i], 
          xclipmax = amax[,i], 
          xrange = xrange, 
          lambda = lambda,
          mu = mu)
}

#--------------------- LEAST-SQUARES DECLIPPING -------------------------------#
# based on the code of Ivan Selesnick:
#  "we minimize
#   the energy of the the third derivative. This encourages the filled in data 
#   to have the form of a parabola (second order polynomial), because the third
#   derivative of a parabola is zero."
# + constraint (constrained least-square) like in
# http://www.cs.cmu.edu/~robust/Papers/HarvillaStern15a.pdf

# objclip = boolean matrix (TRUE for clipped values)
# xrange = range of the values of the unclipped signal
# lambda = ...

rLSClip <- function(x, xclipmin = NULL, xclipmax = NULL, xrange = NULL, 
                    lambda = 5, mu = 0){
  xclip <- xclipmin | xclipmax
  
  if(any(xclip)){
    nx <- length(x)
    # -------------------------- SHORTEN TO SPEED UP ------------------------- #
    # take a short part of the signal around the clipped values
    # to speed up the computations
    # remove the clipped values at the very ends of the signal
    i <- .decliprange(xclip, nx)
    
    # if(length(i) == 0) return(x)
    # x0 <- x
    # shorten x and other variables
    xs <- x[i]
    xclip <- xclip[i]
    xclipmin <- xclipmin[i]
    xclipmax <- xclipmax[i]
    
    nxs <- length(xs)
    
    D <- matrix(0, nrow = nxs - 3, ncol = nxs)
    D[ col(D) == row(D)  ] <- 1
    D[ col(D) == row(D) +1  ] <- -2
    D[ col(D) == row(D) +2  ] <- 1
    
    S  <- diag(1L, nrow = nxs, ncol = nxs, names = TRUE)
    Sc <- diag(1L, nrow = nxs, ncol = nxs, names = TRUE)
    S  <- S[!xclip,, drop = FALSE]   # S : sampling matrix
    Sc <- Sc[xclip,, drop = FALSE]   # S : complement of S (zmin & zmax)
    # x_lsq <- x
    A <- Sc %*% (t(D) %*% D) %*% t(Sc) 
    B <- Sc %*% t(D) %*% D %*% t(S) %*% xs[!xclip]
    if(lambda > 0){
      Sm <- diag(1L, nrow = nxs, ncol = nxs, names = TRUE)
      Sp <- diag(1L, nrow = nxs, ncol = nxs, names = TRUE)
      Sm <- Sm[xclipmin, xclip, drop = FALSE]   # S : complement of S (zmin)
      Sp <- Sp[xclipmax, xclip, drop = FALSE]   # S : complement of S (zmax)
      A <- A + lambda * (t(Sm) %*% Sm + t(Sp) %*% Sp)
      B <- B -  lambda * ( t(Sm) %*% rep(xrange[1], sum(xclipmin)) + 
                             t(Sp) %*% rep(xrange[2], sum(xclipmax)))
    }
    Nc <- sum(xclip)
    if(mu > 0){
      A <- A + diag(x = mu, nrow = Nc, ncol = Nc)
    }
    
    # y <- x
    # y[i][xclip] <- -solve(A , B)
    # 
    # plot(y, type = "l")
    # lines(x, col = "red")
    
    
    x[i][xclip] <- -solve(A , B)
    # plot(x[i][xclip])
    # x[i] <- xs
    
  }
  return(x)
}


# remove the clipped values at the very ends of the signal
.decliprange <- function(xclip, nx){
  x_rle <- rle(xclip)
  nrm <- 0
  # if the last values of the signal are clipped
  if(tail(x_rle$values, 1) == TRUE){
    # if these values are the only one that are clipped
    #if(length(x_rle$lengths) == 2) return(x)
    # else: remove
    nrm <- tail(x_rle$lengths, 1)
    xclip <- xclip[1:(nx - nrm)]
  }
  test <- range(which(xclip))
  D <- diff(test)
  if(D <  30) D <- 30
  testi <- test + round(c(-D/2, D/2))
  i <- seq(from = testi[1], to = testi[2], by = 1L)
  i <- i[i > 0 & i <= (nx - nrm) ]
  return(i)
}
