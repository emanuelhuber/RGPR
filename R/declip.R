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
#' @param lambda [\code{numeric(1)}] Positive value that add some noise to
#'               stabilize the matrix inversion used in the least-square 
#'               approach (could be useful if the matrix to invert is singular,
#'               use small value, e.g., 0.00001 of the signal amplitude).
#' @return [\code{GPR}]    
#' @name declip     
setGeneric("declip", function(x, 
                              xclip  = NULL, 
                              xrange = NULL, 
                              lambda = 1,
                              mu     = 0) 
  standardGeneric("declip"))

#' @rdname declip      
#' @export
setMethod("declip", "GPR", function(x, 
                                      xclip = NULL, 
                                      xrange = NULL, 
                                      lambda = 1,
                                      mu = 0){
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
  x@data <- sapply(seq_len(ncol(x)), FUNrslClip, a = x@data,  
                   amin = xclipmin, amax = xclipmax, xrange = xrange, 
                   lambda = lambda, mu = mu)
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

FUNrslClip <- function(i, a, amin, amax, range, lambda, mu){
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

# xclip = boolean matrix (TRUE for clipped values)
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
    # remove the clipped values at the very end of the signal
    x_rle <- rle(xclip)
    # if the last values of the signal are clipped
    nrm <- 0
    if(tail(x_rle$values, 1) == TRUE){
      # if these values are the only one that are clipped
      if(length(x_rle$lengths) == 2) return(x)
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
    A <- -Sc %*% (t(D) %*% D) %*% t(Sc) 
    B <- Sc %*% t(D) %*% D %*% t(S) %*% xs[!xclip]
    if(lambda > 0){
      Sm <- diag(1L, nrow = nxs, ncol = nxs, names = TRUE)
      Sp <- diag(1L, nrow = nxs, ncol = nxs, names = TRUE)
      Sm <- Sm[xclipmin, xclip, drop = FALSE]   # S : complement of S (zmin)
      Sp <- Sp[xclipmax, xclip, drop = FALSE]   # S : complement of S (zmax)
      A <- A + lambda * (t(Sm) %*% Sm + t(Sp) %*% Sp)
      B <- B +  lambda * ( t(Sm) %*% rep(xrange[1], sum(xclipmin)) + 
                             t(Sp) %*% rep(xrange[2], sum(xclipmax)))
    }
    Nc <- sum(xclip)
    if(mu > 0){
      A <- A + diag(x = mu, nrow = Nc, ncol = Nc)
    }
    
    x[i][xclip] <- solve(A , B)
    # x[i] <- xs
    
  }
  return(x)
}

# rLSClip <- function(x, xclipmin = NULL, xclipmax = NULL, xrange = NULL, lambda = 5){
#   xclip <- xclipmin | xclipmax
#   
#   if(any(xclip)){
#     test <- range(which(xclip))
#     D <- diff(test)
#     if(D <  30) D <- 30
#     testi <- test + round(c(-D/2, D/2))
#     i <- seq(from = testi[1], to = testi[2], by = 1L)
#     i <- i[i > 0 & i < length(x)]
#     x0 <- x
#     x <- x[i]
#     xclip <- xclip[i]
#     xclipmin <- xclipmin[i]
#     xclipmax <- xclipmax[i]
#     
#     
#     if(is.null(xrange)){
#       dx <- diff(range(x)) * 0.15
#       xrange <- c(min(x), max(x)) + c(- dx, + dx)
#     }
#     
#     N <- length(x)
#     
#     D <- matrix(0, nrow = N-3, ncol = N)
#     D[ col(D) == row(D)  ] <- 1
#     D[ col(D) == row(D) +1  ] <- -2
#     D[ col(D) == row(D) +2  ] <- 1
#     
#     S <- matrix(0L, N, N)
#     diag(S) <- 1L
#     Sc <- S
#     Sm <- S
#     Sp <- S
#     
#     S <- S[!xclip,, drop = FALSE]   # S : sampling matrix
#     Sc <- Sc[xclip,, drop = FALSE]   # S : complement of S (zmin & zmax)
#     Sm <- Sm[xclipmin, xclip, drop = FALSE]   # S : complement of S (zmin)
#     Sp <- Sp[xclipmax, xclip, drop = FALSE]   # S : complement of S (zmax)
#     
#     L = sum(xclip)                        # L : number of missing values
#     
#     x_lsq <- x
#     I_L <- matrix(0L,L,L)
#     # diag(I_L) <- 1L
#     # lambda <- 1
#     
#     
#     x_lsq[xclip] <- solve(-(Sc %*% (t(D) %*% D) %*% t(Sc) + 
#                               lambda * (t(Sm) %*% Sm + t(Sp) %*% Sp) ), 
#                           ( Sc %*% t(D) %*% D %*% t(S) %*% x[!xclip] -
#                               lambda * ( t(Sm) %*% rep(xrange[1], sum(xclipmin)) + 
#                                            t(Sp) %*% rep(xrange[2], sum(xclipmax))))
#     )
#     
#     # least-square without regularization for min/max
#     # x_lsq[xclip] <- solve(-(Sc %*% (t(D) %*% D) %*% t(Sc) + lambda*I_L ), 
#     #                       ( Sc %*% t(D) %*% D %*% t(S) %*% x[!xclip]))
#     
#     x0[i] <- x_lsq
#     # print(range(i))
#     return(x0)
#   }else{
#     return(x)
#   }
# }
