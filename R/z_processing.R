.medianFilter1D <- function(a,w){
  b <- a  # <- matrix(0, 364,364)
  for(i in (w+1):(length(a)-w-1)){
    xm <- a[i+(-w:w)]
    b[i] <- xm[order(xm)[w+1]]
  }
  return(b)
}

# run med mean mad
.runmmmMat <- function(x, w, type = c("runmed", "runmean", "runmad", "hampel")){
  type <- match.arg(type, c("runmed", "runmean", "runmad", "hampel"))
  if( (w %% 2) == 0 )  w <- w + 1 
  xdata <- matrix(0, nrow = nrow(x) + 2*w , ncol = ncol(x) )
  xdata[1:nrow(x) + w, ] <- x
  if(type == "runmed"){
    xdata <- apply(xdata, 2, stats::runmed, k = w)
  }else if(type == "runmean"){
    runmean <- function(x, k = 5){stats::filter(x, rep(1 / k, k), sides = 2)}
    xdata <- apply(xdata, 2, runmean, k = w)
  }
  else if(type == "hampel"){
    xdata <- apply(xdata, 2, rollapplyHampel, w ,  .fHampel)
  }
  # x <- x - xdata[1:nrow(x) + w, ]
  return(xdata[1:nrow(x) + w, ])
}


rollapplyHampel <- function(x, w, FUN){
  k <- trunc((w - 1)/ 2)
  locs <- (k + 1):(length(x) - k)
  num <- vapply(
    locs, 
    function(i) FUN(x[(i - k):(i + k)], x[i]),
    numeric(1)
  )
  x[locs] <- num
  return(x)
}
.fHampel <- function(x, y){
  x0 <- median(x)
  S0 <- 1.4826 * median(abs(x - x0))
  if(abs(y - x0) > 3 * S0) return(x0)
  # y[test] <- x0[test]
  return(y)
}




#' Normal-score transformation
#'
#' Transforms a numeric vector to an approximately Gaussian distribution
#' using empirical ranks. Equal input values are assigned equal normal
#' scores through average ranks.
#'
#' The transformation can preserve the mean and standard deviation of the
#' original finite observations. An inverse transformation can be performed
#' when a transformation table is supplied.
#'
#' @param x A numeric vector. For a forward transformation, `x` contains the
#' values to transform. For an inverse transformation, `x` contains the
#' normal scores to back-transform.
#'
#' @param inverse Logical scalar. If `FALSE`, perform the forward normal-score
#' transformation. If `TRUE`, back-transform normal scores using `tbl`.
#'
#' @param tbl An optional two-column numeric matrix or data frame containing
#' the transformation table. The first column must contain original values
#' and the second column must contain corresponding normal scores.
#'
#' A transformation table is required when `inverse = TRUE`. For the forward
#' transformation, `tbl` is currently not used because scores are derived
#' directly from the empirical ranks of `x`.
#'
#' @return A numeric vector with the same length as `x`.
#'
#' For the forward transformation, non-finite input values are returned as
#' `NA_real_`. A finite constant vector is returned as zeros because its
#' standard deviation is zero and a normal-score transformation is not
#' uniquely defined.
#'
#' For the inverse transformation, values are obtained by linear
#' interpolation of the supplied transformation table. Values outside the
#' table range are assigned the nearest endpoint value.
#'
#' @details
#' For a vector containing \eqn{n} finite observations, plotting positions
#' are computed as
#'
#' \deqn{p_i = \frac{r_i - 0.5}{n},}
#'
#' where \eqn{r_i} is the average rank of observation \eqn{i}. Normal scores
#' are then calculated as
#'
#' \deqn{z_i = \Phi^{-1}(p_i),}
#'
#' where \eqn{\Phi^{-1}} is the standard normal quantile function.
#'
#' Average ranks ensure that tied input values obtain identical normal
#' scores. Plotting positions are strictly between zero and one, which
#' prevents infinite values from [stats::qnorm()].
#'
#' The resulting standard normal scores are rescaled to the mean and standard
#' deviation of the original finite observations.
#'
#' During inverse transformation, duplicate normal scores in `tbl` are
#' collapsed by averaging their corresponding original values. This ensures
#' that the interpolation coordinates are unique.
#'
#' @seealso [base::rank()], [stats::qnorm()], [stats::approxfun()]
#'
#' @examples
#' \dontrun{
#' x <- c(1, 1, 1, 2, 3, 5, 8)
#'
#' # Tied values receive identical transformed values
#' z <- .nScoreTrans(x)
#' z
#'
#' # Missing values are retained as missing
#' .nScoreTrans(c(1, 2, NA, 4))
#'
#' # A constant vector is transformed to zero
#' .nScoreTrans(rep(5, 10))
#'
#' # Example of an inverse transformation table
#' tbl <- data.frame(
#' value = sort(unique(x)),
#' nscore = .nScoreTrans(sort(unique(x)))
#' )
#'
#' .nScoreTrans(
#' tbl$nscore,
#' inverse = TRUE,
#' tbl = tbl
#' )
#'}
#' @keywords internal
#' @noRd
.nScoreTrans <- function(x, inverse = FALSE, tbl = NULL) {
  if (isTRUE(inverse)) {
    if (is.null(tbl)) {
      stop("tbl must be provided for the inverse transformation")
    }
    tbl <- tbl[complete.cases(tbl), , drop = FALSE]
    tbl <- tbl[order(tbl[, 2]), , drop = FALSE]
    # Required because approxfun() also expects unique interpolation values
    tbl <- aggregate(tbl[, 1],
                     by = list(nscore = tbl[, 2]),
                     FUN = mean)
    names(tbl) <- c("nscore", "value")
    back.xf <- approxfun(
      x = tbl$nscore,
      y = tbl$value,
      rule = 2,
      ties = mean
    )
    return(back.xf(x))
  }
  ok <- is.finite(x)
  y_n <- rep(NA_real_, length(x))
  n <- sum(ok)
  if (n == 0L) {
    return(y_n)
  }
  # A constant trace cannot meaningfully be transformed.
  if (n == 1L || sd(x[ok]) < .Machine$double.eps^0.5) {
    y_n[ok] <- 0
    return(y_n)
  }
  # Equal amplitudes receive equal ranks and therefore equal scores.
  r <- rank(x[ok], ties.method = "average")
  # Plotting positions strictly between 0 and 1
  p <- (r - 0.5) / n
  z <- qnorm(p)
  # Preserve the original trace mean and standard deviation.
  y_n[ok] <- z * sd(x[ok]) + mean(x[ok])
  y_n
}


# x = output of scale(...)
# y = object to back-transform, same dimension as x
# check:
# x <- scale(x0, center = TRUE, scale = TRUE)
# x0 <- unscale(x, x)

#' Unscale
#'
#' Back-transform/unscale from \code{scale}
#' @param x (`numeric[n]`) A numerical vector
#' @param y (`numeric[n]`) A numerical vector
#' @return (`numeric[n]`)
#' @export
unscale <- function(x, y){
  xCenter <- attr(x, 'scaled:center')
  xScale <- attr(x, 'scaled:scale')
  if(is.null(xCenter)) xCenter <- rep(0, ncol(x))
  if(is.null(xScale))  xScale <- rep(1, ncol(x))
  ynew <- scale(y, center = -xCenter/xScale, scale = 1/xScale)
  attr(ynew,'scaled:center') <- NULL
  attr(ynew,'scaled:scale') <- NULL
  return(ynew)
}