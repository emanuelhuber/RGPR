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





# histogram transformation to normal distributed data with same mean and sd
# https://msu.edu/~ashton/temp/nscore.R
.nScoreTrans <- function(x, inverse = FALSE, tbl = NULL){
  if(isTRUE(inverse)){
    if(is.null(tbl)){
      stop("tbl must be provided")
    }
    min_x <- min(tbl[,1])
    max_x <- max(tbl[,1])
    min_sc <- min(x)
    max_sc <- max(x)
    x1 <- c(min_x, tbl[,1], max_x)
    nsc <- c(min_sc, tbl[,2], max_sc)
    
    back.xf <- approxfun(nsc,x1) # Develop the back transform function
    val <- back.xf(x)
    return(val)
  }else{
    #     fx <- ecdf(x)
    #     x1 <- head(knots(fx), -1)
    #     xCDF <- fx(x1)
    #     y <- qnorm(xCDF, mean(x), sd = sd(x))
    if(!is.null(tbl)){
      x1 <- tbl[,1]
      y  <- tbl[,2]
    }else{
      x1 <- sort(x)
      y <- sort(qqnorm(x, plot.it = FALSE)$x)
      tbl <- data.frame(x = x1, nscore = y)
    }
    y_n <- approx(x1, y, xout = x, rule = 2)$y
    y_n <- y_n * sd(x) + mean(x)
    #return(list(x = y_n, table = tbl))
    return(y_n)
  }
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