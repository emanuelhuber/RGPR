
#------------------------------- DEPRECATED -----------------------------------#
setGenericVerif("backgroundSub", function(x, width = 21, trim = 0.2,
                                          s = 1, eps = 1, itmax = 5,
                                          track = TRUE)
  standardGeneric("backgroundSub"))


#' @name backgroundSub
#' @rdname rmBackgroundMatrix
#' @export
setMethod("backgroundSub", "GPR", function(x, width = 21, trim = 0.2,
                                           s = 1, eps = 1, itmax = 5,
                                           track = TRUE){
  message("Soon deprecated. Use 'rmBackgroundMatrix()' instead of ",
"         'backgroundSub()'.")
  rmBackgroundMatrix(x, 
                     width = width, 
                     trim  = trim,
                     s     = s,
                     eps   = eps,
                     itmax = itmax,
                     track = track)

})
#------------------------------- DEPRECATED -----------------------------------#


setGenericVerif("rmBackgroundMatrix", function(x, width = 21, trim = 0.2,
                                          s = 1, eps = 1, itmax = 5,
                                          track = TRUE)
  standardGeneric("rmBackgroundMatrix"))


#' Background matrix substraction
#'  
#' See  Rashed and Harbi (2014) Background matrix subtraction (BMS): 
#' A novel background removal algorithm for GPR data
#' doi: 10.1016/j.jappgeo.2014.04.022
#' Takes time to compute!!
#' @param x An object of the class GPR
#' @param width A length-one integer vector equal to the window length of the 
#'          average window (an odd number).
#' @param trim  A length-one numeric vector: the fraction (0 to 0.5) of 
#'              observations to be trimmed from each end of x before 
#'              the mean is computed. Values of trim outside that range 
#'              are taken as the nearest endpoint 
#'              (argument of the function \code{mean}).
#' @param s A length-one positiv numeric vector controling the strength
#'          of the weighting scheme
#' @param eps A length-one positiv numeric vector defining the minimum
#'            desired residual value. If \code{NULL}, \code{itmax} defines
#'            the iteration number.
#' @param itmax A length-one positiv integer vector defining the maximum
#'            iteration number. If \code{NULL}, \code{itmax} defines
#'            the iteration number.
#' @return An object of the class GPR with substracted background.
#' @name rmBackgroundMatrix
#' @rdname rmBackgroundMatrix
#' @export
setMethod("rmBackgroundMatrix", "GPR", function(x, width = 21, trim = 0.2,
                                           s = 1, eps = 1, itmax = 5,
                                           track = TRUE){
  if(is.null(width)){
    stop("Set a value to 'width'")
  }
  if(width > ncol(x)){
    stop("'width' must be smaller than the column number of x") 
  }
  if(is.null(itmax) && is.null(eps)){
    stop("You cannot set both 'eps' and 'imax' equal to 'NULL'!")
  } 
  if(is.null(itmax)) itmax <- Inf
  if(is.null(eps)) eps <- 0
  
  if( (width %% 2) == 0){
    width <- width + 1
  }
  y0 <- as.matrix(x[, c(((width - 1)/2 + 1):2, 
                        seq_along(x), 
                        ncol(x) - 1:((width - 1)/2))])
  test <- c()
  i <- 0
  
  d <- eps + 1
  while(i < itmax || d <= eps){
    i <- i + 1
    y <- wapplyMat(y0, width = width, by = 1, FUN = .BMSfx, 
                   MARGIN = 1,  s = s, trim = trim)
    message("* ", appendLF = FALSE)
    d <- sum(((y - y0)^2), na.rm =TRUE)/prod(dim(y))
    test <- c(test, d)
    y0 <- y
  }
  message("Residuals: ", paste(round(test, 3), collapse = " "))
  x <- x - y0[, - c(1:((width-1)/2), 
                    (width-1)/2  + ncol(x) + 1:((width-1)/2))]
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
}
) 

.BMSfx <- function(x, s = 1, trim = 0.2){
  x_trimMean <- mean(x, trim = trim, na.rm = TRUE)
  x_hat <- x[sign(x) == sign(x_trimMean)]
  w <- 1/(sqrt((x_hat - x_trimMean)^2))^s 
  w_norm <- length(x_hat) * w/sum(w, na.rm = TRUE)
  return(mean(w_norm * x_hat, na.rm = TRUE))
}  