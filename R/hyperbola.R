

#' Fit hyperbola to data points
#' 
#' Fit hyperbola to data points \eqn{\mathbf{x}, \mathbf{t}}, where
#' \eqn{\mathbf{x} = (x_1, x_2, \ldots)} is the horizontal point position
#' and \eqn{\mathbf{t} = (t_1, t_2, \ldots)} is the vertical (time) point 
#' position.
#' The parameters \eqn{a} and \eqn{b} in 
#' \eqn{\frac{\mathbf{t}^2}{4} = a \mathbf{x}^2 + b \mathbf{x} + c} are 
#' estimated with the \code{lm} function.
#' The position of the vertex is given by:
#' \deqn{x_0 = -\frac{b}{2} v_{RMS}}
#' and
#' \deqn{t_0 = 2 \sqrt{c - (\frac{x_0}{v_{RMS}})^2}}
#' where \eqn{v_{RMS} = \sqrt{\frac{1}{a}}}
#' 
#' @param x [\code{numeric}|\code{list}] Either the x-values of the data
#'          points or a list with elements x and t (in this case, leave 
#'          \code{y = NULL})
#' @param y [\code{numeric}] The time values of the data (if \code{x} is not
#'          a list).
#' 
#' @return [\code{list}] A list with class \code{hyperbola} containing the 
#' following elements:
#' \describe{
#'   \item{reg}{The regression output of the function \code{lm}}
#'   \item{vrms}{The estimated root-mean-square velocity}
#'   \item{x0}{The horizontal position of the hyperbola vertex}
#'   \item{t0}{The vertical position  (time) of the hyperbola vertex}
#'   \item{x}{The input point positions x}
#'   \item{y}{The input point positions y}
#' }
#' @seealso \code{\link{hyperbolaPlot}}, \code{\link{hyperbolaSim}}
#' @examples 
#' data("frenkeLine00")
#' x <- frenkeLine00
#' x <- estimateTime0(x, w = 5, method = "MER", FUN = mean)
#' x <- time0Cor(x, method = "pchip")
#' x <- gainAGC(fFilter(x, f = c(180, 250), type = "low"), w = 20)
#' plot(x)
#' 
#' # xy <- locator(type = "l")
#' xy <- list( x = c( 11.8,  15.0,  17.7, 20.3, 24.4,  27.4,  30.9,  35.2),
#'             y = c(142.2, 119.8, 107.7, 99.5, 97.5, 105.6, 120.9, 138.1))
#' hyp <- hyperbolaFit(xy)
#' hyperbolaPlot(hyp, x = seq(5, 50, by = 0.01), col = "green", 
#'               lwd = 4, ann = TRUE)
#' points(xy, pch = 20, col = "blue")
#' @name hyperbolaFit
#' @rdname hyperbolaFit
#' @export
hyperbolaFit <- function(x, y = NULL){
  if(is.null(y)){
    if(length(x) != 2) stop("x must a list of length = 2")
    y <- x[[2]]
    x <- x[[1]]
  }
  y2 <- y^2/4
  x2 <- x^2
  fit1 <- lm(y2 ~ x2 + x)
  coef1 <- unname(fit1$coefficient)
  vrms <- sqrt(1 / coef1[2])
  x0 <- -coef1[3] * vrms^2 / 2
  t0 <- 2 * sqrt( coef1[1] - x0^2 / vrms^2 )
  z0 <- vrms * t0 / 2
  hyp <- list(reg = fit1, 
              vrms = vrms,
              x0 = x0,
              t0 = t0,
              z0 = z0,
              x = x,
              y = y)
  class(hyp) <- "hyperbola"
  return(hyp)
}

# An object of the class \code{hyperbola} 
#          or a list with 3 elements named \code{x_0}, \code{t_0}, \code{vrms}.
#' Simulate hyperbola
#' 
#' Return the time values of the hyperbola as a function of the position
#' values \code{x}.
#'
#' @param x [\code{numeric}] 
#'          The horitzontal positions at which to compute the hyperbola values.
#' @param hyp [\code{list}|\code{class hyperbola}] 
#'            Either a list with elements \code{x0}, \code{t0}, and \code{vrms}
#'            corresponding to the coordinates of the hyperbola vertex and
#'            the root-mean-square velocity, or a list of class 
#'            \code{hyperbola} (i.e., the output of the function 
#'            \code{\link{hyperbolaFit}}). 
#'          \code{y = NULL})
#' @param [\code{numeric}]
#'        The time values of the hyperbola.
#' @seealso \code{\link{hyperbolaSim}}, \code{\link{hyperbolaPlot}}
#' @examples 
#' 
#' xy <- list( x = c( 11.8,  15.0,  17.7, 20.3, 24.4,  27.4,  30.9,  35.2),
#'             y = c(142.2, 119.8, 107.7, 99.5, 97.5, 105.6, 120.9, 138.1))
#' hyp <- hyperbolaFit(xy)
#' 
#' x <- seq(10, 40, by = 0.1)
#' y <- hyperbolaSim(x, hyp) 
#' plot(x, y, type = "l", ylim = rev(range(y)))   
#' 
#' hyp2 <- list(x0 = hyp$x0, t0 = hyp$t0, vrms = hyp$vrms)  
#' y <- hyperbolaSim(x, hyp) 
#' plot(x, y, type = "l", ylim = rev(range(y)))  
#' @name hyperbolaSim
#' @rdname hyperbolaSim
#' @export                  
hyperbolaSim <- function(x, hyp){
  if(class(hyp) == "hyperbola"){
    a  <- hyp$reg$coefficients[2]
    b  <- hyp$reg$coefficients[3]
    cc <- hyp$reg$coefficients[1]
    if(is.null(x)){
      xrge <- (range(hyp$x))
      D <- abs(diff(xrge)) * perc
      x <- seq(xrge[1] - D, to = xrge[2] + D, length.out = n)
    } 
    y <- 2 * sqrt( a*x^2 + b*x + cc ) 
  }else{
    if( !(all(names(hyp) %in% c("x0", "t0", "vrms"))) ){
      stop("the names of 'hyp' must be 'x0', 't0' and 'vrms'")
    }
    hyp$z0 <- hyp$vrms * hyp$t0/2   # we need z0 later!
    y <- 2/hyp$vrms * sqrt( (x - hyp$x0)^2 + hyp$z0^2 )
  }
  return(y)
}


#' Plot a hyperbola
#' 
#' Plot a hyperbola
#' 
#' If \code{x = NULL} and \code{add = TRUE}, the hyperbola is per default
#' displayed over the entire plot. You can define the extent of the hyperbola
#' by setting \code{xlim} as in the function \code{\link[graphics]{plot}}.
#' If \code{add = FALSE} \code{x} must be defined otherwise
#' an error is raised. 
#' 
#' @param hyp [\code{list}|\code{class hyperbola}] 
#'            Either a list with elements \code{x0}, \code{t0}, and \code{vrms}
#'            corresponding to the coordinates of the hyperbola vertex and
#'            the root-mean-square velocity, or a list of class 
#'            \code{hyperbola} (i.e., the output of the function 
#'            \code{\link{hyperbolaFit}}). 
#'          \code{y = NULL})
#' @param x [\code{numeric}] 
#'          The horitzontal positions at which to plot the hyperbola (optional,
#'          see details).
#' @param add [\code{logical(1)}]
#'            If \code{TRUE} (defaults), add the hyperbola to current 
#'            plot. If \code{FALSE}, display the hyperbola in a new plot.
#'            In this case you must specify \code{x}.
#' @param ann [\code{logical(1)}]
#'            If \code{TRUE}, add the following annotation above the 
#'            hyperbola vertex: root-mean-square velocity and vertex depth.
#' @param ann.pos [\code{numeric(1)}]
#'                A position specifier for the annotation text. 
#'                See argument \code{pos}
#'                of the function \code{\link[graphics]{text}}.
#' @param ann.offset [\code{numeric(1)}]
#'                   when \code{pos} is specified, this value controls the 
#'                   distance ('offset') of the annotation text label from the 
#'                   specified coordinate in fractions of a character width. 
#'                   See argument \code{offset}
#'                   of the function \code{\link[graphics]{text}}.                
#' @param ann.col [\code{character(1)}]
#'                Color of the annotation text. 
#'                See argument \code{col}
#'                of the function \code{\link[graphics]{text}}.      
#' @param ann.font [\code{integer(1)}]
#'                 An integer which specifies which font to use for text.
#'                 See argument \code{font}
#'                 of the function \code{\link[graphics]{par}}.
#' @param ann.font [\code{integer(1)}]
#'                 Number of points used to plot the hyperbola when
#'                 \code{x} is not specified.
#' @param ... Additional arguments passed to 
#'            \code{\link[graphics]{lines}} (if \code{add = TRUE}) or to
#'            \code{\link[graphics]{plot}} (if \code{add = FALSE})    .         
#' @seealso \code{\link{hyperbolaFit}}, \code{\link{hyperbolaSim}}
#' @examples 
#' data("frenkeLine00")
#' x <- frenkeLine00
#' x <- estimateTime0(x, w = 5, method = "MER", FUN = mean)
#' x <- time0Cor(x, method = "pchip")
#' x <- gainAGC(fFilter(x, f = c(180, 250), type = "low"), w = 20)
#' plot(x)
#' 
#' # xy <- locator(type = "l")
#' xy <- list( x = c( 11.8,  15.0,  17.7, 20.3, 24.4,  27.4,  30.9,  35.2),
#'             y = c(142.2, 119.8, 107.7, 99.5, 97.5, 105.6, 120.9, 138.1))
#' hyp <- hyperbolaFit(xy)
#' hyperbolaPlot(hyp, x = seq(5, 50, by = 0.01), col = "green", 
#'               lwd = 4, ann = TRUE)
#' points(xy, pch = 20, col = "blue")
#' 
#' plot(x)
#' hyp2 <- list(x0 = hyp$x0, t0 = hyp$t0, vrms = hyp$vrms)
#' hyperbolaPlot(hyp2, col = "blue", lwd = 2, ann = TRUE, xlim = c(10, 40))
#' points(hyp$x0, hyp$t0, pch = 20, col = "red", cex = 1.3)
#' 
#' # with 'add = FALSE'
#' plot(x)
#' hyperbolaPlot(hyp, x = seq(10, 40, by = 0.1), col = "red", 
#' lwd = 2,  add = FALSE, type = "l")
#' hyperbolaPlot(hyp, x = seq(10, 40, by = 0.1), col = "red", 
#'              lwd = 2,  add = FALSE, type = "l", ylim = c(175, 95))
#' 
#' @name hyperbolaPlot
#' @rdname hyperbolaPlot
#' @export
hyperbolaPlot <- function(hyp, 
                          x = NULL, 
                          add = TRUE, 
                          ann = FALSE, 
                          ann.pos = 3,
                          ann.offset = 0.5,
                          ann.col = "green",
                          ann.font = 2, 
                          n = 100, ...){
  
  if(is.null(x)){
    lst <- list(...)
    xlim <- par()$usr[1:2] 
    if( length(lst) > 0 && !is.null(xlim) ){
      xlim <- lst[["xlim"]]
    }
    x <- seq(xlim[1], to = xlim[2], length.out = n)
  }
  
  if(class(hyp) == "hyperbola"){
    a  <- hyp$reg$coefficients[2]
    b  <- hyp$reg$coefficients[3]
    cc <- hyp$reg$coefficients[1]
    # if(is.null(x)){
    #   xrge <- (range(hyp$x))
    #   D <- abs(diff(xrge)) * perc
    #   x <- seq(xrge[1] - D, to = xrge[2] + D, length.out = n)
    # } 
    y <- 2 * sqrt( a*x^2 + b*x + cc ) 
  }else{
    if( !(all(names(hyp) %in% c("x0", "t0", "vrms"))) ){
      stop("the names of 'hyp' must be 'x0', 't0' and 'vrms'")
    }
    hyp$z0 <- hyp$vrms * hyp$t0/2   # we need z0 later!
    y <- 2/hyp$vrms * sqrt( (x - hyp$x0)^2 + hyp$z0^2 )
  }
  if(isTRUE(add)){
    lines(x, y, ...)
  }else{
    plot(x, y, ...)
  }
  if(isTRUE(ann)){
    tvrms <- paste0("Vrms = ", as.character(round(hyp$vrms, 4 )), 
                    " cm/ns", "  ", "z = ", 
                    as.character(round(hyp$z0, 1)), " m")
    text(hyp$x0, hyp$t0, pos = ann.pos, offset = ann.offset, 
         col = ann.col, font = ann.font, tvrms)
    # fitmat <- rbind(fitmat, c(hyp$x0, hyp$t0, hyp$vrms))
  }
}
