% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hyperbola.R
\name{hyperbolaSim}
\alias{hyperbolaSim}
\title{Simulate hyperbola}
\usage{
hyperbolaSim(x, hyp)
}
\arguments{
\item{x}{[\code{numeric}] 
The horitzontal positions at which to compute the hyperbola values.}

\item{hyp}{[\code{list}|\code{class hyperbola}] 
  Either a list with elements \code{x0}, \code{t0}, and \code{vrms}
  corresponding to the coordinates of the hyperbola vertex and
  the root-mean-square velocity, or a list of class 
  \code{hyperbola} (i.e., the output of the function 
  \code{\link{hyperbolaFit}}). 
\code{y = NULL})}

\item{[\code{numeric}]}{The time values of the hyperbola.}
}
\description{
Return the time values of the hyperbola as a function of the position
values \code{x}.
}
\examples{

xy <- list( x = c( 11.8,  15.0,  17.7, 20.3, 24.4,  27.4,  30.9,  35.2),
            y = c(142.2, 119.8, 107.7, 99.5, 97.5, 105.6, 120.9, 138.1))
hyp <- hyperbolaFit(xy)

x <- seq(10, 40, by = 0.1)
y <- hyperbolaSim(x, hyp) 
plot(x, y, type = "l", ylim = rev(range(y)))   

hyp2 <- list(x0 = hyp$x0, t0 = hyp$t0, vrms = hyp$vrms)  
y <- hyperbolaSim(x, hyp) 
plot(x, y, type = "l", ylim = rev(range(y)))  
}
\seealso{
\code{\link{hyperbolaSim}}, \code{\link{hyperbolaPlot}}
}
