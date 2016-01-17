\name{freqFilter}
\alias{freqFilter}
\alias{freqFilter-methods}
\alias{freqFilter,GPR-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Trace to trace frequency filter
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
freqFilter(x, f = 100, type = c("low", "high", "bandpass"), L = 257, plot_spec = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{f}{
%%     ~~Describe \code{f} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{L}{
%%     ~~Describe \code{L} here~~
}
  \item{plot_spec}{
%%     ~~Describe \code{plot_spec} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
structure(function (x, f = 100, type = c("low", "high", "bandpass"), 
    L = 257, plot_spec = FALSE) 
standardGeneric("freqFilter"), generic = structure("freqFilter", package = "RGPR"), package = "RGPR", group = list(), valueClass = character(0), signature = c("x", 
"f", "type", "L", "plot_spec"), default = `\001NULL\001`, skeleton = (function (x, 
    f = 100, type = c("low", "high", "bandpass"), L = 257, plot_spec = FALSE) 
stop("invalid call in method dispatch to 'freqFilter' (no default method)", 
    domain = NA))(x, f, type, L, plot_spec), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
