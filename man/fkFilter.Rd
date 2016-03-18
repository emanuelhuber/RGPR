\name{fkFilter}
\alias{fkFilter}
\alias{fkFilter-methods}
\alias{fkFilter,GPR-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	frequency-wave filter (f-k filter, 2D frequency filter)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fkFilter(x, fk = NULL, L = c(5, 5), npad = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{fk}{
%%     ~~Describe \code{fk} here~~
}
  \item{L}{
%%     ~~Describe \code{L} here~~
}
  \item{npad}{
%%     ~~Describe \code{npad} here~~
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
structure(function (x, fk = NULL, L = c(5, 5), npad = 1) 
standardGeneric("fkFilter"), generic = structure("fkFilter", package = "RGPR"), package = "RGPR", group = list(), valueClass = character(0), signature = c("x", 
"fk", "L", "npad"), default = `\001NULL\001`, skeleton = (function (x, 
    fk = NULL, L = c(5, 5), npad = 1) 
stop("invalid call in method dispatch to 'fkFilter' (no default method)", 
    domain = NA))(x, fk, L, npad), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
