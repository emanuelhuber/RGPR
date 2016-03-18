\name{getAmpl}
\alias{getAmpl}
\alias{getAmpl-methods}
\alias{getAmpl,GPR-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Return mean/median trace amplitude
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getAmpl(x, FUN = mean, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{FUN}{
%%     ~~Describe \code{FUN} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
structure(function (x, FUN = mean, ...) 
standardGeneric("getAmpl"), generic = structure("getAmpl", package = "RGPR"), package = "RGPR", group = list(), valueClass = character(0), signature = c("x", 
"FUN"), default = `\001NULL\001`, skeleton = (function (x, FUN = mean, 
    ...) 
stop("invalid call in method dispatch to 'getAmpl' (no default method)", 
    domain = NA))(x, FUN, ...), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
