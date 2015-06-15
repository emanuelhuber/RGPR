\name{plotAmpl}
\alias{plotAmpl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotAmpl(x, FUN = mean, add = FALSE, ylim = NULL, xlim = NULL, col = 1, all = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{FUN}{
%%     ~~Describe \code{FUN} here~~
}
  \item{add}{
%%     ~~Describe \code{add} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{all}{
%%     ~~Describe \code{all} here~~
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
structure(function (x, FUN = mean, add = FALSE, ylim = NULL, 
    xlim = NULL, col = 1, all = FALSE, ...) 
standardGeneric("plotAmpl"), generic = structure("plotAmpl", package = "RGPR"), package = "RGPR", group = list(), valueClass = character(0), signature = c("x", 
"FUN", "add", "ylim", "xlim", "col", "all"), default = `\001NULL\001`, skeleton = (function (x, 
    FUN = mean, add = FALSE, ylim = NULL, xlim = NULL, col = 1, 
    all = FALSE, ...) 
stop("invalid call in method dispatch to 'plotAmpl' (no default method)", 
    domain = NA))(x, FUN, add, ylim, xlim, col, all, ...), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
