\name{wapply}
\alias{wapply}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wapply(x, width, by = NULL, FUN = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{by}{
%%     ~~Describe \code{by} here~~
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
function (x, width, by = NULL, FUN = NULL, ...) 
{
    FUN <- match.fun(FUN)
    if (is.null(by)) 
        by <- width
    lenX <- length(x)
    SEQ1 <- seq(1, lenX - width + 1, by = by)
    SEQ2 <- lapply(SEQ1, function(x) x:(x + width - 1))
    OUT <- lapply(SEQ2, function(a) FUN(x[a], ...))
    OUT <- base:::simplify2array(OUT, higher = TRUE)
    return(OUT)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
