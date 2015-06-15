\name{selectBBox}
\alias{selectBBox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
selectBBox(border = "red", lwd = 2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{border}{
%%     ~~Describe \code{border} here~~
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
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
function (border = "red", lwd = 2, ...) 
{
    bbox <- locator(type = "p", n = 2)
    LIM <- sapply(bbox, range)
    rect(LIM[1, "x"], LIM[1, "y"], LIM[2, "x"], LIM[2, "y"], 
        border = border)
    return(list(xlim = LIM[, "x"], ylim = LIM[, "y"]))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
