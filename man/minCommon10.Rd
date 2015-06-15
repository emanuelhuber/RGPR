\name{minCommon10}
\alias{minCommon10}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
minCommon10(xmin, xmax)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xmin}{
%%     ~~Describe \code{xmin} here~~
}
  \item{xmax}{
%%     ~~Describe \code{xmax} here~~
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
function (xmin, xmax) 
{
    xmin <- as.numeric(xmin)
    xmax <- as.numeric(xmax)
    D <- xmax - xmin
    n <- nchar(D)
    if (as.numeric(substr(xmin, nchar(xmin) - n + 1, nchar(xmin))) + 
        D < 10^(n)) {
        return(as.numeric(substr(xmin, 1, n + 1)) * 10^(nchar(xmin) - 
            n - 1))
    }
    else {
        return(xmin)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
