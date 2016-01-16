\name{paddMatrix}
\alias{paddMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
paddMatrix(I, p1, p2 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{I}{
%%     ~~Describe \code{I} here~~
}
  \item{p1}{
%%     ~~Describe \code{p1} here~~
}
  \item{p2}{
%%     ~~Describe \code{p2} here~~
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
function (I, p1, p2 = NULL) 
{
    if (is.null(p2)) {
        p2 <- p1
    }
    nI <- nrow(I)
    mI <- ncol(I)
    Ipad <- matrix(0, nrow = nI + 2 * p1, ncol = mI + 2 * p2)
    Ipad[(p1 + 1):(p1 + nI), (p2 + 1):(p2 + mI)] <- I
    Ipad[1:p1, (p2 + 1):(p2 + mI)] <- repmat(I[1, ], p1, 1)
    Ipad[(p1 + nI + 1):(nI + 2 * p1), (p2 + 1):(p2 + mI)] <- repmat(I[nI, 
        ], p1, 1)
    Ipad[(p1 + 1):(p1 + nI), 1:p2] <- repmat(I[, 1], 1, p2)
    Ipad[(p1 + 1):(p1 + nI), (p2 + mI + 1):(mI + 2 * p2)] <- repmat(I[, 
        mI], 1, p2)
    Ipad[1:p1, 1:p2] <- I[1, 1]
    Ipad[1:p1, (p2 + mI + 1):(mI + 2 * p2)] <- I[1, mI]
    Ipad[(p1 + nI + 1):(nI + 2 * p1), 1:p2] <- I[nI, 1]
    Ipad[(p1 + nI + 1):(nI + 2 * p1), (p2 + mI + 1):(mI + 2 * 
        p2)] <- I[nI, mI]
    return(Ipad)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
