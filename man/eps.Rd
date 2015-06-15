\name{eps}
\alias{eps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eps(x, ns)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{ns}{
%%     ~~Describe \code{ns} here~~
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
function (x, ns) 
{
    xmean <- c(rep(0, floor(ns/2)), wapply(x, width = ns, by = 1, 
        FUN = mean), rep(0, floor(ns/2)))
    xsd <- c(rep(0, floor(ns/2)), wapply(x, width = ns, by = 1, 
        FUN = sd), rep(0, floor(ns/2)))
    xtest <- wapply(xsd, width = ns, by = 1, FUN = which.min) + 
        (0):(length(xmean) - 2 * floor(ns/2) - 1)
    return(c(rep(0, floor(ns/2)), xmean[xtest], rep(0, floor(ns/2))))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
