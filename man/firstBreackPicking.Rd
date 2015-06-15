\name{firstBreackPicking}
\alias{firstBreackPicking}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
firstBreackPicking(s, nl = 11, ns = 23, bet = 0.2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
%%     ~~Describe \code{s} here~~
}
  \item{nl}{
%%     ~~Describe \code{nl} here~~
}
  \item{ns}{
%%     ~~Describe \code{ns} here~~
}
  \item{bet}{
%%     ~~Describe \code{bet} here~~
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
function (s, nl = 11, ns = 23, bet = 0.2) 
{
    E1 <- c(wapply(s, width = nl, by = 1, FUN = sum), rep(0, 
        2 * floor(nl/2)))
    E2 <- cumsum(s)
    Er <- E1/(E2 + bet)
    Er_fil <- eps(Er, ns = ns)
    first_break <- which.max(abs(diff(Er_fil)))
    return(first_break)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
