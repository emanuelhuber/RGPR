\name{gain_agc}
\alias{gain_agc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gain_agc(A, d_t, sig = 10, p = 2, r = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
%%     ~~Describe \code{A} here~~
}
  \item{d_t}{
%%     ~~Describe \code{d_t} here~~
}
  \item{sig}{
%%     ~~Describe \code{sig} here~~
}
  \item{p}{
%%     ~~Describe \code{p} here~~
}
  \item{r}{
%%     ~~Describe \code{r} here~~
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
function (A, d_t, sig = 10, p = 2, r = 0.5) 
{
    sig <- sig/d_t
    Anew <- apply(A, 2, .gain_agc, sig, p, r)
    s1 = ((max(A)) - (min(A)))
    s2 = ((max(Anew)) - (min(Anew)))
    return(Anew * s1/s2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
