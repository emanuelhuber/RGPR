\name{gain_geospreading}
\alias{gain_geospreading}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gain_geospreading(A, alpha, d_t, t_0 = NULL, t_end = NULL, t_cst = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
%%     ~~Describe \code{A} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{d_t}{
%%     ~~Describe \code{d_t} here~~
}
  \item{t_0}{
%%     ~~Describe \code{t_0} here~~
}
  \item{t_end}{
%%     ~~Describe \code{t_end} here~~
}
  \item{t_cst}{
%%     ~~Describe \code{t_cst} here~~
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
function (A, alpha, d_t, t_0 = NULL, t_end = NULL, t_cst = NULL) 
{
    g <- .gain_geospreading(A[, 1], alpha, d_t, t_0, t_end, t_cst)
    Anew <- (A) * g
    s1 = ((max(A)) - (min(A)))
    s2 = ((max(Anew)) - (min(Anew)))
    return(Anew * s1/s2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
