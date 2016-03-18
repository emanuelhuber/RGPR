\name{depth0}
\alias{depth0}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	zero-depth
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
depth0(time0, v = 0.1, antsep = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{time0}{
%%     ~~Describe \code{time0} here~~
}
  \item{v}{
%%     ~~Describe \code{v} here~~
}
  \item{antsep}{
%%     ~~Describe \code{antsep} here~~
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
function (time0, v = 0.1, antsep = 1) 
{
    time0 - antsep/0.299 + antsep/v
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
