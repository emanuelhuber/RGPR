\name{ar_fb}
\alias{ar_fb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ar_fb(y, nf, mu = 0.1, type = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{nf}{
%%     ~~Describe \code{nf} here~~
}
  \item{mu}{
%%     ~~Describe \code{mu} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (y, nf, mu = 0.1, type = 1) 
{
    if (type == -1) {
        ny <- length(y)
        H <- convmtx(y, nf)[, nf:1]
        d <- numeric(nf + ny - 1)
        d[(nf + 1):(ny + nf - 1)] <- y[1:(ny - 1)]
        f <- solve(t(H) \%*\% H + mu * diag(nf)) \%*\% t(H) \%*\% d
        y_pred <- numeric(ny)
        y_pred[1:(ny - 1)] <- (H \%*\% f)[(nf + 1):(nf + ny - 1)]
    }
    else if (type == 1) {
        ny <- length(y)
        H <- convmtx(y, nf)
        d <- numeric(nf + ny - 1)
        d[1:(ny - 1)] <- y[2:ny]
        f <- solve(t(H) \%*\% H + mu * diag(nf)) \%*\% t(H) \%*\% d
        y_pred <- numeric(ny)
        y_pred[2:ny] <- (H \%*\% f)[1:(ny - 1)]
    }
    return(y_pred)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
