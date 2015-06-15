\name{spikingFilter}
\alias{spikingFilter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spikingFilter(y, nf = 32, mu = 0.1, shft = 1)
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
  \item{shft}{
%%     ~~Describe \code{shft} here~~
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
function (y, nf = 32, mu = 0.1, shft = 1) 
{
    y_acf <- as.numeric(acf(y, lag = nf - 1, plot = FALSE)[[1]])
    y_acf[1] <- y_acf[1] + mu
    YtY <- toeplitz(y_acf)
    if (is.null(shft)) {
        ny <- length(y)
        L <- nf + ny - 1
        Y <- convmtx(y, nf)
        H <- solve(YtY) \%*\% t(Y)
        v <- numeric(L)
        P <- Y \%*\% H
        i <- which.max(diag(P))
        v[i] <- 1
        h <- H \%*\% v
        return(list(h = h, delay = i))
    }
    else {
        v <- numeric(nf)
        v[shft] <- 1
        h <- solve(YtY) \%*\% v
        return(h)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
