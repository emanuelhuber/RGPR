\name{convolution2D}
\alias{convolution2D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
convolution2D(h, k, bias = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h}{
%%     ~~Describe \code{h} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{bias}{
%%     ~~Describe \code{bias} here~~
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
function (h, k, bias = 0) 
{
    nh = nrow(h)
    mh = ncol(h)
    nk = nrow(k)
    mk = ncol(k)
    if (nk > nh || mk > mh) {
        stop("Kernel 'k' should be smaller than the matrix 'h'\n")
    }
    h0 <- paddMatrix(h, nk, mk)
    nL <- nrow(h0)
    mL <- ncol(h0)
    k0 <- matrix(0, nrow = nL, ncol = mL)
    h0[1:nh, 1:mh] <- h
    k0[1:nk, 1:mk] <- k
    g <- Re(fft(fft(k0) * fft(h0), inverse = TRUE))
    g2 <- g[nk - 1 + 1:nh, mk - 1 + 1:mh]
    return(g2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
