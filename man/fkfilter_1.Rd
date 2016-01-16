\name{FKFilter}
\alias{FKFilter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
FKFilter(A, fk, L = c(5, 5), npad = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
%%     ~~Describe \code{A} here~~
}
  \item{fk}{
%%     ~~Describe \code{fk} here~~
}
  \item{L}{
%%     ~~Describe \code{L} here~~
}
  \item{npad}{
%%     ~~Describe \code{npad} here~~
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
function (A, fk, L = c(5, 5), npad = 1) 
{
    nr <- nrow(A)
    nc <- ncol(A)
    nk <- npad * (nextpower2(nc))
    nf <- npad * (nextpower2(nr))
    A1 <- matrix(0, nrow = nf, ncol = nk)
    A1[1:nr, 1:nc] <- A
    A1_fft <- fft(A1)
    myFlong <- matrix(0, nrow = nf, ncol = nk)
    myFlong[1:(nf/2), 1:(nk/2)] <- fk[(nf/2):1, (nk/2):1]
    myFlong[(nf/2 + 1):(nf), (nk/2 + 1):nk] <- fk[1:(nf/2), 1:(nk/2)]
    myFlong[1:(nf/2), (nk/2 + 1):nk] <- fk[(nf/2):1, (nk):(nk/2 + 
        1)]
    myFlong[(nf/2 + 1):(nf), 1:(nk/2)] <- fk[1:(nf/2), (nk/2 + 
        1):nk]
    if (length(L) == 1) 
        L <- c(L, L)
    if (all(L != 0)) {
        ham2D = hammingWindow(L[1]) \%*\% t(hammingWindow(L[2]))
        ham2Dlong = matrix(0, nrow = nf, ncol = nk)
        ham2Dlong[1:L[1], 1:L[2]] <- ham2D
        FF <- Re(fft(fft(myFlong) * fft(ham2Dlong), inv = TRUE))
    }
    else {
        FF <- myFlong
    }
    FF <- FF/sum(FF)
    A_back <- Re(fft(A1_fft * FF, inv = TRUE))[1:nr, 1:nc]
    return(A_back/(max(A_back) - min(A_back)) * (max(A) - min(A)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
