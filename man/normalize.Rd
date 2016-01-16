\name{normalize}
\alias{normalize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
normalize(A, type = c("stat", "min-max", "95", "eq", "sum"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
%%     ~~Describe \code{A} here~~
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
function (A, type = c("stat", "min-max", "95", "eq", "sum")) 
{
    A = as.matrix(A)
    type = match.arg(type)
    if (type == "stat") {
        Anorm <- scale(A, center = .colMeans(A, nrow(A), ncol(A)), 
            scale = apply(A, 2, sd, na.rm = TRUE))
    }
    else if (type == "sum") {
        Anorm <- scale(A, center = FALSE, scale = colSums(abs(A)))
    }
    else if (type == "eq") {
        amp <- apply((A)^2, 2, sum)
        Anorm <- A * sqrt(amp)/sum(sqrt(amp))
    }
    else if (type == "95") {
        A_q95 = (apply((A), 2, quantile, 0.99, na.rm = TRUE))
        A_q05 = (apply((A), 2, quantile, 0.01, na.rm = TRUE))
        Anorm = (A)/(A_q95 - A_q05)
    }
    else {
        Anorm <- scale(A, center = FALSE, scale = (apply((A), 
            2, max, na.rm = TRUE)) - (apply((A), 2, min, na.rm = TRUE)))
    }
    return(Anorm)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
