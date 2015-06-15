\name{readFID}
\alias{readFID}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
readFID(FID, sep = ",")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FID}{
%%     ~~Describe \code{FID} here~~
}
  \item{sep}{
%%     ~~Describe \code{sep} here~~
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
function (FID, sep = ",") 
{
    myFid <- list()
    for (i in seq_along(FID)) {
        A <- read.table(FID[[i]], sep = ",", stringsAsFactors = FALSE, 
            header = TRUE)
        colnames(A) <- toupper(colnames(A))
        if (!all(c("E", "N", "Z", "TRACE") \%in\% colnames(A))) {
            stop("The headers should be \"E\",\"N\",\"Z\",\"TRACE\"!\n")
        }
        myFid[[i]] <- A[, c("E", "N", "Z", "TRACE")]
    }
    return(myFid)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
