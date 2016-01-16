\name{print.GPRsurvey}
\alias{print.GPRsurvey}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.GPRsurvey(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (x, ...) 
{
    cat("*** Class GPRsurvey ***\n")
    n <- length(x)
    dirNames <- dirname(x@filepaths)
    if (length(unique(dirNames)) == 1) {
        cat("Unique directory:", dirNames[1], "\n")
    }
    else {
        cat("One directory among others:", dirNames[1], "\n")
    }
    testCoords <- rep(0, n)
    names(testCoords) <- x@names
    if (length(x@coords) > 0) {
        testLength <- sapply(x@coords, length)
        testCoords[names(testLength)] <- testLength
    }
    testCoords <- as.numeric(testCoords > 0) + 1
    testIntersecs <- rep(0, n)
    names(testIntersecs) <- x@names
    if (length(x@intersections) > 0) {
        testLength <- sapply(x@intersections, length)
        testIntersecs[names(testLength)] <- testLength
    }
    testIntersecs <- as.numeric(testIntersecs > 0) + 1
    is_test <- c("NO", "YES")
    cat("- - - - - - - - - - - - - - -\n")
    overview <- data.frame(name = .filename(x@filepaths), length = round(x@lengths, 
        2), units = rep(x@posunit, n), date = x@dates, fequency = x@freqs, 
        coordinates = is_test[testCoords], intersections = is_test[testIntersecs])
    print(overview)
    if (length(x@coords) > 0) {
        cat("- - - - - - - - - - - - - - -\n")
        if (x@crs != "") {
            cat("Coordinate system:", x@crs, "\n")
        }
        else {
            cat("Coordinate system: unknown\n")
        }
        cat
    }
    cat("****************\n")
    return(invisible(overview))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
