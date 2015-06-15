\name{plotTopo}
\alias{plotTopo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotTopo(NEZ_file, add = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{NEZ_file}{
%%     ~~Describe \code{NEZ_file} here~~
}
  \item{add}{
%%     ~~Describe \code{add} here~~
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
function (NEZ_file, add = TRUE) 
{
    topo <- read.table(NEZ_file, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    PCODE <- unique(topo$PCODE)
    TS <- agrep("TS", PCODE)
    REF <- agrep("REF", PCODE)
    WATER <- agrep("WATER", PCODE)
    CROSS <- which("CROSS" == PCODE)
    REVERSE <- agrep("REVERSE", PCODE)
    LINES <- agrep("LINE", PCODE)
    LINES <- LINES[!(agrep("LINE", PCODE) \%in\% REVERSE)]
    POINTS <- which(!(1:length(PCODE) \%in\% c(LINES, TS, REVERSE, 
        WATER, CROSS, REF)))
    NOT_REVERSE <- !(1:length(PCODE) \%in\% agrep("REVERSE", PCODE))
    not_rev <- !(1:nrow(topo) \%in\% agrep("REVERSE", topo$PCODE))
    if (add == FALSE) {
        plot(topo[not_rev, c("E", "N")], type = "n", asp = 1)
    }
    for (i in 1:length(REVERSE)) {
        points(-topo[topo[, "PCODE"] == PCODE[REVERSE[i]], c("E", 
            "N")], pch = 20, col = 1)
    }
    points(topo[topo[, "PCODE"] \%in\% PCODE[WATER], c("E", "N")], 
        pch = 10, col = 1)
    for (i in 1:length(POINTS)) {
        points(topo[topo[, "PCODE"] == PCODE[POINTS[i]], c("E", 
            "N")], pch = 3, col = 1, cex = 0.7)
    }
    points(topo[topo[, "PCODE"] \%in\% PCODE[REF], c("E", "N")], 
        pch = 25, col = 3, bg = "green")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
