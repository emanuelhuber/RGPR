\name{GPRsurvey}
\alias{GPRsurvey}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GPRsurvey(LINES)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LINES}{
%%     ~~Describe \code{LINES} here~~
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
function (LINES) 
{
    n <- length(LINES)
    line_names <- character(n)
    line_descriptions <- character(n)
    line_surveymodes <- character(n)
    line_dates <- character(n)
    line_freq <- numeric(n)
    line_antsep <- numeric(n)
    line_lengths <- numeric(n)
    posunit <- character(1)
    crs <- character(1)
    coords <- list()
    fids <- list()
    for (i in seq_along(LINES)) {
        gpr <- readGPR(LINES[[i]])
        line_names[i] <- name(gpr)
        line_descriptions[i] <- description(gpr)
        line_surveymodes[i] <- gpr@surveymode
        line_dates[i] <- gpr@date
        line_freq[i] <- gpr@freq
        line_antsep[i] <- gpr@antsep
        posunit <- gpr@posunit
        crs <- gpr@crs
        if (length(gpr@coord) > 0) {
            if (is.null(colnames(gpr@coord))) {
                coords[[line_names[i]]] <- gpr@coord
            }
            else if (all(toupper(colnames(gpr@coord)) \%in\% c("E", 
                "N", "Z"))) {
                coords[[line_names[i]]] <- gpr@coord[, c("E", 
                  "N", "Z")]
            }
            else if (all(toupper(colnames(gpr@coord)) \%in\% c("X", 
                "Y", "Z"))) {
                coords[[line_names[i]]] <- gpr@coord[, c("X", 
                  "Y", "Z")]
            }
            else {
                coords[[line_names[i]]] <- gpr@coord
            }
            line_lengths[i] <- lineDist(gpr@coord[, 1:2], last = TRUE)
        }
        else {
            line_lengths[i] <- gpr@dx * gpr@ntr
        }
        fids[[line_names[i]]] <- gpr@com
    }
    x <- new("GPRsurvey", filepaths = LINES, names = line_names, 
        descriptions = line_descriptions, surveymodes = line_surveymodes, 
        dates = line_dates, freqs = line_freq, lengths = line_lengths, 
        antseps = line_antsep, posunit = posunit, crs = crs, 
        coords = coords, fids = fids, intersections = list())
    x <- setCoordref(x)
    return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
