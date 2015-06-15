\name{plot.GPRsurvey}
\alias{plot.GPRsurvey}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.GPRsurvey(x, y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
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
function (x, y, ...) 
{
    if (length(x@coords) > 0) {
        plotAdd <- FALSE
        addArrows <- TRUE
        add_shp_files <- FALSE
        addIntersections <- TRUE
        addFid <- TRUE
        dots <- list()
        lwd = 1
        if (length(list(...))) {
            dots <- list(...)
            if (!is.null(dots$add) && isTRUE(dots$add)) {
                plotAdd <- TRUE
            }
            if (!is.null(dots$addArrows) && isTRUE(!dots$addArrows)) {
                addArrows <- FALSE
            }
            dots$addArrows <- NULL
            if (!is.null(dots$lwd)) {
                lwd <- dots$lwd
            }
            if (!is.null(dots$addIntersections)) {
                addIntersections <- dots$addIntersections
            }
            dots$addIntersections <- NULL
            if (!is.null(dots$addFid)) {
                addFid <- dots$addFid
            }
            dots$addFid <- NULL
            dots$add <- NULL
            if (!is.null(dots$shp_files)) {
                add_shp_files <- TRUE
                shp_files <- dots$shp_files
            }
            dots$shp_files <- NULL
        }
        dots <- c(dots, list(type = "n"))
        if (!plotAdd) {
            do.call("plot", c(list((do.call(rbind, x@coords))[, 
                1:2]), dots))
        }
        if (add_shp_files) {
            if (length(shp_files) > 0) {
                BASEName <- unlist(strsplit(basename(shp_files), 
                  "[.]"))[seq(from = 1, length.out = length(shp_files), 
                  by = 2)]
                DIRName <- dirname(shp_files)
                for (i in seq_along(shp_files)) {
                  shp <- readOGR(DIRName[i], BASEName[i])
                  cat(DIRName[i], BASEName[i], "\n", sep = "")
                  plot(shp, add = TRUE, pch = 13, col = "darkblue")
                }
            }
        }
        niet <- lapply(x@coords, plotLine, lwd = lwd)
        if (addArrows) {
            niet <- lapply(x@coords, plotArrows, lwd = lwd)
        }
        if (addFid) {
            for (i in 1:length(x)) {
                fidxyz <- fidpos(x@coords[[i]], x@fids[[i]])
                if (length(fidxyz) > 0) {
                  points(fidxyz[, 1:2], pch = 21, col = "black", 
                    bg = "red", cex = 0.7)
                }
            }
        }
        if (length(x@intersections) > 0 && addIntersections) {
            for (i in 1:length(x@intersections)) {
                if (!is.null(x@intersections[[i]])) {
                  points(x@intersections[[i]][, 1:2], pch = 1, 
                    cex = 0.8)
                }
            }
        }
    }
    else {
        warning("no coordinates")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
