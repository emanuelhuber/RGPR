\name{plot.GPR}
\alias{plot.GPR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Plot GPR data (2D)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.GPR(x, y, ...)
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
    type <- "raster"
    add_topo <- FALSE
    clip = NULL
    normalize = NULL
    upsample = NULL
    dots <- list()
    if (length(list(...))) {
        dots <- list(...)
        if (!is.null(dots$type)) {
            type <- dots$type
            dots$type <- NULL
        }
        if (!is.null(dots$clip)) {
            clip <- dots$clip
            dots$clip <- NULL
        }
        if (!is.null(dots$normalize)) {
            normalize <- dots$normalize
            dots$normalize <- NULL
        }
        if (!is.null(dots$upsample)) {
            upsample <- dots$upsample
            dots$upsample <- NULL
        }
        add_ann <- TRUE
        if (!is.null(dots$add_ann) && !isTRUE(dots$add_ann)) {
            add_ann <- FALSE
        }
        add_fid <- TRUE
        if (!is.null(dots$add_fid) && !isTRUE(dots$add_fid)) {
            add_fid <- FALSE
        }
        add_topo <- FALSE
        if (!is.null(dots$add_topo) && isTRUE(dots$add_topo)) {
            add_topo <- TRUE
        }
        dots$add_fid <- NULL
        dots$add_topo <- NULL
        dots$addArrows <- NULL
        if (!is.null(dots$lwd)) {
            lwd <- dots$lwd
        }
        dots$add <- NULL
        if (!is.null(dots$shp_files)) {
            add_shp_files <- TRUE
            shp_files <- dots$shp_files
        }
        dots$shp_files <- NULL
    }
    if (length(x@vel) > 0) {
        vel <- x@vel[[1]]
    }
    else {
        vel <- 0
    }
    if (any(dim(x) == 1)) {
        par(mar = c(5, 4, 3, 2) + 0.1, oma = c(0, 0, 3, 0), mgp = c(2, 
            0.5, 0))
        z <- seq(0, by = x@dz, length.out = length(x@data))
        plot(z, x@data, type = "n", xlab = x@depthunit, ylab = "mV", 
            xaxt = "n")
        x_axis <- pretty(seq(x@time0, by = x@dz, length.out = length(x@data)))
        axis(side = 1, at = x_axis + x@time0, labels = x_axis, 
            tck = +0.02)
        depth0 <- depth0(x@time0, vel, antsep = x@antsep)
        depth <- (seq(0, by = 2.5, max(z) * vel))
        depth2 <- seq(0.1, by = 0.1, 0.9)
        depthat <- depthToTime(depth, x@time0, vel, antsep = x@antsep)
        depthat2 <- depthToTime(depth2, x@time0, vel, antsep = x@antsep)
        axis(side = 3, at = depthat, labels = depth, tck = +0.02)
        axis(side = 3, at = depthat2, labels = FALSE, tck = +0.01)
        axis(side = 3, at = depthToTime(1, x@time0, vel, antsep = x@antsep), 
            labels = FALSE, tck = +0.02)
        abline(h = 0, lty = 3, col = "grey")
        abline(v = x@time0, col = "red")
        abline(v = depth0, col = "grey", lty = 3)
        lines(z, x@data)
        title(paste(x@name, ": trace n°", x@traces, " @", x@pos, 
            x@posunit, sep = ""), outer = TRUE)
        mtext(paste("depth (m),   v=", vel, "m/ns", sep = ""), 
            side = 3, line = 2)
    }
    else {
        if (!is.null(upsample)) {
            x <- upsample(x, n = upsample)
        }
        if (!is.null(normalize)) {
            x@data <- normalize(x@data, type = normalize)
        }
        if (!is.null(clip) && is.numeric(clip)) {
            if (length(clip) > 1) {
                x@data <- .clip(x@data, clip[2], clip[1])
            }
            else if (length(clip) == 1) {
                x@data <- .clip(x@data, clip[1])
            }
        }
        if (add_fid == FALSE) {
            x@com <- character(length(x@com))
        }
        type = match.arg(type, c("raster", "wiggles"))
        if (type == "raster") {
            if (add_topo) {
                x <- migration(x)
            }
            if (grepl("[m]$", x@depthunit)) {
                ylab <- paste("depth (", x@depthunit, ")", sep = "")
            }
            else if (grepl("[s]$", x@depthunit)) {
                ylab <- paste("two-way travel time (", x@depthunit, 
                  ")", sep = "")
            }
            if (length(x@coord) > 0 && sum(abs(x@coord[, 1:2]) > 
                0)) {
                xvalues <- lineDist(x@coord)
            }
            else {
                xvalues <- x@pos
            }
            cat(xvalues)
            cat("\n")
            cat(-rev(x@depth))
            cat("\n")
            do.call(plotRaster, c(list(A = x@data, col = diverge_hcl(101, 
                h = c(246, 10), c = 120, l = c(30, 90)), x = xvalues, 
                y = -rev(x@depth), main = x@name, xlab = x@posunit, 
                ylab = ylab, note = x@filename, time0 = x@time0, 
                antsep = x@antsep, v = vel, fid = x@com, ann = x@ann, 
                depthunit = x@depthunit), dots))
        }
        else if (type == "wiggles") {
            if (add_topo && length(x@coord) > 0) {
                topo <- x@coord[, 3]
            }
            else {
                topo = NULL
            }
            if (grepl("[m]$", x@depthunit)) {
                ylab <- paste("depth (", x@depthunit, ")", sep = "")
            }
            else if (grepl("[s]$", x@depthunit)) {
                if (add_topo) {
                  ylab <- paste("depth (m)", sep = "")
                }
                else {
                  ylab <- paste("two-way travel time (", x@depthunit, 
                    ")", sep = "")
                }
            }
            if (length(x@coord) > 0) {
                xvalues <- lineDist(x@coord)
            }
            else {
                xvalues <- x@pos
            }
            do.call(plotWig, c(list(A = x@data, x = xvalues, 
                y = -rev(x@depth), main = x@name, xlab = x@posunit, 
                ylab = ylab, topo = topo, note = x@filename, 
                col = "black", time0 = x@time0, antsep = x@antsep, 
                v = vel, fid = x@com, ann = x@ann, depthunit = x@depthunit), 
                dots))
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
