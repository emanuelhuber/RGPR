\name{plotRaster}
\alias{plotRaster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotRaster(A, x = NULL, y = NULL, plot_raster = TRUE, barscale = TRUE, add = FALSE, mai = c(1, 0.8, 0.8, 1.8), col = heat.colors(101), note = NULL, main = "", time0 = 0, antsep = 1, v = 0.1, ann = NULL, add_ann = TRUE, fid = NULL, depthunit = "ns", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
%%     ~~Describe \code{A} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{plot_raster}{
%%     ~~Describe \code{plot_raster} here~~
}
  \item{barscale}{
%%     ~~Describe \code{barscale} here~~
}
  \item{add}{
%%     ~~Describe \code{add} here~~
}
  \item{mai}{
%%     ~~Describe \code{mai} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{note}{
%%     ~~Describe \code{note} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{time0}{
%%     ~~Describe \code{time0} here~~
}
  \item{antsep}{
%%     ~~Describe \code{antsep} here~~
}
  \item{v}{
%%     ~~Describe \code{v} here~~
}
  \item{ann}{
%%     ~~Describe \code{ann} here~~
}
  \item{add_ann}{
%%     ~~Describe \code{add_ann} here~~
}
  \item{fid}{
%%     ~~Describe \code{fid} here~~
}
  \item{depthunit}{
%%     ~~Describe \code{depthunit} here~~
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
function (A, x = NULL, y = NULL, plot_raster = TRUE, barscale = TRUE, 
    add = FALSE, mai = c(1, 0.8, 0.8, 1.8), col = heat.colors(101), 
    note = NULL, main = "", time0 = 0, antsep = 1, v = 0.1, ann = NULL, 
    add_ann = TRUE, fid = NULL, depthunit = "ns", ...) 
{
    GPR = as.matrix(A)
    GPR[is.na(GPR)] = 0
    time0 <- mean(time0)
    zlim = range(GPR)
    if (length(list(...))) {
        Lst <- list(...)
        if (!is.null(Lst$zlim)) {
            zlim <- Lst$zlim
        }
    }
    if (grepl("[m]$", depthunit)) {
        mai <- c(1, 0.8, 0.8, 0.5)
    }
    reverse <- nrow(GPR):1
    GPR <- t(GPR[reverse, ])
    if (is.null(x)) {
        x <- (1:nrow(GPR))
    }
    if (is.null(y)) {
        y <- -(ncol(GPR):1)
    }
    if (add == TRUE) {
        par(new = TRUE)
    }
    else {
        par(mai = mai, oma = c(0, 0, 3, 0))
    }
    y <- y + time0
    image(x, y, GPR, col = col, zlim = zlim, xaxs = "i", yaxs = "i", 
        yaxt = "n", ...)
    title(main, outer = TRUE, line = 1)
    usr <- par()$usr
    pin <- par()$pin
    dxin <- diff(usr[1:2])/(pin[1])
    dylim <- diff(usr[3:4])
    dusr <- dylim/length(y)
    pretty_y <- pretty(y)
    if (!is.null(fid) && length(fid) > 0 && any(fid != "")) {
        cin <- par()$cin[2]
        posfid <- x
        testfid <- (fid != "")
        ylim = range(y)
        yr <- diff(usr[3:4])/(pin[2])
        if (sum(testfid) > 0) {
            par(xpd = TRUE)
            cst <- yr * cin
            points(posfid[testfid], cst/2 * 0.75 + rep(ylim[2], 
                sum(testfid)), pch = 25, col = "red", bg = "yellow", 
                cex = 1)
            text(posfid[testfid], cst + rep(ylim[2], sum(testfid)), 
                fid[testfid], cex = 0.6)
            par(xpd = FALSE)
        }
    }
    if (add_ann && !is.null(ann) && length(ann) > 0) {
        posann <- x
        testann <- (ann != "")
        ann <- gsub("#", "\n", ann)
        if (sum(testann) > 0) {
            abline(v = posann[testann], col = "red", lwd = 1)
            mtext(ann[testann], side = 3, line = 1.7, at = posann[testann], 
                col = "red", cex = 0.9)
        }
    }
    axis(side = 2, at = pretty_y + dusr/2, labels = -pretty_y)
    abline(h = 0, col = "red", lwd = 0.5)
    if (grepl("[s]$", depthunit)) {
        depth <- (seq(0, by = 2.5, max(abs(y)) * v))
        depth2 <- seq(0.1, by = 0.1, 0.9)
        depthat <- depthToTime(depth, 0, v, antsep)
        depthat2 <- depthToTime(depth2, 0, v, antsep)
        axis(side = 4, at = -depthat, labels = depth, tck = -0.02)
        axis(side = 4, at = -depthat2, labels = FALSE, tck = -0.01)
        axis(side = 4, at = -1 * depthToTime(1, 0, v, antsep), 
            labels = FALSE, tck = -0.02)
        mtext(paste("depth (m),   v=", v, "m/ns", sep = ""), 
            side = 4, line = 2)
    }
    else {
        axis(side = 4, at = pretty_y + dusr/2, labels = -pretty_y)
    }
    if (!is.null(note)) {
        mtext(note, side = 1, line = 4, cex = 0.6)
    }
    box()
    op <- par(no.readonly = TRUE)
    if (barscale && grepl("[s]$", depthunit)) {
        fin <- par()$fin
        mai2 <- c(1, 0.8 + pin[1] + 1, 0.8, 0.6)
        par(mai = mai2)
        fin2 <- par()$fin
        wstrip <- fin2[1] - mai2[2] - mai2[4]
        xpos <- diff(usr[1:2]) * (mai2[2] - mai[2])/pin[1]
        zstrip <- matrix(seq(zlim[1], zlim[2], length.out = length(col)), 
            nrow = 1)
        xstrip <- c(xpos, xpos + wstrip * dxin) * c(0.9, 1.1)
        ystrip <- seq(min(y), max(y), length.out = length(col))
        pretty_z <- pretty(as.vector(zstrip))
        dzlim <- zlim[2] - zlim[1]
        pretty_at <- usr[3] - dylim * (zlim[1] - pretty_z)/dzlim
        axis(side = 4, las = 2, at = pretty_at, labels = pretty_z)
        image(xstrip, ystrip, zstrip, zlim = zlim, add = TRUE, 
            col = col, axes = FALSE, xlab = "", ylab = "", xaxs = "i", 
            yaxs = "i")
        box()
    }
    par(op)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
