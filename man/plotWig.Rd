\name{plotWig}
\alias{plotWig}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotWig(A, x = NULL, y = NULL, xlim = NULL, ylim = NULL, topo = NULL, main = "", note = NULL, fid = NULL, ann = NULL, add_ann = TRUE, pdfName = NULL, ws = 1, side = 1, dx = 0.25, dz = 0.4, ratio = 1, col = black, time0 = 0, antsep = 1, v = 0.1, depthunit = "ns", lwd = 0.5, ...)
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
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{topo}{
%%     ~~Describe \code{topo} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{note}{
%%     ~~Describe \code{note} here~~
}
  \item{fid}{
%%     ~~Describe \code{fid} here~~
}
  \item{ann}{
%%     ~~Describe \code{ann} here~~
}
  \item{add_ann}{
%%     ~~Describe \code{add_ann} here~~
}
  \item{pdfName}{
%%     ~~Describe \code{pdfName} here~~
}
  \item{ws}{
%%     ~~Describe \code{ws} here~~
}
  \item{side}{
%%     ~~Describe \code{side} here~~
}
  \item{dx}{
%%     ~~Describe \code{dx} here~~
}
  \item{dz}{
%%     ~~Describe \code{dz} here~~
}
  \item{ratio}{
%%     ~~Describe \code{ratio} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
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
  \item{depthunit}{
%%     ~~Describe \code{depthunit} here~~
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
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
function (A, x = NULL, y = NULL, xlim = NULL, ylim = NULL, topo = NULL, 
    main = "", note = NULL, fid = NULL, ann = NULL, add_ann = TRUE, 
    pdfName = NULL, ws = 1, side = 1, dx = 0.25, dz = 0.4, ratio = 1, 
    col = black, time0 = 0, antsep = 1, v = 0.1, depthunit = "ns", 
    lwd = 0.5, ...) 
{
    dx <- mean(diff(x))
    A[is.na(A)] = 0
    A = A/max(abs(A)) * dx
    nr = nrow(A)
    nc = ncol(A)
    A <- A[nr:1, ]
    time0 <- mean(time0)
    if (is.null(y)) {
        y <- -(ncol(GPR):1)
    }
    if (is.null(topo)) {
        topo <- rep(0L, nc)
    }
    else {
        if (grepl("[s]$", depthunit)) {
            y <- y * v/2
            depthunit <- "m"
        }
        topo <- topo - max(topo)
    }
    if (grepl("[s]$", depthunit)) {
    }
    else if (grepl("[m]$", depthunit)) {
        depth0 <- depthToTime(z = 0, time0, v = v, antsep = antsep) * 
            v/2
        y <- y + depth0
    }
    if (is.null(xlim)) {
        xlim <- range(x) + c(-1, 1) * dx
        test <- rep(TRUE, length(x))
    }
    else {
        test <- (x >= xlim[1] & x <= xlim[2])
        xlim <- xlim + c(-1, 1) * dx
    }
    if (is.null(ylim)) {
        ylim <- range(y) + range(topo)
    }
    omi = c(0, 0, 0.6, 0)
    mgp = c(2.5, 0.75, 0)
    fac <- 0.2
    if (grepl("[m]$", depthunit)) {
        mai = c(1, 0.8, 0.6, 0.4) + 0.02
        heightPDF <- fac * diff(ylim) + sum(omi[c(1, 3)] + mai[c(1, 
            3)])
        widthPDF <- fac * diff(xlim) * ratio + sum(omi[c(2, 4)] + 
            mai[c(2, 4)])
    }
    else {
        mai = c(1, 0.8, 0.6, 0.8) + 0.02
        heightPDF <- fac * (ylim[2] - ylim[1]) * v/2 + sum(omi[c(1, 
            3)] + mai[c(1, 3)])
        widthPDF <- fac * (xlim[2] - xlim[1]) * ratio + sum(omi[c(2, 
            4)] + mai[c(2, 4)])
    }
    if (!is.null(pdfName)) {
        CairoPDF(file = paste(pdfName, ".pdf", sep = ""), width = widthPDF, 
            height = heightPDF, bg = "white", pointsize = 10, 
            title = pdfName)
    }
    par(mai = mai, omi = omi, mgp = mgp)
    plot(0, 0, type = "n", xaxs = "i", yaxs = "i", axes = FALSE, 
        xlim = xlim, ylim = ylim, ...)
    title(main, outer = TRUE, line = 1)
    if (!is.null(fid) && length(fid) > 0 && any(fid != "")) {
        pin <- par("pin")
        usr <- par("usr")
        cin <- par()$cin[2]
        posfid <- x[test]
        fid <- fid[test]
        testfid <- (fid != "")
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
    if (side == 1) {
        for (i in rev(seq_along(x))) {
            y2 <- y + topo[i]
            wig = cbind(ws * A[, i] + x[i], y2)
            wig1 = rbind(c(x[i], y2[1]), wig, c(x[i], y2[nr]))
            polygon(wig1, col = col, border = NA)
            rect(min(wig1[, 1]), ylim[1], x[i], ylim[2], col = "white", 
                border = NA)
        }
    }
    else {
        for (i in (seq_along(x))) {
            y2 <- y + topo[i]
            wig = cbind(ws * A[, i] + x[i], y2)
            wig1 = rbind(c(x[i], y2[1]), wig, c(x[i], y2[nr]))
            polygon(wig1, col = col, border = NA)
            rect(max(wig1[, 1]), ylim[1], x[i], ylim[2], col = "white", 
                border = NA)
        }
    }
    for (i in (seq_along(x))) {
        y2 <- y + topo[i]
        lines(x[i] + ws * A[, i], y2, lwd = lwd)
    }
    if (add_ann && !is.null(ann) && length(ann) > 0) {
        posann <- x[test]
        ann <- ann[test]
        testann <- (ann != "")
        ann <- gsub("#", "\n", ann)
        if (sum(testann) > 0) {
            abline(v = posann[testann], col = "red", lwd = 0.5)
            mtext(ann[testann], side = 3, line = 1.7, at = posann[testann], 
                col = "red", cex = 0.9)
        }
    }
    axis(side = 1, tck = -0.02)
    if (grepl("[s]$", depthunit)) {
        abline(h = -time0, col = "red", lwd = 0.5)
        depth <- (seq(0, by = 2.5, max(abs(y)) * v))
        depth2 <- seq(0.1, by = 0.1, 0.9)
        depthat <- depthToTime(depth, time0, v, antsep)
        depthat2 <- depthToTime(depth2, time0, v, antsep)
        axis(side = 4, at = -depthat, labels = depth, tck = -0.02)
        axis(side = 4, at = -depthat2, labels = FALSE, tck = -0.01)
        axis(side = 4, at = -1 * depthToTime(1, time0, v, antsep), 
            labels = FALSE, tck = -0.02)
        axis(side = 2, at = pretty(y) - time0, labels = -pretty(y), 
            tck = -0.02)
        mtext(paste("depth (m),   v=", v, "m/ns", sep = ""), 
            side = 4, line = 2)
    }
    else {
        abline(h = 0, col = "red", lwd = 0.5)
        axis(side = 2, at = pretty(y), labels = -pretty(y), tck = -0.02)
        axis(side = 4, at = pretty(y), labels = -pretty(y), tck = -0.02)
    }
    box()
    if (!is.null(note)) {
        mtext(note, side = 1, line = 4, cex = 0.6)
    }
    if (!is.null(pdfName)) {
        dev.off()
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
