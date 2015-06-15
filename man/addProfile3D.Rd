\name{addProfile3D}
\alias{addProfile3D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
addProfile3D(LINES, col = diverge_hcl(101, h = c(246, 10), c = 120, l = c(30, 90)), plotNew = FALSE, normalize = TRUE, v = 1, zlim = NULL, AGC = FALSE, sig = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LINES}{
%%     ~~Describe \code{LINES} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{plotNew}{
%%     ~~Describe \code{plotNew} here~~
}
  \item{normalize}{
%%     ~~Describe \code{normalize} here~~
}
  \item{v}{
%%     ~~Describe \code{v} here~~
}
  \item{zlim}{
%%     ~~Describe \code{zlim} here~~
}
  \item{AGC}{
%%     ~~Describe \code{AGC} here~~
}
  \item{sig}{
%%     ~~Describe \code{sig} here~~
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
function (LINES, col = diverge_hcl(101, h = c(246, 10), c = 120, 
    l = c(30, 90)), plotNew = FALSE, normalize = TRUE, v = 1, 
    zlim = NULL, AGC = FALSE, sig = 10) 
{
    if (plotNew) {
        open3d()
    }
    for (i in seq_along(LINES)) {
        lineName2 <- strsplit(LINES, split = "[.]")
        lineName <- lineName2[[i]][1]
        fileNameHD <- paste(lineName, ".HD", sep = "")
        fileNameDT1 <- paste(lineName, ".DT1", sep = "")
        cat(basename(lineName), "\n")
        GPR <- readDT1(LINES[[i]])
        myGPRdZ <- as.numeric(as.character(GPR$hd[7, 2]))/as.numeric(as.character(GPR$hd[5, 
            2]))
        HD <- GPR$dt1hd
        A <- GPR$data
        A[is.na(A)] <- 0
        if (!is.null(zlim)) {
            sel <- seq(1, zlim/myGPRdZ/v, by = myGPRdZ)
            A <- A[sel, ]
        }
        if (normalize) {
            A <- normalizeGPR(A)
        }
        if (AGC) {
            A <- apply(A, 2, gain, sig = sig)
        }
        nr = nrow(A)
        nc = ncol(A)
        X <- matrix(HD$recx, ncol = nc, nrow = nr, byrow = TRUE)
        Y <- matrix(HD$recy, ncol = nc, nrow = nr, byrow = TRUE)
        Z <- matrix(HD$topo, ncol = nc, nrow = nr, byrow = TRUE) - 
            matrix(myGPRdZ * v * (0:(nr - 1)), ncol = nc, nrow = nr, 
                byrow = FALSE)
        if (all(HD$topo == 0)) {
            warning("No topography \n")
        }
        if (all(HD$recx == 0)) {
            warning("No x-coordinates \n")
        }
        if (all(HD$recy == 0)) {
            warning("No y-coordinates \n")
        }
        A = (A - min(A))/(max(A) - min(A))
        Alim <- range(A)
        Alen <- Alim[2] - Alim[1] + 1
        colA <- col[(A) * 100 + 1]
        rgl.surface(X, Y, Z, color = colA, back = "fill", smooth = TRUE, 
            lit = FALSE, lwd = 0)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
