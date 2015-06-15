\name{plot3DSlice}
\alias{plot3DSlice}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot3DSlice(XYZ, slice = c("x", "y", "z"), section = 1, col = diverge_hcl(101, h = c(246, 10), c = 120, l = c(30, 90)), sampling = c(0.25, 0.25, 0.04), rmStripes = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{XYZ}{
%%     ~~Describe \code{XYZ} here~~
}
  \item{slice}{
%%     ~~Describe \code{slice} here~~
}
  \item{section}{
%%     ~~Describe \code{section} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{sampling}{
%%     ~~Describe \code{sampling} here~~
}
  \item{rmStripes}{
%%     ~~Describe \code{rmStripes} here~~
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
function (XYZ, slice = c("x", "y", "z"), section = 1, col = diverge_hcl(101, 
    h = c(246, 10), c = 120, l = c(30, 90)), sampling = c(0.25, 
    0.25, 0.04), rmStripes = TRUE) 
{
    slice = match.arg(slice)
    if (length(slice) > 1) {
        slice = slice[1]
    }
    dimXYZ = dim(XYZ)
    vz = seq(0, dimXYZ[3] - 1, by = 1) * sampling[3]
    vx = seq(0, dimXYZ[1] - 1, by = 1) * sampling[1]
    vy = seq(0, dimXYZ[2] - 1, by = 1) * sampling[2]
    if (rgl.cur() == 0) {
        rgl.open()
        rgl.bg(color = c("white"))
    }
    i = section
    j = i
    k = i
    if (slice == "x") {
        if (rmStripes == TRUE) {
            Xside = normalizeGPR(removeStripes(t(XYZ[, j, ])))
        }
        else {
            Xside = normalizeGPR((t(XYZ[, j, ])))
        }
        Xside_x = matrix(vx, nrow = dimXYZ[3], ncol = dimXYZ[1], 
            byrow = TRUE)
        Xside_y = matrix(vy[j], nrow = dimXYZ[3], ncol = dimXYZ[1], 
            byrow = TRUE)
        Xside_z = matrix(max(vz) - vz, nrow = dimXYZ[3], ncol = dimXYZ[1], 
            byrow = FALSE)
        CCX = (Xside - min(Xside))/(max(Xside) - min(Xside))
        ClimX <- range(CCX)
        ClenX <- ClimX[2] - ClimX[1] + 1
        colCX <- col[(CCX) * 100 + 1]
        surface3d(Xside_x, Xside_z, Xside_y, col = setCol(Xside), 
            lit = FALSE, front = "fill", back = "fill")
    }
    else if (slice == "z") {
        if (rmStripes == TRUE) {
            Zside = (removeStripes(t(XYZ[, , k])))
        }
        else {
            Zside = ((t(XYZ[, , k])))
        }
        Zside_x = matrix(vx, nrow = dimXYZ[2], ncol = dimXYZ[1], 
            byrow = TRUE)
        Zside_y = matrix(vy, nrow = dimXYZ[2], ncol = dimXYZ[1], 
            byrow = FALSE)
        Zside_z = matrix(max(vz) - vz[k], nrow = dimXYZ[2], ncol = dimXYZ[1], 
            byrow = FALSE)
        CCZ = (Zside - min(Zside))/(max(Zside) - min(Zside))
        ClimZ <- range(CCZ)
        ClenZ <- ClimZ[2] - ClimZ[1] + 1
        colCZ <- col[(CCZ) * 100 + 1]
        surface3d(Zside_x, Zside_z, Zside_y, col = setCol(Zside), 
            lit = FALSE, front = "fill", back = "fill")
    }
    else if (slice == "y") {
        if (rmStripes == TRUE) {
            Yside = normalizeGPR(removeStripes(t(XYZ[i, , ])))
        }
        else {
            Yside = normalizeGPR((t(XYZ[i, , ])))
        }
        Yside_x = matrix(vx[i], nrow = dimXYZ[3], ncol = dimXYZ[2], 
            byrow = TRUE)
        Yside_y = matrix(vy, nrow = dimXYZ[3], ncol = dimXYZ[2], 
            byrow = TRUE)
        Yside_z = matrix(max(vz) - vz, nrow = dimXYZ[3], ncol = dimXYZ[2], 
            byrow = FALSE)
        CCY = (Yside - min(Yside))/(max(Yside) - min(Yside))
        ClimY <- range(CCY)
        ClenY <- ClimY[2] - ClimY[1] + 1
        colCY <- col[(CCY) * 100 + 1]
        surface3d(Yside_x, Yside_z, Yside_y, col = setCol(Yside), 
            lit = FALSE, front = "fill", back = "fill")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
