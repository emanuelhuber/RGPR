\name{localOrientation}
\alias{localOrientation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
localOrientation(P, blksze = c(5, 10), thresh = 0.1, winEdge = c(7, 7), winBlur = c(3, 3), winTensor = c(5, 10), sdTensor = 2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{P}{
%%     ~~Describe \code{P} here~~
}
  \item{blksze}{
%%     ~~Describe \code{blksze} here~~
}
  \item{thresh}{
%%     ~~Describe \code{thresh} here~~
}
  \item{winEdge}{
%%     ~~Describe \code{winEdge} here~~
}
  \item{winBlur}{
%%     ~~Describe \code{winBlur} here~~
}
  \item{winTensor}{
%%     ~~Describe \code{winTensor} here~~
}
  \item{sdTensor}{
%%     ~~Describe \code{sdTensor} here~~
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
function (P, blksze = c(5, 10), thresh = 0.1, winEdge = c(7, 
    7), winBlur = c(3, 3), winTensor = c(5, 10), sdTensor = 2, 
    ...) 
{
    n = nrow(P)
    m = ncol(P)
    Pn = (P - mean(P))/sd(as.vector(P))
    P <- Pn
    blurWinX = winBlur[1]
    blurWinY = winBlur[2]
    k = matrix(1, nrow = blurWinX, ncol = blurWinY, byrow = T)/(blurWinX * 
        blurWinY)
    P_f = convolution2D(P, k, 0)
    nnx = winEdge[1]
    nny = winEdge[2]
    vx = convolution2D(P_f, dx_gkernel(nnx, nny, 1), 0)
    vy = convolution2D(P_f, dy_gkernel(nnx, nny, 1), 0)
    Gxx = vx^2
    Gyy = vy^2
    Gxy = vx * vy
    Jxx = convolution2D(Gxx, gkernel(winTensor[1], winTensor[2], 
        sdTensor), 0)
    Jyy = convolution2D(Gyy, gkernel(winTensor[1], winTensor[2], 
        sdTensor), 0)
    Jxy = convolution2D(Gxy, gkernel(winTensor[1], winTensor[2], 
        sdTensor), 0)
    o_alpha = Jxx + Jyy
    o_beta = sqrt((Jxx - Jyy)^2 + 4 * (Jxy)^2)/o_alpha
    o_theta = 1/2 * atan2(2 * Jxy, (Jxx - Jyy)) + pi/2
    o_lambda1 = (Jxx + Jyy + sqrt((Jxx - Jyy)^2 + 4 * (Jxy)^2))/2
    o_lambda2 = (Jxx + Jyy - sqrt((Jxx - Jyy)^2 + 4 * (Jxy)^2))/2
    return(list(energy = o_alpha, anisotropy = o_beta, orientation = o_theta, 
        lambda1 = o_lambda1, lambda2 = o_lambda2))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
