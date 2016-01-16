\name{fx_deconv}
\alias{fx_deconv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fx_deconv(Y, nf, mu = 0.1, flow = NULL, fhigh = NULL, dz, type = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{nf}{
%%     ~~Describe \code{nf} here~~
}
  \item{mu}{
%%     ~~Describe \code{mu} here~~
}
  \item{flow}{
%%     ~~Describe \code{flow} here~~
}
  \item{fhigh}{
%%     ~~Describe \code{fhigh} here~~
}
  \item{dz}{
%%     ~~Describe \code{dz} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (Y, nf, mu = 0.1, flow = NULL, fhigh = NULL, dz, type = 1) 
{
    npts <- nrow(Y)
    npos <- ncol(Y)
    nfft <- nextpower2(npts)
    Y0 <- matrix(0, nrow = nfft, ncol = npos)
    FX_pred <- Y0
    FX_pred_b <- Y0
    Y0[1:npts, 1:npos] <- Y
    FX <- mvfft(Y0)
    if (is.null(flow)) {
        ilow <- 1
    }
    else {
        ilow <- floor(flow * dz * nfft) + 1
        ilow <- ifelse(ilow < 1, 1, ilow)
    }
    if (is.null(fhigh)) {
        ihigh <- floor(nfft/2) + 1
    }
    else {
        ihigh <- floor(fhigh * dz * nfft) + 1
        ihigh <- ifelse(ihigh > floor(nfft/2) + 1, floor(nfft/2) + 
            1, ihigh)
    }
    for (k in ilow:ihigh) {
        FX_pred[k, ] <- ar_fb(FX[k, ], nf = nf, mu = mu, type = 1)
        FX_pred_b[k, ] <- ar_fb(FX[k, ], nf = nf, mu = mu, type = -1)
    }
    for (k in (nfft/2 + 2):nfft) {
        FX_pred[k, ] <- Conj(FX_pred[nfft - k + 2, ])
        FX_pred_b[k, ] <- Conj(FX_pred_b[nfft - k + 2, ])
    }
    Y_pred_f <- Re(mvfft(FX_pred, inverse = TRUE))/nfft
    Y_pred_b <- Re(mvfft(FX_pred_b, inverse = TRUE))/nfft
    Y_pred <- Y_pred_f[1:npts, ] + Y_pred_b[1:npts, ]
    Y_pred[, (nf + 1):(npos - nf)] <- Y_pred[, (nf + 1):(npos - 
        nf)]/2
    return(Y_pred)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
