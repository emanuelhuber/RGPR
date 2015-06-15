\name{decon_spiking}
\alias{decon_spiking}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
decon_spiking(ym, nf = 60, mu = 1e-04, shft = 1, phase_rot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ym}{
%%     ~~Describe \code{ym} here~~
}
  \item{nf}{
%%     ~~Describe \code{nf} here~~
}
  \item{mu}{
%%     ~~Describe \code{mu} here~~
}
  \item{shft}{
%%     ~~Describe \code{shft} here~~
}
  \item{phase_rot}{
%%     ~~Describe \code{phase_rot} here~~
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
function (ym, nf = 60, mu = 1e-04, shft = 1, phase_rot = FALSE) 
{
    ym_max <- max(abs(ym))
    ym <- ym/ym_max
    ny <- length(ym)
    f_min <- spikingFilter(ym, nf, mu = mu)
    v <- numeric(nf)
    v[shft] <- 1
    w_min <- deconvFreq(v, f_min, mu = mu)
    x_dec <- convolution(f_min, ym)
    x_dec <- x_dec[1:ny]
    phi_max <- NULL
    if (phase_rot) {
        pi_seq <- seq(0, pi, by = 0.001)
        kurt <- numeric(length(pi_seq))
        for (i in seq_along(pi_seq)) {
            xrot <- phaseRotation(x_dec, pi_seq[i])
            kurt[i] <- sum((xrot - mean(xrot))^4)/((sum((xrot - 
                mean(xrot))^2))^2)
        }
        phi_max <- pi_seq[which.max(kurt)]
        cat("rotation angle =", phi_max, "rad\n")
        dev.off()
        windows()
        plot(pi_seq, kurt, type = "l")
        abline(v = phi_max, col = "red")
        x_dec <- phaseRotation(x_dec, phi_max)
    }
    w_mixed <- deconvFreq(ym, x_dec, mu = mu)[1:nf]
    f_mixed <- deconvFreq(x_dec, ym, mu = mu)[1:nf]
    return(list(x = x_dec, w_min = w_min, f_min = f_min, f = f_mixed, 
        w = w_mixed, phi = phi_max))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
