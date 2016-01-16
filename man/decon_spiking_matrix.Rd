\name{decon_spiking_matrix}
\alias{decon_spiking_matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
decon_spiking_matrix(ym, nf = 60, mu = 1e-04, shft = 1, phase_rot = FALSE, myCols = NULL)
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
  \item{myCols}{
%%     ~~Describe \code{myCols} here~~
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
function (ym, nf = 60, mu = 1e-04, shft = 1, phase_rot = FALSE, 
    myCols = NULL) 
{
    ym_max <- apply(abs(ym), 2, max)
    ym <- t(t(ym)/ym_max)
    ny <- nrow(ym)
    if (is.null(myCols)) {
        myCols <- 1:ncol(ym)
    }
    cat(range(myCols))
    f_min <- apply(ym[, myCols], 2, spikingFilter, nf, mu)
    plot(f_min[, 1], type = "n")
    apply(f_min, 2, lines)
    lines(apply(f_min, 1, median), col = "red")
    v <- numeric(nf)
    v[shft] <- 1
    deconWrap <- function(aa, bb, cc) {
        deconvFreq(bb, aa, cc)
    }
    w_min <- apply(f_min, 2, deconWrap, v, mu)
    w_min2 <- rbind(rep(0, length(myCols)), w_min, rep(0, length(myCols)))
    plot(w_min2[, 1], type = "n", ylim = range(w_min2))
    apply(w_min2, 2, lines)
    abline(h = 0)
    Sys.sleep(2)
    x_dec <- apply(ym[, myCols], 2, convolution, apply(f_min, 
        1, median))
    w_mixed <- NULL
    f_mixed <- NULL
    if (phase_rot) {
        pi_seq <- seq(0, pi, by = 0.01)
        kurt <- numeric(length(pi_seq))
        for (i in seq_along(pi_seq)) {
            xrot <- apply(x_dec, 2, phaseRotation, pi_seq[i])
            A <- as.vector(xrot - colMeans(xrot))
            kurt[i] <- sum((xrot - mean(xrot))^4)/((sum((xrot - 
                mean(xrot))^2))^2)
        }
        phi_max <- pi_seq[which.max(kurt)]
        cat("rotation angle =", phi_max, "rad\n")
        dev.off()
        windows()
        plot(pi_seq, kurt, type = "l")
        abline(v = phi_max, col = "red")
        Sys.sleep(1)
        w_mixed <- w_min
        f_mixed <- f_min
        for (i in 1:ncol(w_min)) {
            w_mixed[, i] <- phaseRotation(w_min[, i], -phi_max)
            f_mixed[, i] <- phaseRotation(f_mixed[, i], phi_max)
        }
        w_mixed <- rbind(rep(0, length(myCols)), w_mixed, rep(0, 
            length(myCols)))
        plot(w_mixed[, 1], type = "n", ylim = range(w_mixed))
        apply(w_mixed, 2, lines)
        abline(h = 0)
        Sys.sleep(2)
        plot(f_mixed[, 1], type = "n")
        apply(f_mixed, 2, lines)
    }
    return(list(w_min = w_min, f_min = f_min, f = f_mixed, w = w_mixed, 
        phi = phi_max))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
