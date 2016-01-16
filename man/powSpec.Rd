\name{powSpec}
\alias{powSpec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
powSpec(A, T = 0.8, fac = 1e+06, plot_spec = TRUE, return_spec = FALSE, title_spec = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
%%     ~~Describe \code{A} here~~
}
  \item{T}{
%%     ~~Describe \code{T} here~~
}
  \item{fac}{
%%     ~~Describe \code{fac} here~~
}
  \item{plot_spec}{
%%     ~~Describe \code{plot_spec} here~~
}
  \item{return_spec}{
%%     ~~Describe \code{return_spec} here~~
}
  \item{title_spec}{
%%     ~~Describe \code{title_spec} here~~
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
function (A, T = 0.8, fac = 1e+06, plot_spec = TRUE, return_spec = FALSE, 
    title_spec = NULL) 
{
    A = as.matrix(A)
    nr = nrow(A)
    nc = ncol(A)
    N = 2^(ceiling(log2(nr)))
    A = rbind(A, matrix(0, nrow = N - nr, ncol = nc))
    fft_A = mvfft(A)
    pow = as.matrix(Mod(fft_A))
    pow = as.matrix(Mod(fft_A))
    pha = as.matrix(Arg(fft_A))
    nfreq <- N/2 + 1
    pha = pha[1:nfreq, , drop = FALSE]
    pow = pow[1:nfreq, , drop = FALSE]
    pow_mean = apply(pow, 1, mean, na.rm = TRUE)
    unwrap_pha <- apply(pha, 2, unwrap)
    pha_mean = apply(unwrap_pha, 1, mean, na.rm = TRUE)
    Ts = T * (10^(-9))
    Fs = 1/Ts
    Fc = 1/(2 * Ts)
    fre = Fs * seq(0, N/2)/N/fac
    if (plot_spec) {
        m = seq(0, 10000, by = 50)
        par(mfrow = c(2, 1))
        par(mar = c(0, 4, 4, 2) + 0.1, oma = c(1, 1, 1, 1))
        plot(fre, pow_mean, type = "n", xaxt = "n", ylim = c(0, 
            max(pow)), ylab = "amplitude", xlab = "")
        if (!is.null(dim(A))) {
            nothing <- apply(pow, 2, lines, x = fre, col = rgb(0.2, 
                0.2, 0.2, 7/max(ncol(A), 7)))
        }
        lines(fre, pow_mean, col = "red")
        Axis(side = 1, tcl = +0.3, labels = FALSE, at = m)
        if (!is.null(title_spec)) {
            title(title_spec)
        }
        par(mar = c(4, 4, 0.3, 2))
        plot(fre, pha_mean, type = "n", xaxt = "n", ylim = range(unwrap_pha), 
            xlab = "frequency MHz", ylab = "phase")
        if (!is.null(dim(A))) {
            nothing <- apply(unwrap_pha, 2, lines, x = fre, col = rgb(0.2, 
                0.2, 0.2, 7/max(ncol(A), 7)))
        }
        lines(fre, pha_mean, col = "red")
        Axis(side = 1, tcl = +0.3, labels = m, at = m)
    }
    if (return_spec) {
        return(list(freq = fre, pow = pow, pha = pha))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
