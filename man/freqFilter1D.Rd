\name{freqFilter1D}
\alias{freqFilter1D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
freqFilter1D(A, f = c(100), type = c("low", "high", "bandpass"), L = 257, T = 0.8, plot_spec = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
%%     ~~Describe \code{A} here~~
}
  \item{f}{
%%     ~~Describe \code{f} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{L}{
%%     ~~Describe \code{L} here~~
}
  \item{T}{
%%     ~~Describe \code{T} here~~
}
  \item{plot_spec}{
%%     ~~Describe \code{plot_spec} here~~
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
function (A, f = c(100), type = c("low", "high", "bandpass"), 
    L = 257, T = 0.8, plot_spec = FALSE) 
{
    type = match.arg(type)
    A <- as.matrix(A)
    M = nrow(A)
    Ts = T * 10^(-9)
    Fs = 1/Ts
    f = sort(f) * 10^6
    if (type == "low" || type == "high") {
        if (length(f) > 1) {
            BW = (f[2] - f[1])/Fs
            fc = f[1] + (f[2] - f[1])/2
            L = 4/BW
            L = round(L)
            if (L\%\%2 == 0) 
                L = L + 1
        }
        else if (length(f) == 1) {
            fc = f[1]
        }
        h <- winSincKernel(L, fc/Fs, type)
    }
    else if (type == "bandpass") {
        if (length(f) == 2) {
            h1 <- winSincKernel(L, f[1]/Fs, "low")
            h2 <- winSincKernel(L, f[2]/Fs, "high")
        }
        else if (length(f) == 4) {
            BW = (f[2] - f[1])/Fs
            fc = f[1] + (f[2] - f[1])/2
            L = 4/BW
            L = round(L)
            if (L\%\%2 == 0) 
                L = L + 1
            h1 <- winSincKernel(L, fc/Fs, "low")
            BW = (f[4] - f[3])/Fs
            fc = f[3] + (f[4] - f[3])/2
            L = 4/BW
            L = round(L)
            if (L\%\%2 == 0) 
                L = L + 1
            h2 <- winSincKernel(L, fc/Fs, "high")
        }
        L = max(length(h1), length(h2))
        cat("lenght max", L, "\n")
        if (length(h2) < L) {
            h2 = c(rep(0, (L - length(h2))/2), h2, rep(0, (L - 
                length(h2))/2))
        }
        if (length(h1) < L) {
            h1 = c(rep(0, (L - length(h1))/2), h1, rep(0, (L - 
                length(h1))/2))
        }
        h = -h1 - h2
        h[(L + 1)/2] = h[(L + 1)/2] + 1
    }
    Nfft = 2^(ceiling(log2(L + M - 1)))
    h_long = c(h, rep(0, Nfft - L))
    A = rbind(as.matrix(A), matrix(0, nrow = Nfft - M, ncol = ncol(A)))
    fft_A = mvfft(A)
    fft_h = fft(h_long)
    Y = fft_A * fft_h
    if (type == "bandpass") {
    }
    pow_A = Mod(fft_A)
    pow_h = Mod(fft_h)
    pow_y = Mod(Y)
    if (!is.null(dim(A))) {
        pow_A = apply(pow_A, 1, mean, na.rm = T)
        pow_y = apply(pow_y, 1, mean, na.rm = T)
    }
    pow_A = pow_A[1:(Nfft/2 + 1)]
    pow_y = pow_y[1:(Nfft/2 + 1)]
    pow_h = pow_h[1:(Nfft/2 + 1)]
    fre = Fs * (0:(Nfft/2))/Nfft/1e+06
    if (plot_spec == TRUE) {
        m = seq(0, 900, by = 50)
        par(mar = c(0, 4, 0.3, 2) + 0.1, oma = c(3, 2, 1, 2))
        plot(fre, pow_A, type = "l", xaxt = "n", ylim = c(0, 
            max(pow_A, pow_y)), ylab = "power", lwd = 2)
        lines(fre, pow_y, type = "l", col = "blue", lwd = 2)
        Axis(side = 1, tcl = +0.3, labels = m, at = m)
        par(new = TRUE)
        plot(fre, pow_h, type = "l", col = "red", yaxt = "n", 
            ylab = "")
        legend("topright", c("input signal", "filter", "filtered signal"), 
            col = c("black", "red", "blue"), lwd = c(2, 1, 2), 
            bg = "white")
        abline(v = f/1e+06, col = "grey", lty = 2)
    }
    a = (L - 1)/2
    y = mvfft(Y, inverse = TRUE)
    y = y[a:(a + M - 1), ]/nrow(y)
    return(Re(y))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
