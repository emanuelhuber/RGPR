% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/STFT.R
\name{STFT,GPR-method}
\alias{STFT,GPR-method}
\title{Short-time Fourier Transform (STFT)}
\usage{
\S4method{STFT}{GPR}(x, w)
}
\description{
The STFT can be seen as a sliding window that takes at each sample-window 
a Fast-Fourier Transform of the windowed signal.
}
\details{
DFT with N = 128:
The  time  domain  signal  is Contained  in  the  array:  
x[1] to x[128]. The  frequency  domain  signals  are contained  in  the  
two  arrays: ReX[1] to ReX[65], and to ImX[1] to ImX[65].
Notice that 128 points in the time domain corresponds to 65 points in each 
of the frequency domain signals.
That is, N  points in the time domain corresponds to points in the
N/2 + 1 frequency domain (not N/2 points).
 Forgetting about this extra point is a common bug in DFT programs.
ReX[1] holds the average value of all the points.
ImX[1] is equal to zero.
In R, the fft returns: c(ReX[1:(N/2 + 1)], ImX[(N/2 + 1):2]) (??)
}
