% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/global.R
\name{convolution}
\alias{convolution}
\title{Linear convolution based on FFT}
\usage{
convolution(A, B)
}
\arguments{
\item{A}{A numeric vector or matrix.}

\item{A}{B numeric vector or matrix.}
}
\description{
If A (or B) is a numeric vector, it is converted into a one-column 
matrix. Then if A and B do not have the same number of column, then the 
first column of the matrix with the smallest number of column is repeated to
match the dimension of the other matrix.
match the dimension of the other matrix.
}
