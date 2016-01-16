\name{dewow}
\alias{dewow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dewow(x, w = 100, x0 = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{w}{
%%     ~~Describe \code{w} here~~
}
  \item{x0}{
%%     ~~Describe \code{x0} here~~
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
structure(function (x, w = 100, x0 = 0.1) 
standardGeneric("dewow"), generic = structure("dewow", package = "RGPR"), package = "RGPR", group = list(), valueClass = character(0), signature = c("x", 
"w", "x0"), default = `\001NULL\001`, skeleton = (function (x, 
    w = 100, x0 = 0.1) 
stop("invalid call in method dispatch to 'dewow' (no default method)", 
    domain = NA))(x, w, x0), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
