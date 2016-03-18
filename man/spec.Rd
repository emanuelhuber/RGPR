\name{spec}
\alias{spec}
\alias{spec-methods}
\alias{spec,GPR-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Show the spectrum
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spec(x, type = c("f-x", "f-k"), return_spec = FALSE, plot_spec = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{return_spec}{
%%     ~~Describe \code{return_spec} here~~
}
  \item{plot_spec}{
%%     ~~Describe \code{plot_spec} here~~
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
structure(function (x, type = c("f-x", "f-k"), return_spec = FALSE, 
    plot_spec = TRUE, ...) 
standardGeneric("spec"), generic = structure("spec", package = "RGPR"), package = "RGPR", group = list(), valueClass = character(0), signature = c("x", 
"type", "return_spec", "plot_spec"), default = `\001NULL\001`, skeleton = (function (x, 
    type = c("f-x", "f-k"), return_spec = FALSE, plot_spec = TRUE, 
    ...) 
stop("invalid call in method dispatch to 'spec' (no default method)", 
    domain = NA))(x, type, return_spec, plot_spec, ...), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
