\name{delineate}
\alias{delineate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
delineate(x, name = NULL, type = c("raster", "wiggles"), add_topo = FALSE, upsample = NULL, n = 10000, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{name}{
%%     ~~Describe \code{name} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{add_topo}{
%%     ~~Describe \code{add_topo} here~~
}
  \item{upsample}{
%%     ~~Describe \code{upsample} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
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
structure(function (x, name = NULL, type = c("raster", "wiggles"), 
    add_topo = FALSE, upsample = NULL, n = 10000, ...) 
standardGeneric("delineate"), generic = structure("delineate", package = "RGPR"), package = "RGPR", group = list(), valueClass = character(0), signature = c("x", 
"name", "type", "add_topo", "upsample", "n"), default = `\001NULL\001`, skeleton = (function (x, 
    name = NULL, type = c("raster", "wiggles"), add_topo = FALSE, 
    upsample = NULL, n = 10000, ...) 
stop("invalid call in method dispatch to 'delineate' (no default method)", 
    domain = NA))(x, name, type, add_topo, upsample, n, ...), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
