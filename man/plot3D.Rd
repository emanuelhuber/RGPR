\name{plot3D}
\alias{plot3D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot3D(x, add_topo = FALSE, clip = NULL, normalize = NULL, upsample = NULL, add = TRUE, xlim = NULL, ylim = NULL, zlim = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{add_topo}{
%%     ~~Describe \code{add_topo} here~~
}
  \item{clip}{
%%     ~~Describe \code{clip} here~~
}
  \item{normalize}{
%%     ~~Describe \code{normalize} here~~
}
  \item{upsample}{
%%     ~~Describe \code{upsample} here~~
}
  \item{add}{
%%     ~~Describe \code{add} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{zlim}{
%%     ~~Describe \code{zlim} here~~
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
structure(function (x, add_topo = FALSE, clip = NULL, normalize = NULL, 
    upsample = NULL, add = TRUE, xlim = NULL, ylim = NULL, zlim = NULL, 
    ...) 
standardGeneric("plot3D"), generic = structure("plot3D", package = "RGPR"), package = "RGPR", group = list(), valueClass = character(0), signature = c("x", 
"add_topo", "clip", "normalize", "upsample", "add", "xlim", "ylim", 
"zlim"), default = `\001NULL\001`, skeleton = (function (x, add_topo = FALSE, 
    clip = NULL, normalize = NULL, upsample = NULL, add = TRUE, 
    xlim = NULL, ylim = NULL, zlim = NULL, ...) 
stop("invalid call in method dispatch to 'plot3D' (no default method)", 
    domain = NA))(x, add_topo, clip, normalize, upsample, add, 
    xlim, ylim, zlim, ...), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
