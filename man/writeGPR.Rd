\name{writeGPR}
\alias{writeGPR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
writeGPR(x, path, format = c("DT1", "rds"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{path}{
%%     ~~Describe \code{path} here~~
}
  \item{format}{
%%     ~~Describe \code{format} here~~
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
structure(function (x, path, format = c("DT1", "rds")) 
standardGeneric("writeGPR"), generic = structure("writeGPR", package = "RGPR"), package = "RGPR", group = list(), valueClass = character(0), signature = c("x", 
"path", "format"), default = `\001NULL\001`, skeleton = (function (x, 
    path, format = c("DT1", "rds")) 
stop("invalid call in method dispatch to 'writeGPR' (no default method)", 
    domain = NA))(x, path, format), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
