\name{readGPR}
\alias{readGPR}
\alias{readGPR-methods}
\alias{readGPR,character-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Read GPR data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
readGPR(filename, description = "", coordfile = NULL, crs = "", intfile = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{description}{
%%     ~~Describe \code{description} here~~
}
  \item{coordfile}{
%%     ~~Describe \code{coordfile} here~~
}
  \item{crs}{
%%     ~~Describe \code{crs} here~~
}
  \item{intfile}{
%%     ~~Describe \code{intfile} here~~
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
structure(function (filename, description = "", coordfile = NULL, 
    crs = "", intfile = NULL) 
standardGeneric("readGPR"), generic = structure("readGPR", package = "RGPR"), package = "RGPR", group = list(), valueClass = character(0), signature = c("filename", 
"description", "coordfile", "crs", "intfile"), default = `\001NULL\001`, skeleton = (function (filename, 
    description = "", coordfile = NULL, crs = "", intfile = NULL) 
stop("invalid call in method dispatch to 'readGPR' (no default method)", 
    domain = NA))(filename, description, coordfile, crs, intfile), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
