\name{GPRsurvey-class}
\Rdversion{1.1}
\docType{class}
\alias{GPRsurvey-class}
\alias{[,GPRsurvey,ANY,ANY,ANY-method}
\alias{coords<-,GPRsurvey-method}
\alias{exportFID,GPRsurvey-method}
\alias{getLine,GPRsurvey-method}
\alias{interpTraces,GPRsurvey-method}
\alias{intersections,GPRsurvey-method}
\alias{length,GPRsurvey-method}
\alias{plot3D,GPRsurvey-method}
\alias{plotDelineations3D,GPRsurvey-method}
\alias{setCoordref,GPRsurvey-method}
\alias{show,GPRsurvey-method}
\alias{surveyIntersections,GPRsurvey-method}
\alias{writeGPR,GPRsurvey-method}

\title{Class \code{"GPRsurvey"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("GPRsurvey", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{filepaths}:}{Object of class \code{"character"} ~~ }
    \item{\code{names}:}{Object of class \code{"character"} ~~ }
    \item{\code{descriptions}:}{Object of class \code{"character"} ~~ }
    \item{\code{freqs}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{lengths}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{surveymodes}:}{Object of class \code{"character"} ~~ }
    \item{\code{dates}:}{Object of class \code{"character"} ~~ }
    \item{\code{antseps}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{posunit}:}{Object of class \code{"character"} ~~ }
    \item{\code{crs}:}{Object of class \code{"character"} ~~ }
    \item{\code{coordref}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{coords}:}{Object of class \code{"list"} ~~ }
    \item{\code{intersections}:}{Object of class \code{"list"} ~~ }
    \item{\code{fids}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "GPRsurvey", i = "ANY", j = "ANY", drop = "ANY")}: ... }
    \item{coords<-}{\code{signature(x = "GPRsurvey")}: ... }
    \item{exportFID}{\code{signature(x = "GPRsurvey")}: ... }
    \item{getLine}{\code{signature(x = "GPRsurvey")}: ... }
    \item{interpTraces}{\code{signature(x = "GPRsurvey")}: ... }
    \item{intersections}{\code{signature(x = "GPRsurvey")}: ... }
    \item{length}{\code{signature(x = "GPRsurvey")}: ... }
    \item{plot3D}{\code{signature(x = "GPRsurvey")}: ... }
    \item{plotDelineations3D}{\code{signature(x = "GPRsurvey")}: ... }
    \item{setCoordref}{\code{signature(x = "GPRsurvey")}: ... }
    \item{show}{\code{signature(object = "GPRsurvey")}: ... }
    \item{surveyIntersections}{\code{signature(x = "GPRsurvey")}: ... }
    \item{writeGPR}{\code{signature(x = "GPRsurvey")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("GPRsurvey")
}
\keyword{classes}
