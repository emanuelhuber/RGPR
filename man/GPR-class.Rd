\name{GPR-class}
\Rdversion{1.1}
\docType{class}
\alias{GPR-class}
\alias{[,GPR,ANY,ANY,ANY-method}
\alias{[<-,GPR,ANY,ANY,ANY-method}
\alias{ann,GPR-method}
\alias{ann<-,GPR-method}
\alias{apply,GPR-method}
\alias{Arith,ANY,GPR-method}
\alias{Arith,GPR,ANY-method}
\alias{Arith,GPR,GPR-method}
\alias{as.matrix,GPR-method}
\alias{clip,GPR-method}
\alias{coerce,GPR,matrix-method}
\alias{coord,GPR-method}
\alias{coord<-,GPR-method}
\alias{crs,GPR-method}
\alias{crs<-,GPR-method}
\alias{dcshift,GPR-method}
\alias{delineate,GPR-method}
\alias{delineations,GPR-method}
\alias{description,GPR-method}
\alias{dewow,GPR-method}
\alias{dewow2,GPR-method}
\alias{dim,GPR-method}
\alias{exportDelineations,GPR-method}
\alias{exportFID,GPR-method}
\alias{exportPDF,GPR-method}
\alias{fid,GPR-method}
\alias{fid<-,GPR-method}
\alias{filename,GPR-method}
\alias{firstBreack,GPR-method}
\alias{fkFilter,GPR-method}
\alias{freqFilter,GPR-method}
\alias{gain,GPR-method}
\alias{gammaCorrection,GPR-method}
\alias{getAmpl,GPR-method}
\alias{getData,GPR-method}
\alias{gethd,GPR-method}
\alias{identifyDelineation,GPR-method}
\alias{interpTraces,GPR-method}
\alias{length,GPR-method}
\alias{Math,GPR-method}
\alias{max,GPR-method}
\alias{mean,GPR-method}
\alias{medianFilter,GPR-method}
\alias{migration,GPR-method}
\alias{min,GPR-method}
\alias{name,GPR-method}
\alias{ncol,GPR-method}
\alias{nrow,GPR-method}
\alias{plot3D,GPR-method}
\alias{plotAmpl,GPR-method}
\alias{plotDelineations,GPR-method}
\alias{plotDelineations3D,GPR-method}
\alias{range,GPR-method}
\alias{reverse,GPR-method}
\alias{rmDelineations<-,GPR-method}
\alias{setData<-,GPR-method}
\alias{show,GPR-method}
\alias{showDelineations,GPR-method}
\alias{spec,GPR-method}
\alias{summary,GPR-method}
\alias{time0,GPR-method}
\alias{time0<-,GPR-method}
\alias{upsample,GPR-method}
\alias{writeGPR,GPR-method}

\title{Class \code{"GPR"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("GPR", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{traces}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{depth}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{pos}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{time0}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{time}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{com}:}{Object of class \code{"character"} ~~ }
    \item{\code{ann}:}{Object of class \code{"character"} ~~ }
    \item{\code{coord}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{rec}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{trans}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{coordref}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{ntr}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{w}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{freq}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{dz}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{dx}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{antsep}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{name}:}{Object of class \code{"character"} ~~ }
    \item{\code{description}:}{Object of class \code{"character"} ~~ }
    \item{\code{filename}:}{Object of class \code{"character"} ~~ }
    \item{\code{depthunit}:}{Object of class \code{"character"} ~~ }
    \item{\code{posunit}:}{Object of class \code{"character"} ~~ }
    \item{\code{surveymode}:}{Object of class \code{"character"} ~~ }
    \item{\code{date}:}{Object of class \code{"character"} ~~ }
    \item{\code{crs}:}{Object of class \code{"character"} ~~ }
    \item{\code{proc}:}{Object of class \code{"character"} ~~ }
    \item{\code{vel}:}{Object of class \code{"list"} ~~ }
    \item{\code{delineations}:}{Object of class \code{"list"} ~~ }
    \item{\code{hd}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "GPR", i = "ANY", j = "ANY", drop = "ANY")}: ... }
    \item{[<-}{\code{signature(x = "GPR", i = "ANY", j = "ANY", value = "ANY")}: ... }
    \item{ann}{\code{signature(x = "GPR")}: ... }
    \item{ann<-}{\code{signature(x = "GPR")}: ... }
    \item{apply}{\code{signature(X = "GPR")}: ... }
    \item{Arith}{\code{signature(e1 = "ANY", e2 = "GPR")}: ... }
    \item{Arith}{\code{signature(e1 = "GPR", e2 = "ANY")}: ... }
    \item{Arith}{\code{signature(e1 = "GPR", e2 = "GPR")}: ... }
    \item{as.matrix}{\code{signature(x = "GPR")}: ... }
    \item{clip}{\code{signature(x = "GPR")}: ... }
    \item{coerce}{\code{signature(from = "GPR", to = "matrix")}: ... }
    \item{coord}{\code{signature(x = "GPR")}: ... }
    \item{coord<-}{\code{signature(x = "GPR")}: ... }
    \item{crs}{\code{signature(x = "GPR")}: ... }
    \item{crs<-}{\code{signature(x = "GPR")}: ... }
    \item{dcshift}{\code{signature(x = "GPR")}: ... }
    \item{delineate}{\code{signature(x = "GPR")}: ... }
    \item{delineations}{\code{signature(x = "GPR")}: ... }
    \item{description}{\code{signature(x = "GPR")}: ... }
    \item{dewow}{\code{signature(x = "GPR")}: ... }
    \item{dewow2}{\code{signature(x = "GPR")}: ... }
    \item{dim}{\code{signature(x = "GPR")}: ... }
    \item{exportDelineations}{\code{signature(gpr = "GPR")}: ... }
    \item{exportFID}{\code{signature(x = "GPR")}: ... }
    \item{exportPDF}{\code{signature(x = "GPR")}: ... }
    \item{fid}{\code{signature(x = "GPR")}: ... }
    \item{fid<-}{\code{signature(x = "GPR")}: ... }
    \item{filename}{\code{signature(x = "GPR")}: ... }
    \item{firstBreack}{\code{signature(x = "GPR")}: ... }
    \item{fkFilter}{\code{signature(x = "GPR")}: ... }
    \item{freqFilter}{\code{signature(x = "GPR")}: ... }
    \item{gain}{\code{signature(x = "GPR")}: ... }
    \item{gammaCorrection}{\code{signature(x = "GPR")}: ... }
    \item{getAmpl}{\code{signature(x = "GPR")}: ... }
    \item{getData}{\code{signature(x = "GPR")}: ... }
    \item{gethd}{\code{signature(x = "GPR")}: ... }
    \item{identifyDelineation}{\code{signature(x = "GPR")}: ... }
    \item{interpTraces}{\code{signature(x = "GPR")}: ... }
    \item{length}{\code{signature(x = "GPR")}: ... }
    \item{Math}{\code{signature(x = "GPR")}: ... }
    \item{max}{\code{signature(x = "GPR")}: ... }
    \item{mean}{\code{signature(x = "GPR")}: ... }
    \item{medianFilter}{\code{signature(x = "GPR")}: ... }
    \item{migration}{\code{signature(x = "GPR")}: ... }
    \item{min}{\code{signature(x = "GPR")}: ... }
    \item{name}{\code{signature(x = "GPR")}: ... }
    \item{ncol}{\code{signature(x = "GPR")}: ... }
    \item{nrow}{\code{signature(x = "GPR")}: ... }
    \item{plot3D}{\code{signature(x = "GPR")}: ... }
    \item{plotAmpl}{\code{signature(x = "GPR")}: ... }
    \item{plotDelineations}{\code{signature(x = "GPR")}: ... }
    \item{plotDelineations3D}{\code{signature(x = "GPR")}: ... }
    \item{range}{\code{signature(x = "GPR")}: ... }
    \item{reverse}{\code{signature(x = "GPR")}: ... }
    \item{rmDelineations<-}{\code{signature(x = "GPR")}: ... }
    \item{setData<-}{\code{signature(x = "GPR")}: ... }
    \item{show}{\code{signature(object = "GPR")}: ... }
    \item{showDelineations}{\code{signature(x = "GPR")}: ... }
    \item{spec}{\code{signature(x = "GPR")}: ... }
    \item{summary}{\code{signature(object = "GPR")}: ... }
    \item{time0}{\code{signature(x = "GPR")}: ... }
    \item{time0<-}{\code{signature(x = "GPR")}: ... }
    \item{upsample}{\code{signature(x = "GPR")}: ... }
    \item{writeGPR}{\code{signature(x = "GPR")}: ... }
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
showClass("GPR")
}
\keyword{classes}
