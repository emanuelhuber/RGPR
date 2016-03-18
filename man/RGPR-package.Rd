\name{RGPR-package}
\alias{RGPR-package}
\alias{RGPR}
\docType{package}
\title{
What the package does (short line)
RGPR
}
\description{
More about what it does (maybe more than one line)
~~ A concise (1-5 lines) description of the package ~~
}
\details{
\tabular{ll}{
Package: \tab RGPR\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-05-06\cr
License: \tab What license is it under?\cr
Depends: \tab methods\cr
}

Do like raster package:

The package implements classes for Raster data (see \link{Raster-class}) and supports
  \itemize{
    \item Creation of Raster* objects from scratch or from file
    \item Handling extremely large raster files
    \item Reading and writing various raster file types
  }
  
  
\section{I. Creating Raster* objects}{
RasterLayer, RasterStack, and RasterBrick objects are, as a group, referred to as Raster* objects. Raster* objects can be created, from scratch, files, or from objects of other classes, with the following functions:
  \tabular{ll}{
    \code{\link{raster}}\tab To create a RasterLayer \cr
    \code{\link{stack}} \tab To create a RasterStack (multiple layers)\cr
    \code{\link{brick}} \tab To create a RasterBrick (multiple layers)\cr
    \code{\link{subset}} \tab Select layers of a RasterStack/Brick\cr
    \code{\link{addLayer}} \tab Add a layer to a Raster* object\cr
    \code{\link{dropLayer}} \tab Remove a layer from a RasterStack or RasterBrick \cr
    \code{\link{unstack}} \tab Create a list of RasterLayer objects from a RasterStack \cr
  --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr  
}
}


\section{XIII. Writing files}{
  \tabular{ll}{
   \bold{Basic}\cr
    \code{\link{setValues}} \tab Put new values in a Raster* object \cr
    \code{\link{writeRaster}} \tab Write all values of Raster* object to disk \cr
    \code{\link{KML}} \tab Save raster as KML file \cr	
   .\cr
   \bold{Advanced}\cr
   \code{\link{blockSize}} \tab Get suggested block size for reading and writing \cr
   \code{\link{writeStart}} \tab Open a file for writing \cr
   \code{\link{writeValues}} \tab Write some values  \cr
   \code{\link{writeStop}} \tab Close the file after writing \cr
   \code{\link{update}} \tab Change the values of an existing file \cr	
    --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
}
}	


\section{XVI. Miscellaneous}{
\tabular{ll}{
    \code{\link{rasterOptions}} \tab Show, set, save or get session options \cr
    \code{\link{getData}} \tab Download and geographic data\cr
    \code{\link{pointDistance}} \tab Distance between points \cr
    \code{\link{readIniFile}} \tab Read a (windows) 'ini' file \cr
    \code{\link{hdr}} \tab Write header file for a number of raster formats \cr
    \code{\link{trim}} \tab Remove leading and trainling blanks from a character string \cr
    \code{\link{extension}} \tab Get or set the extentsion of a filename \cr
    \code{\link{cv}} \tab Coefficient of variation  \cr
    \code{\link{modal}} \tab Modal value  \cr
    \code{\link{sampleInt}} \tab Random sample of (possibly very large) range of integer values \cr
    \code{\link{showTmpFiles}} \tab Show temporary files \cr
    \code{\link{removeTmpFiles}} \tab Remove temporary files \cr
    --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
}
}
\section{XVII. For programmers}{
\tabular{ll}{
  \code{\link{canProcessInMemory}} \tab Test whether a file can be created in memory \cr
  \code{\link{pbCreate}} \tab Initialize a progress bar  \cr
  \code{\link{pbStep}} \tab Take a progress bar step \cr
  \code{\link{pbClose}} \tab Close a progress bar \cr
  \code{\link{readStart}} \tab Open file connections for efficient multi-chunck reading \cr
  \code{\link{readStop}} \tab Close file connections \cr
  \code{\link{rasterTmpFile}}  \tab Get a name for a temporary file \cr
  \code{\link{inMemory}} \tab Are the cell values in memory? \cr
  \code{\link{fromDisk}} \tab Are the cell values read from a file? \cr	
    --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
}
}
\author{Except where indicated otherwise, the functions in this package were written by Robert J. Hijmans}
\section{Acknowledgements}{
Extensive contributions were made by Jacob van Etten, Jonathan Greenberg, Matteo Mattiuzzi, and Michael Sumner. Significant help was also provided by Phil Heilman, Agustin Lobo, Oscar Perpinan Lamigueiro, Stefan Schlaffer, Jon Olav Skoien, Steven Mosher, and Kevin Ummel. Contributions were also made by Jochen Albrecht, Neil Best, Andrew Bevan, Roger Bivand, Isabelle Boulangeat, Lyndon Estes, Josh Gray, Tim Haering, Herry Herry, Paul Hiemstra, Ned Hornig, Mayeul Kauffmann, Bart Kranstauber, Rainer Krug, Alice Laborte, John Lewis, Lennon Li, Justin McGrath, Babak Naimi, Carsten Neumann, Joshua Perlman, Richard Plant, Edzer Pebesma, Etienne Racine, David Ramsey, Shaun Walbridge, Julian Zeidler and many others.
}



}
\author{
Who wrote it

Maintainer: Who to complain to <yourfault@somewhere.net>
~~ The author and/or maintainer of the package ~~
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R ~~
~~ documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
~~ simple examples of the most important functions ~~
}
