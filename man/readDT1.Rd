\name{readDT1}
\alias{readDT1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
readDT1(filePath)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filePath}{
%%     ~~Describe \code{filePath} here~~
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
function (filePath) 
{
    dirName <- dirname(filePath)
    splitBaseName <- unlist(strsplit(basename(filePath), "[.]"))
    baseName <- paste(splitBaseName[1:(length(splitBaseName) - 
        1)], sep = "")
    fileNameHD <- paste(dirName, "/", baseName, ".HD", sep = "")
    fileNameDT1 <- paste(dirName, "/", baseName, ".DT1", sep = "")
    headHD <- scan(fileNameHD, what = character(), strip.white = TRUE, 
        quiet = TRUE, fill = TRUE, blank.lines.skip = TRUE, flush = TRUE, 
        sep = "\n")
    nHD <- length(headHD)
    headerHD <- data.frame(nrow = nHD, ncol = 2)
    for (i in seq_along(headHD)) {
        hdline <- strsplit(headHD[i], "=")[[1]]
        if (length(hdline) < 2) {
            headerHD[i, 1] <- ""
            headerHD[i, 2] <- trim(hdline[1])
        }
        else {
            headerHD[i, 1:2] <- as.character(sapply(hdline[1:2], 
                trim))
        }
    }
    nbTraces = as.integer(as.character(headerHD[4, 2]))
    nbPt = as.integer(as.character(headerHD[5, 2]))
    dt1 <- file(fileNameDT1, "rb")
    indexDT1Header = c("traces", "position", "samples", "topo", 
        "NA1", "bytes", "tracenb", "stack", "window", "NA2", 
        "NA3", "NA4", "NA5", "NA6", "recx", "recy", "recz", "transx", 
        "transy", "transz", "time0", "zeroflag", "NA7", "time", 
        "x8", "com")
    headerDT1 = list()
    myData = matrix(NA, nrow = nbPt, ncol = nbTraces)
    for (i in 1:nbTraces) {
        for (j in 1:25) {
            headerDT1[[indexDT1Header[j]]][i] = readBin(dt1, 
                what = numeric(), n = 1L, size = 4)
        }
        headerDT1[[indexDT1Header[26]]][i] = readChar(dt1, 28)
        myData[, i] = readBin(dt1, what = integer(), n = nbPt, 
            size = 2)
    }
    close(dt1)
    return(list(hd = headerHD, dt1hd = headerDT1, data = myData))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
