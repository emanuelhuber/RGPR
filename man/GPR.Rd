\name{GPR}
\alias{GPR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GPR(x, name = "", description = "", filename = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{name}{
%%     ~~Describe \code{name} here~~
}
  \item{description}{
%%     ~~Describe \code{description} here~~
}
  \item{filename}{
%%     ~~Describe \code{filename} here~~
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
function (x, name = "", description = "", filename = "") 
{
    rec_coord <- cbind(x$dt1$recx, x$dt1$recy, x$dt1$recz)
    trans_coord <- cbind(x$dt1$transx, x$dt1$transy, x$dt1$transz)
    if (sum(is.na(rec_coord)) > 0) {
        warning(paste(sum(is.na(rec_coord)), "NA's in the receiver coordinates\n"))
    }
    if (sum(is.na(trans_coord)) > 0) {
        warning(paste(sum(is.na(trans_coord)), "NA's in the transmitter coordinates\n"))
    }
    if (sum(is.na(x$dt1$topo)) > 0) {
        warning(paste(sum(is.na(x$dt1$topo)), "NA's in the topo coordinates\n"))
    }
    if (sum(abs(rec_coord), na.rm = TRUE) == 0) {
        rec_coord <- matrix(nrow = 0, ncol = 0)
    }
    if (sum(abs(trans_coord), na.rm = TRUE) == 0) {
        trans_coord <- matrix(nrow = 0, ncol = 0)
    }
    if (sum(abs(x$dt1$topo), na.rm = TRUE) == 0) {
        coord <- matrix(nrow = 0, ncol = 0)
    }
    else {
        coord <- matrix(0, nrow = ncol(x$data), ncol = 3)
        coord[, 3] <- x$dt1$topo
    }
    dz <- getHD(x$hd, "TOTAL TIME WINDOW")/getHD(x$hd, "NUMBER OF PTS/TRC")
    if (sum(abs(x$dt1$time0)) == 0) {
        time0 <- rep(getHD(x$hd, "TIMEZERO AT POINT") * dz, ncol(x$data))
    }
    else {
        time0 <- x$dt1$time0
    }
    if (!grepl("^([0-9]{4})(-)([0-9]{2})(-)([0-9]{2})", x$hd[3, 
        2])) {
        d <- "1970-01-01"
    }
    else {
        d <- x$hd[3, 2]
    }
    myT <- as.double(as.POSIXct(x$dt1$time, origin = as.Date(d)))
    GPR_device <- x$hd[2, 2]
    if (!grepl("^(Data.)", GPR_device)) {
        GPR_device <- ""
    }
    hd_list <- list(startpos = getHD(x$hd, "STARTING POSITION"), 
        endpos = getHD(x$hd, "FINAL POSITION"), nstacks = getHD(x$hd, 
            "NUMBER OF STACKS"), nstacks = getHD(x$hd, "NUMBER OF STACKS"), 
        gprdevice = GPR_device)
    if (nrow(x$hd) > 17) {
        key <- trim(x$hd[, 1])
        test <- key != "" & seq_along(key) > 17
        key <- key[test]
        key2 <- gsub("[[:punct:]]", replacement = "", key)
        key2 <- gsub(" ", replacement = "_", key2)
        nameL <- trim(x$hd[test, 2])
        names(nameL) <- as.character(key2)
        hd_list_supp <- as.list(nameL)
        hd_list <- c(hd_list, hd_list_supp)
    }
    new("GPR", data = byte2volt() * x$data, traces = x$dt1$traces, 
        com = x$dt1$com, coord = coord, pos = x$dt1$pos, depth = seq(0, 
            by = dz, length.out = nrow(x$data)), rec = rec_coord, 
        trans = trans_coord, time0 = time0, time = myT, proc = character(0), 
        vel = list(0.1), name = name, description = description, 
        filename = filename, ntr = ncol(x$data), w = getHD(x$hd, 
            "TOTAL TIME WINDOW"), dz = dz, dx = getHD(x$hd, "STEP SIZE USED"), 
        depthunit = "ns", posunit = getHD(x$hd, "POSITION UNITS", 
            number = FALSE), freq = getHD(x$hd, "NOMINAL FREQUENCY"), 
        antsep = getHD(x$hd, "ANTENNA SEPARATION"), surveymode = getHD(x$hd, 
            "SURVEY MODE", number = FALSE), date = d, crs = "", 
        hd = hd_list)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
