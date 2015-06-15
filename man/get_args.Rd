\name{get_args}
\alias{get_args}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_args(return_character = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{return_character}{
%%     ~~Describe \code{return_character} here~~
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
function (return_character = TRUE) 
{
    arg <- as.list(match.call(def = sys.function(-1), call = sys.call(-1), 
        expand.dots = TRUE))
    narg <- length(arg)
    if (return_character) {
        if (narg >= 3) {
            eval_arg <- sapply(arg[3:narg], eval)
            paste(arg[[1]], ":", paste(names(arg[3:narg]), sapply(eval_arg, 
                pasteArgs, arg[3:narg]), sep = "=", collapse = "+"), 
                sep = "")
        }
        else {
            paste(arg[[1]], ":", sep = "")
        }
    }
    else {
        return(arg)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
