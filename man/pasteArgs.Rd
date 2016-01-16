\name{pasteArgs}
\alias{pasteArgs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pasteArgs(eval_arg, arg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eval_arg}{
%%     ~~Describe \code{eval_arg} here~~
}
  \item{arg}{
%%     ~~Describe \code{arg} here~~
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
function (eval_arg, arg) 
{
    if (is.numeric(eval_arg) || is.character(eval_arg)) {
        return(paste(eval_arg, collapse = ",", sep = ""))
    }
    else if (is.list(eval_arg)) {
        return(paste(names(eval_arg), "<-", (eval_arg), collapse = ",", 
            sep = ""))
    }
    else if (is.matrix(eval_arg)) {
        return(paste(arg))
    }
    else if (any(is.null(eval_arg))) {
        return("")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
