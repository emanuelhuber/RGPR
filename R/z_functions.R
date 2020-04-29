
# https://stackoverflow.com/questions/50561768/r-get-argument-names-from-function-call
# Using the same formalArgs suggested by @Akrun 
# (and also in the almost duplicate Get the argument names of an R function):
#   
#   getArgNames <- function(value) formalArgs(deparse(substitute(value)[[1]]))
# 
# substitute(value) quotes the input, to prevent immediate evaluation, [[1]] 
# retrieves the function from the parsed input, deparse turns it into character 
# (since formalArgs can take the function name as character).
# 
# getArgNames(testFun())
# 
# #[1] "x" "z"



# http://stackoverflow.com/questions/17256834/getting-the-arguments-of-a-parent-
# function-in-r-with-names
# Ryan Grannell
# website   twitter.com/RyanGrannell
# location   Galway, Ireland
# @export
getArgs <- function (returnCharacter = TRUE, addArgs = NULL) {
  # print(sys.nframe())
  # 50 -> 1 error with devtools::test() and opencpu
  # 100 -> works with devtools::test() and does not work with opencpu
  if(sys.nframe() >= 2){
    arg <- as.list(match.call(definition = sys.function( -1 ),
                              call = sys.call(-1),
                              expand.dots = TRUE )
    )
    narg <- length(arg)
    if(returnCharacter){
      if(narg  >= 3){
        eval_arg <- tryCatch(sapply(arg[3:narg], eval, simplify = FALSE),
                             error = function(cond){return(NULL)})
        if(!is.null(eval_arg)){                     
          argChar <- paste0(arg[[1]],"//", 
                            paste(names(arg[3:narg]), 
                                  mapply(pasteArgs, eval_arg, arg[3:narg]), 
                                  #sapply(eval_arg, pasteArgs, arg[3:narg]), 
                                  sep = "=", collapse = "+"))
        }else{
          return(c())
        }  
      }else{
        argChar <- paste0(arg[[1]],"//")
      }
      if(!is.null(addArgs)){
        argChar <- addArg(argChar, addArgs)
      }
      return(argChar)
    }else{
      return(arg)
    }
  }else{
    message("getargs rerror, frame = ", sys.nframe())
  }
}

pasteArgs <- function(eval_arg, arg){
  arg <- deparse((arg))
  # print(deparse(eval_arg))
  # print(class(eval_arg))
  if(class(eval_arg) == "function" || class(eval_arg) == "standardGeneric"){
    return(arg)
  }else if(is.list(eval_arg)){
    return( paste0(names(eval_arg), "<-", (eval_arg), collapse = "," ) )
  }else if(is.matrix(eval_arg)){
    return(paste(arg))
    # if eval_arg == "1:10", returns "1:10" instead of "1,2,3,4,5,6,7,8,9,10"
  }else if(is.null(eval_arg)){
    return("NULL")
  }else if(all(grepl(pattern = '^([[:digit:]]+):([[:digit:]]+)$', arg))){
    return(paste0(arg))
  }else{
    return( paste0(eval_arg, collapse = ",") )
  }
}


addArg <- function(proc, arg){
  proc_add <- paste(names(arg), sapply(pasteArgs, arg, arg),
                    sep = "=", collapse = "+")
  if(substr(proc, nchar(proc), nchar(proc)) == "//"){
    proc <- paste(proc, proc_add, sep = "")
  }else{
    proc <- paste(proc, "+", proc_add, sep = "")
  }
  return(proc)
}

# return a character vector containing the name of the FUN function
getFunName <- function(FUN){
  if(class(FUN) == "function"){
    funName <- "FUN"
  }else{
    #  if(isGeneric("FUN")){
    funName0 <- selectMethod(FUN, "numeric")
    funName <-funName0@generic[1]
  }
  return(funName)
}


# wapply: A faster (but less functional) "rollapply" for vector setups

# April 23, 2013.
# By A.N. Spiess, senior scientist at the Department of Andrology at the 
# University Hospital Hamburg-Eppendorf.
# This is what turned out (wapply for "window apply").
# @export
# x = vector!
wapply <- function(x=NULL, width = NULL, by = NULL, FUN = NULL, ...){
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width
  lenX <- length(x)
  SEQ1 <- seq(1, lenX - width + 1, by = by)
  SEQ2 <- lapply(SEQ1, function(x) x:(x + width - 1))
  
  OUT <- lapply(SEQ2, function(a) FUN(x[a], ...))
  OUT <- base::simplify2array(OUT, higher = TRUE)
  return(OUT)
}

# centered wapply
# @export
# x = vector!
wapplyC <- function(x=NULL, width = NULL, by = NULL, FUN = NULL, ...){
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width
  lenX <- length(x)
  # SEQ1 <- seq(1, lenX - width + 1, by = by)
  SEQ1 <- seq(-(width - 1)/2 + 1, lenX -(width - 1)/2, by = by)
#  SEQ2 <- lapply(SEQ1, function(x) x:(x + width - 1))
  SEQ2 <- lapply(SEQ1, function(x){ 
                          xnew <- x:(x + width - 1)
                          xnew <- xnew[xnew > 0]
                          xnew <- xnew[xnew <= lenX]})
  
  OUT <- lapply(SEQ2, function(a) FUN(x[a], ...))
  OUT <- base::simplify2array(OUT, higher = TRUE)
  return(OUT)
}


# x = matrix
# width uneven!! 3, 5, 7 or ...
# Wapply on the row of a matrix (windowed + CENTERED)
wapplyRowC <- function(x = NULL, width = NULL, by = NULL, FUN = NULL, ...){
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width
  lenX <- nrow(x)
  SEQ1 <- seq(-(width - 1)/2 + 1, lenX -(width - 1)/2, by = by)
  SEQ2 <- lapply(SEQ1, function(x){ 
                                   xnew <- x:(x + width - 1)
                                   xnew <- xnew[xnew > 0]
                                   xnew <- xnew[xnew <= lenX]})
  
  OUT <- lapply(SEQ2, function(a) FUN(x[a,,drop=FALSE], ...))
  OUT <- base::simplify2array(OUT, higher = TRUE)
  return(OUT)
}

# FIXME CHECK MARGIN
# windowing with centered window
#
# based on wapply and modified by Manu.
# centered moving window.
# return a matrix of the same dimension than x.
# some border effect at a distance < width/2 at the first and last col/row
# @export
wapplyMat <- function(x = NULL, width = NULL, by = NULL, FUN = NULL, 
                      MARGIN = 1, ...){
  warning("MARGIN are corrected, use 1 instead of 2 and vice versa!!")
  FUN <- match.fun(FUN)
  width <- ifelse(width %% 2 == 0, width + 1, width)
  if (is.null(by)) by <- width
  lenX <- ifelse(MARGIN == 1, nrow(x), ncol(x))
  SEQ1 <- seq(-(width-1)/2 + 1, lenX -(width-1)/2, by = by)
  SEQ2 <- lapply(SEQ1, function(x){ xnew <- x:(x + width - 1)
  xnew <- xnew[xnew > 0]
  xnew <- xnew[xnew <= lenX]})
  if(MARGIN == 2){
    OUT <- lapply(SEQ2, function(a) apply(x[, a, drop = FALSE], MARGIN, FUN, ...))
  }else if( MARGIN == 1) {
    OUT <- lapply(SEQ2, function(a) apply(x[a,, drop = FALSE], MARGIN, FUN, ...))
  }
  OUT <- base::simplify2array(OUT, higher = TRUE)
  if(MARGIN == 1){
    return(t(OUT))
  }else{
    return(OUT)
  }
}
