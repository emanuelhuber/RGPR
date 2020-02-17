
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

