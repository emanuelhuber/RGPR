# ?groupGeneric
# > getGroupMembers("Ops")
# > getGroupMembers("Arith")
# [1] "+"   "-"   "*"   "^"   "%%"  "%/%" "/" 
.GPR.add <- function(a, b){
  #FIXME #TODO: case where a (or b) is a vector trace
  if(is(b,"GPR")){
    x <- b
    b <- b@data
  }
  if(is(a,"GPR")){
    x <- a
    a <- a@data
  }
  x@data <- a + b
  return(x)
}
.GPR.sub <- function(a, b){
  if(is(b,"GPR")){
    x <- b
    b <- b@data
  }
  if(is(a,"GPR")){
    x <- a
    a <- a@data
  }
  if(!is.null(dim(a)) && !is.null(dim(b))){
    if(dim(a)[2] == 1 && dim(b)[2] > 1){
      a <- as.vector(a)
    }else if(dim(b)[2] == 1 && dim(a)[2] > 1){
      b <- as.vector(b)
    }
  }
  x@data <- a - b
  return(x)
}
.GPR.mul <- function(a, b){
  if(is(b,"GPR")){
    x <- b
    b <- b@data
  }
  if(is(a,"GPR")){
    x <- a
    a <- a@data
  }
  x@data <- a * b
  return(x)
}
.GPR.div <- function(a, b){
  if(is(b,"GPR")){
    x <- b
    b <- b@data
  }
  if(is(a,"GPR")){
    x <- a
    a <- a@data
  }
  x@data <- a / b
  return(x)
}
.GPR.pow <- function(a, b){
  if(is(b,"GPR")){
    x <- b
    b <- b@data
  }
  if(is(a,"GPR")){
    x <- a
    a <- a@data
  }
  x@data <- a ^ b
  return(x)
}

.GPR.arith <- function(e1,e2){
  switch(.Generic,
         "+" = .GPR.add(e1, e2),
         "-" = .GPR.sub(e1, e2),
         "*" = .GPR.mul(e1, e2),
         "/" = .GPR.div(e1, e2),
         "^" = .GPR.pow(e1, e2),
         stop(paste("binary operator \"", .Generic, "\" not defined for GPR"))
  )
}



#' Basic arithmetical functions
#'
#' 
#' @param e1 An object of the class RGPR.
#' @param e2 An object of the class RGPR.
#' @examples
#' data(frenkeLine00)
#' A <- exp(frenkeLine00)
#' B <- A + frenkeLine00
#' @rdname Arith-methods
#' @aliases Arith,GPR,ANY-method
setMethod(
  f= "Arith",
  signature=c(e1="GPR",e2="ANY"), 
  definition=.GPR.arith
)

#' @name Arith
#' @rdname Arith-methods
#' @aliases Arith,GPR,GPR-method
setMethod(
  f= "Arith",
  signature=c(e1="GPR",e2="GPR"), 
  definition=.GPR.arith
)
#' @name Arith
#' @rdname Arith-methods
#' @aliases Arith,ANY,GPR-method
setMethod(
  f= "Arith",
  signature=c(e1="ANY",e2="GPR"), 
  definition=.GPR.arith
)
