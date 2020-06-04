# ?groupGeneric
# > getGroupMembers("Ops")
# > getGroupMembers("Arith")
# [1] "+"   "-"   "*"   "^"   "%%"  "%/%" "/" 
.GPR.add <- function(a, b){
  if(missing(b)){
    return(a)
  }
  #FIXME #TODO: case where a (or b) is a vector trace
  if(is(b,"GPRslice")){
    x <- b
    b <- b@data
  }
  if(is(a,"GPRslice")){
    x <- a
    a <- a@data
  }
  x@data <- a + b
  return(x)
}
.GPR.sub <- function(a, b){
  if(missing(b)){
    a@data <- -a@data
    return(a)
  }
  if(is(b,"GPRslice")){
    x <- b
    b <- b@data
  }
  if(is(a,"GPRslice")){
    x <- a
    a <- a@data
  }
  x@data <- a - b
  return(x)
}
.GPR.mul <- function(a, b){
  if(is(b,"GPRslice")){
    x <- b
    b <- b@data
  }
  if(is(a,"GPRslice")){
    x <- a
    a <- a@data
  }
  x@data <- a * b
  return(x)
}
.GPR.div <- function(a, b){
  if(is(b,"GPRslice")){
    x <- b
    b <- b@data
  }
  if(is(a,"GPRslice")){
    x <- a
    a <- a@data
  }
  x@data <- a / b
  return(x)
}
.GPR.pow <- function(a, b){
  if(is(b,"GPRslice")){
    x <- b
    b <- b@data
  }
  if(is(a,"GPRslice")){
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
         stop(paste("binary operator \"", .Generic, "\" not defined for GPRslice"))
  )
}



#' Basic arithmetical functions
#'
#' 
#' @param e1 An object of the class GPRslice
#' @param e2 An object of the class GPRslice
# @examples
# data(frenkeLine00)
# A <- exp(frenkeLine00)
# B <- A + frenkeLine00
#' @rdname Arith-methods
#' @aliases Arith,GPRslice,ANY-method
setMethod(
  f = "Arith",
  signature = c(e1 = "GPRslice", e2 = "ANY"), 
  definition = .GPR.arith
)

#' @name Arith
#' @rdname Arith-methods
#' @aliases Arith,GPRslice,GPRslice-method
setMethod(
  f = "Arith",
  signature = c(e1 = "GPRslice", e2 = "GPRslice"), 
  definition = .GPR.arith
)
#' @name Arith
#' @rdname Arith-methods
#' @aliases Arith,ANY,GPRslice-method
setMethod(
  f = "Arith",
  signature = c(e1 = "ANY", e2 = "GPRslice"), 
  definition = .GPR.arith
)