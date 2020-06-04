# ?groupGeneric
# > getGroupMembers("Ops")
# > getGroupMembers("Arith")
# [1] "+"   "-"   "*"   "^"   "%%"  "%/%" "/" 
.GPR.add <- function(a, b){
  if(missing(b)){
    return(a)
  }
  #FIXME #TODO: case where a (or b) is a vector trace
  if(is(b,"GPRcube")){
    x <- b
    b <- b@data
  }
  if(is(a,"GPRcube")){
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
  if(is(b,"GPRcube")){
    x <- b
    b <- b@data
  }
  if(is(a,"GPRcube")){
    x <- a
    a <- a@data
  }
  x@data <- a - b
  return(x)
}
.GPR.mul <- function(a, b){
  if(is(b,"GPRcube")){
    x <- b
    b <- b@data
  }
  if(is(a,"GPRcube")){
    x <- a
    a <- a@data
  }
  x@data <- a * b
  return(x)
}
.GPR.div <- function(a, b){
  if(is(b,"GPRcube")){
    x <- b
    b <- b@data
  }
  if(is(a,"GPRcube")){
    x <- a
    a <- a@data
  }
  x@data <- a / b
  return(x)
}
.GPR.pow <- function(a, b){
  if(is(b,"GPRcube")){
    x <- b
    b <- b@data
  }
  if(is(a,"GPRcube")){
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
         stop(paste("binary operator \"", .Generic, "\" not defined for GPRcube"))
  )
}



#' Basic arithmetical functions
#'
#' 
#' @param e1 An object of the class GPRcube
#' @param e2 An object of the class GPRcube
# @examples
# data(frenkeLine00)
# A <- exp(frenkeLine00)
# B <- A + frenkeLine00
#' @rdname Arith-methods
#' @aliases Arith,GPRcube,ANY-method
setMethod(
  f = "Arith",
  signature = c(e1 = "GPRcube", e2 = "ANY"), 
  definition = .GPR.arith
)

#' @name Arith
#' @rdname Arith-methods
#' @aliases Arith,GPRcube,GPRcube-method
setMethod(
  f = "Arith",
  signature = c(e1 = "GPRcube", e2 = "GPRcube"), 
  definition = .GPR.arith
)
#' @name Arith
#' @rdname Arith-methods
#' @aliases Arith,ANY,GPRcube-method
setMethod(
  f = "Arith",
  signature = c(e1 = "ANY", e2 = "GPRcube"), 
  definition = .GPR.arith
)