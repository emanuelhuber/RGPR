.GPR.Compare <- function(e1,e2){
  if(is(e2, "GPRvirtual")){
    x <- e2
    e2 <- e2@data
  }
  if(is(e1, "GPRvirtual")){
    x <- e1
    e1 <- e1@data
  }
  x@data <- switch(.Generic,
                   "==" = e1 == e2,
                   ">"  = e1 >  e2,
                   "<"  = e1 <  e2,
                   "!=" = e1 != e2,
                   "<=" = e1 <= e2,
                   ">=" = e1 >= e2,
                   stop(paste("Operator \"", .Generic, "not defined for GPRvirtual"))
  )
  return(x)
}

#' Comparison operations
#'
#' Methods for the base Compare methods \link[methods]{S4groupGeneric}: "==", ">", "<", "!=", "<=", ">="
#' @param e1 An object of the class GPRvirtual
#' @param e2 An object of the class GPRvirtual
#' @rdname Compare-methods
#' @aliases Compare,GPRvirtual,ANY-method
setMethod(
  f = "Compare",
  signature = c(e1 = "GPRvirtual", e2 = "ANY"), 
  definition = .GPR.Compare
)

#' @name Compare
#' @rdname Compare-methods
#' @aliases Compare,GPRvirtual,GPRvirtual-method
setMethod(
  f = "Compare",
  signature = c(e1 = "GPRvirtual", e2 = "GPRvirtual"), 
  definition = .GPR.Compare
)
#' @name Compare
#' @rdname Compare-methods
#' @aliases Compare,ANY,GPRvirtual-method
setMethod(
  f = "Compare",
  signature = c(e1 = "ANY", e2 = "GPRvirtual"), 
  definition = .GPR.Compare
)

