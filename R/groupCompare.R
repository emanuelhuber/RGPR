.GPR.Compare <- function(e1,e2){
  if(is(e2, "GPR")){
    x <- e2
    e2 <- e2@data
  }
  if(is(e1, "GPR")){
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
                   stop(paste("Operator \"", .Generic, "not defined for GPR"))
  )
  return(x)
}

#' Comparison operations
#'
#' Methods for the base Compare methods \link[methods]{S4groupGeneric}: "==", ">", "<", "!=", "<=", ">="
#' @param e1 An object of the class GPR
#' @param e2 An object of the class GPR
#' @rdname Compare-methods
#' @aliases Compare,GPR,ANY-method
setMethod(
  f = "Compare",
  signature = c(e1 = "GPR", e2 = "ANY"), 
  definition = .GPR.Compare
)

#' @name Compare
#' @rdname Compare-methods
#' @aliases Compare,GPR,GPR-method
setMethod(
  f = "Compare",
  signature = c(e1 = "GPR", e2 = "GPR"), 
  definition = .GPR.Compare
)
#' @name Compare
#' @rdname Compare-methods
#' @aliases Compare,ANY,GPR-method
setMethod(
  f = "Compare",
  signature = c(e1 = "ANY", e2 = "GPR"), 
  definition = .GPR.Compare
)