.GPR.Logic <- function(e1,e2){
  if(is(e2, "GPRvirtual")){
    x <- e2
    e2 <- e2@data
  }
  if(is(e1, "GPRvirtual")){
    x <- e1
    e1 <- e1@data
  }
  x@data <- switch(.Generic,
                   "&" = e1 & e2,
                   "|"  = e1 |  e2,
                   stop(paste("Operator \"", .Generic, "\" not defined for GPRvirtual"))
  )
  return(x)
}

#' Logic operations
#'
#' Methods for the base Logic methods [S4groupGeneric][methods::S4groupGeneric]: "==", ">", "<", "!=", "<=", ">="
#' @param e1 An object of the class GPRvirtual
#' @param e2 An object of the class GPRvirtual
#' @rdname Logic-methods
#' @aliases Logic,GPRvirtual,ANY-method
setMethod(
  f = "Logic",
  signature = c(e1 = "GPRvirtual", e2 = "ANY"), 
  definition = .GPR.Logic
)

#' @name Logic
#' @rdname Logic-methods
#' @aliases Logic,GPRvirtual,GPRvirtual-method
setMethod(
  f = "Logic",
  signature = c(e1 = "GPRvirtual", e2 = "GPRvirtual"), 
  definition = .GPR.Logic
)
#' @name Logic
#' @rdname Logic-methods
#' @aliases Logic,ANY,GPRvirtual-method
setMethod(
  f = "Logic",
  signature = c(e1 = "ANY", e2 = "GPRvirtual"), 
  definition = .GPR.Logic
)


