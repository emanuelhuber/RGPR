#' Logical negation
#' 
#' Apply logical negation to GPR data
#' @param x [\code{GPRvirtual}]
#' @name logicalNegation
#' @aliases !,GPRvirtual-method
#' @rdname logicalNegation-GPRvirtual
#' @export
setMethod("!", "GPRvirtual", function(x){
  x@data <- !x@data
  return(x)
}
)