#' Round and signif
#'
#' Methods for the base Math2 methods \link[methods]{S4groupGeneric}: "round", "signif"
#' @param x An object of the class GPRvirtual
#' @param digits number of digits to be used in round or signif.
#' @rdname Math2-methods
#' @aliases Math2,GPRvirtual-method
setMethod(
  f = "Math2",
  signature = "GPRvirtual",
  definition = function(x, digits){
    x@data <- switch(.Generic,
                     round    = round(x@data, digits),
                     signif   = signif(x@data, digits),
                     stop(paste(.Generic, "not yet allowed on GPR objects"))
    )
    # proc(x) <- getArgs()
    return(x)
  }
)