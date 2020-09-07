#' Complex operations
#'
#' Methods for the base Complex methods \link[methods]{S4groupGeneric}: "Arg", "Conj", "Im", "Mod", "Re"
#' @param z An object of the class GPRvirtual
#' @rdname Complex-methods
#' @aliases Complex,GPRvirtual-method
setMethod(
  f = "Complex",
  signature = "GPRvirtual",
  definition = function(z){
    z@data <- switch(.Generic,
                     Arg  = Arg(z@data),
                     Conj = Conj(z@data),
                     Im   = Im(z@data),
                     Mod  = Mod(z@data),
                     Re   = Re(z@data),
                     stop(paste(.Generic, "not yet allowed on GPR objects"))
    )
    # proc(x) <- getArgs()
    return(z)
  }
)