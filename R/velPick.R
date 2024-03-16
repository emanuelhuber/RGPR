#' Pick velocity interactively
#' 
#' Pick velocity interactively on a previously plotted velocity spectrum. The
#' picked velocity are NMO velocities which are a good approximation for
#' RMS velocities. The internal velocities are estimated from the NMO 
#' velocities through the Dix's formula \code{velPick()} add the NMO velocities
#' as RMS velocity as well as the internal velocities to \code{x}.
#' 
#' Application of the Dix's formula can provide non-real velocities, 
#' if the travel time intervals are small or
#' if the NMO velocity change is large. In this case, this 
#' 
#' @param x [\code{GPR class}] An object of the class \code{GPR}
#' @param ... additional graphics parameters used if type != "n" for plotting the locations
#' @return [\code{GPR class}] An object of the class \code{GPR}.
#' @name velPick
#' @rdname velPick
setGeneric("velPick", function(x, ...) standardGeneric("velPick"))

#' @rdname velPick
#' @export
setMethod("velPick", "GPR", function(x, ...){
  dots <- list(...)
  type <- "o"
  pch <- 4
  col <- "firebrick"
  cex <- 2
  if(length(dots) > 0){
    if(!is.null(dots$type)){
      type <-  dots$type
      dots$type <- NULL
    } 
    if(!is.null(dots$pch)){
      pch <-  dots$pch
      dots$pch <- NULL
    }     
    if(!is.null(dots$col)){
      col <-  dots$col
      dots$col <- NULL
    } 
    if(!is.null(dots$cex)){
      cex <-  dots$cex
      dots$cex <- NULL
    } 
  }
  vv <- locator(type = type, pch = pch, col = col, cex = cex, ...)
  if(length(vv) > 0){
    x <- setVel(x, v = vv$x, twt = vv$y, type = "vrms")
  }
  return(x)
})