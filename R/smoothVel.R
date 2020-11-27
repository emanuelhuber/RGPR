
#' Smooth velocity model
#' 
#' Define the smoothing parameters that will be used when the velocities 
#' will be plotted or used in other functions. 
#' To undo smoothing, set \code{w = NULL}.
#' @param x [\code{GPR class}] An object of the class \code{GPR}
#' @param type [\code{character(1)}] Which type of velocity values has to be
#'             updated? The root-mean-square velocity (\code{vrms}) or the
#'             internal velocity (\code{vint})?
#' @param w [\code{numeric(1)}|\code{NULL}] Standard deviation of the
#'          standard deviation of the smoothing kernel. If \code{w = NULL},
#'          no smoothing will be applied.
#' @return [\code{GPR class}] An object of the class GPR.
#' @name smoothVel
setGeneric("smoothVel",  function(x, type = c("vrms", "vint"), w)
  standardGeneric("smoothVel"))
           
           
#' @rdname smoothVel
#' @export
setMethod("smoothVel", "GPR", function(x, type = c("vrms", "vint"), w){
  type <- match.arg(type, c("vrms", "vint"))
  x@vel[[type]][["smooth"]] <- w
  return(x)
})
