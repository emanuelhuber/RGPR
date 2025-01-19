
#' Smooth velocity model
#' 
#' Define the smoothing parameters that will be used when the velocities 
#' will be plotted or used in other functions. 
#' To undo smoothing, set `w = NULL`.
#' @param x (`GPR class`) An object of the class `GPR`
#' @param type (`character[1]`) Which type of velocity values has to be
#'             updated? The root-mean-square velocity (`vrms`) or the
#'             internal velocity (`vint`)?
#' @param w (`numeric[1]|NULL`) Standard deviation of the
#'          standard deviation of the smoothing kernel. If `w = NULL`,
#'          no smoothing will be applied.
#' @return (`GPR class`) An object of the class GPR.
#' @name velSmooth
setGeneric("velSmooth",  function(x, type = c("vrms", "vint"), w)
  standardGeneric("velSmooth"))
           
           
#' @rdname velSmooth
#' @export
setMethod("velSmooth", "GPR", function(x, type = c("vrms", "vint"), w){
  type <- match.arg(type, c("vrms", "vint"))
  x@vel[[type]][["smooth"]] <- w
  return(x)
})
