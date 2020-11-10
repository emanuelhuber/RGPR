
#' Smooth velocity model
#' 
#' Smooth velocity model
#' @export
smoothVel <- function(x, w, type = c("vrms", "vint")){
  type <- match.arg(type, c("vrms", "vint"))
  # v_stairs <- approxfun(x@vel[[type]][["t"]], x@vel[[type]][["v"]], 
  # rule = 2, method = "constant", f = 1)
  # mmand::gaussianSmooth(v_stairs(x@z), sigma = w)
  x@vel[[type]][["smooth"]] <- w
  return(x)
}
