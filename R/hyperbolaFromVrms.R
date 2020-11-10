

#' Returns hyperbola associated with Vrms velociies
#' 
#' Returns hyperbola associated with Vrms velociies
#' @export
hyperbolaFromVrms <- function(x){
  if(is.null(x@vel[["vrms"]])){
    stop("You must first set v_rms velocities with 'setVel()'")
  }
  y <- mapply(hyperbolicTWT, x@vel[["vrms"]]$t, x@vel[["vrms"]]$v, 
              MoreArgs = list(antsep = x@x))
  list(twt = x@x, antsep = y)
}