

#' Plot hyperbolas associated with Vrms velociies
#' 
#' Plot hyperbolas associated with root-mean-square velocities stored in
#' the CMP data.
#' @param x [\code{GPR class}] A CMP object of the class \code{GPR}
#' @param lty [\code{numeric(1)}] The line type 
#'            (default is \code{1} for solid lines). See \code{\link[graphics]{par}}
#' @param lwd [\code{numeric(1)}] Line width
#'             (default is \code{1}). See \code{\link[graphics]{par}}
#' @param type [\code{character(1)}] what type of plot should be drawn 
#'             (default is \code{"l"} for lines). See \code{\link[graphics]{plot}}
#' @param ... see arguments of \code{\link[graphics]{matplot}}
#' @return [\code{list}] A list element key \code{antsep} containing a numeric 
#'        vector of \eqn{n} antenna separation values, and element \code{twt} 
#'        containing a \eqn{n \times m} matrix, where \eqn{m} is the
#'        number of hyperbolas (i.e., the number of velocities).
#' @name plotCMPhyperbolas
#' @rdname plotCMPhyperbolas
setGeneric("plotCMPhyperbolas", function(x, ...) standardGeneric("plotCMPhyperbolas"))

#' @rdname plotCMPhyperbolas
#' @export
setMethod("plotCMPhyperbolas", "GPR", function(x, lty = 1, lwd = 1, type = "l", ...){
  if(!isCMP(x)){
    stop("This function only works on CMP data.")
  }
  if(is.null(x@vel[["vrms"]])){
    stop("You must first set v_rms velocities with 'setVel()'")
  }
  twt <- mapply(hyperbolicTWT, x@vel[["vrms"]]$t, x@vel[["vrms"]]$v, 
              MoreArgs = list(antsep = x@x))
  # dots <- list(...)
  matplot(x@x, twt, lty = lty, lwd = lwd, type = type, ...)
})

