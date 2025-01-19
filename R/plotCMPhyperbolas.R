

#' Plot hyperbolas associated with Vrms velociies
#' 
#' Plot hyperbolas associated with root-mean-square velocities stored in
#' the CMP data.
#' @param x (`GPR class`) A CMP object of the class `GPR`
#' @param lty (`numeric[1]`) The line type 
#'            (default is `1` for solid lines). See [graphics::par()]
#' @param lwd (`numeric[1]`) Line width
#'             (default is `1`). See [graphics::par()]
#' @param type (`character[1]`) what type of plot should be drawn 
#'             (default is `"l"` for lines). See [graphics::plot()]
#' @param ... see arguments of [graphics::matplot()]
#' @return (`list`) A list element key `antsep` containing a numeric 
#'        vector of \eqn{n} antenna separation values, and element `twt` 
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

