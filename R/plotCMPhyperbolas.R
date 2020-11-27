

#' Plot hyperbolas associated with Vrms velociies
#' 
#' Plot hyperbolas associated with root-mean-square velocities stored in
#' the CMP data.
#' @param x [\code{GPR class}] A CMP object of the class \code{GPR}
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
setMethod("plotCMPhyperbolas", "GPR", function(x, ...){
  if(!isCMP(x)){
    stop("This function only works on CMP data.")
  }
  if(is.null(x@vel[["vrms"]])){
    stop("You must first set v_rms velocities with 'setVel()'")
  }
  y <- mapply(hyperbolicTWT, x@vel[["vrms"]]$t, x@vel[["vrms"]]$v, 
              MoreArgs = list(antsep = x@x))
  # dots <- list(...)
  matplot(x@x, ...)
})

