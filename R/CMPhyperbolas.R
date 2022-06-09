#' Returns hyperbolas associated with Vrms velociies
#' 
#' Returns hyperbolas associated with root-mean-square velocities stored in
#' the CMP data.
#' @param x [\code{GPR class}] A CMP object of the class \code{GPR}
#' @return [\code{list}] A list element key \code{antsep} containing a numeric 
#'        vector of \eqn{n} antenna separation values, and element \code{twt} 
#'        containing a \eqn{n \times m} matrix, where \eqn{m} is the
#'        number of hyperbolas (i.e., the number of velocities).
#' @examples 
#' \dontrun{
#' HPB <- hyperbolaFromVrms(x)
#' plot(x, barscale = FALSE, main = "CMP")
#' matplot(HPB$antsep, HPB$twt, type = "l", col = "green", 
#'         lwd = 2, add = TRUE, lty = 1)
#' }        
#' @name CMPhyperbolas
#' @rdname CMPhyperbolas
setGeneric("CMPhyperbolas", function(x) standardGeneric("CMPhyperbolas"))

#' @rdname CMPhyperbolas
#' @export
setMethod("CMPhyperbolas", "GPR", function(x){
  if(!isCMP(x)){
    stop("This function only works on CMP data.")
  }
  if(is.null(x@vel[["vrms"]])){
    stop("You must first set v_rms velocities with 'setVel()'")
  }
  y <- mapply(hyperbolicTWT, x@vel[["vrms"]]$t, x@vel[["vrms"]]$v, 
              MoreArgs = list(antsep = x@pos))
  list(antsep = x@pos, twt = y)
})