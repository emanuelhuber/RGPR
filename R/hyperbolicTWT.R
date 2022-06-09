#' Hyperbolic two-way travel time
#' 
#' Returns two-way travel time as a function of
#' antenna separation, two-way travel time at zero-offset, and wave velocity
#' in the medium
#' @param antsep [\code{numeric}] antenna separation
#' @param t0 [\code{numeric}] two-way travel time when \code{a} = 0 
#'           (zero-offset)
#' @param v [\code{numeric}] layer velocity (vrms)
#' @return [\code{numeric}] two-way travel time
#' @rdname hyperbolicTWT
#' @export
hyperbolicTWT <- function(t0, antsep, v){
  sqrt(t0^2 + (antsep/v)^2)
}