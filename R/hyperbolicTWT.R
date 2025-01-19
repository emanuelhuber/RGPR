
#' Hyperbolic two-way travel time
#' 
#' Returns two-way travel time as a function of
#' antenna separation, two-way travel time at zero-offset, and wave velocity
#' in the medium
#' @param antsep (`numeric`) antenna separation
#' @param t0 (`numeric`) two-way travel time when `a` = 0 
#'           (zero-offset)
#' @param v (`numeric`) layer velocity (vrms)
#' @return (`numeric`) two-way travel time
#' @rdname hyperbolicTWT
#' @export
#' @concept velocity model
hyperbolicTWT <- function(t0, antsep, v){
  sqrt(t0^2 + (antsep/v)^2)
}
