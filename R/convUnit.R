# library(units)
# library(RGPR)


#' Utility function for unit conversion
#' 
#' Convert numerical values from one unit to another unit
#' @param x [\code{numeric}] Numeric vector of values to be converted
#' @param unitIn [\code{character}] Current unit of the \code{x} 
#' (see \code{\link[units]{valid_udunits}} for a list of valid units).
#' @param unitOut [\code{character}] Unit in which to convert \code{x} 
#' (see \code{\link[units]{valid_udunits}} for a list of valid units).
#' @export
#' @examples 
#' # Conversion from feets to meters
#' v_ft <- 1:10
#' v_m <- convUnit(v_ft, unitIn = "ft", unitOut = "m")
#' 
#' # Conversion from meters to milimeters
#' v_mm <- convUnit(v_m, unitIn = "m", unitOut = "mm")
convUnit <- function(x, unitIn, unitOut){
  unit_in <- tryCatch(units::as_units(unitIn),
                      error = function(cond){
                        return(NULL)})
  unit_out <- tryCatch(units::as_units(unitOut),
                       error = function(cond){
                         return(NULL)})
  if(is.null(unit_in) || is.null(unit_out)){
    # aout <- "Error:\n"
    aout <- ""
    if(is.null(unit_in)) aout <- paste0(aout, "\nInput unit '",  unitIn, "' is not recognized as unit.\n")
    if(is.null(unit_out)) aout <- paste0(aout, "\nOutput unit '", unitOut, "' is not recognized as unit.\n")
    aout <- paste0(aout, "Check available units with 'units::valid_udunits()'")
    stop(aout)
  }else{
    x <- units::set_units(x, unitIn, mode = "standard")
    x <- units::set_units(x, unitOut, mode = "standard")
    return(as.numeric(x))
  }
}
