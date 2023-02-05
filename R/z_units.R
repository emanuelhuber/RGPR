
#' Check if the unit is valid
#' @param x [\code{character(n)|units}]
#' @return [\code{character(n)}] 
#' @noRd
.checkUnit <- function(x){
  if(length(x) > 1){
    sapply(x, .checkUnit, USE.NAMES = FALSE)
  }else{
    if(length(x) == 0) return(NA_character_)
    if( is.na(x)) return(NA_character_)
    x <- tryCatch({units::as_units(x)},
              error = function(cond){ 
                stop(paste0("'", x, "'", 
                            "is not recognized as unit.\n",
                            "See a table of valid unit symbols and names with 'units::valid_udunits()'.\n",
                            "See a table of valid unit prefixes with 'units::valid_udunits_prefixes()'."),
                     call. = FALSE) })
    x <- as.character(units(x)) 
    if(x == "\u00B0"){
      return("degree")
    }else{
      return(x)
    }
  }
}
