
#' Unit of Coordinate Reference System (CRS)
#' 
#' Returns the unit of the CRS
#' @param crs [\code{character(1)}] A string accepted by GDAL 
#'               (e.g., \code{"EPSG:2056"}, WKT-string).
#' @return [\code{character}] The unit abbreviation (except for degree)
#' @export
#' @concept units
crsUnit <- function(crs){
  if(!is.character(crs)) stop("'crs' must be a character!")
  if(length(crs) == 0 || all(is.na(crs))){
    return(NA_character_)
  }else if(length(crs) == 1){
    crs <- tryCatch(sf::st_crs(crs),
                       error = function(e){
                         message("Invalid CRS! Try something like 'EPSG:3857'!")
                         return(NA_character_)
                       })
    if(is.na(crs)) return(NA_character_)
    # crs <- as.character(crs)
    # print(crs)
    if(crs$IsGeographic){
      return("degree")
    }else{
      return(.checkUnit(crs$ud_unit))
    }
  }else{
    sapply(crs, crsUnit, USE.NAMES = FALSE)
  }
}
