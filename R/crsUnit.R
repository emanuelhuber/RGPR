
#' Unit of Coordinate Reference System (CRS)
#' 
#' Returns the unit of the CRS
#' @param CRSobj [\code{sp::CRS|character}] CRS object (one or more)
#' @return [\code{character}] The unit abbreviation (except for degree)
#' @export
crsUnit <- function(CRSobj){
  if(length(CRSobj) == 1 || is.na(CRSobj)){
    return(NA_character_)
  }else if(length(CRSobj) == 1){
    CRSobj <- tryCatch(sf::st_crs(CRSobj),
                       error = function(e){
                         message("Invalid CRS! Try something like 'EPSG:3857'!")
                         return(NA_character_)
                       })
    if(is.na(CRSobj)) return(NA_character_)
    # CRSobj <- as.character(CRSobj)
    # print(CRSobj)
    if(CRSobj$IsGeographic){
      return("degree")
    }else{
      return(.checkUnit(CRSobj$ud_unit))
    }
  }else{
    sapply(CRSobj, crsUnit, USE.NAMES = FALSE)
  }
}
