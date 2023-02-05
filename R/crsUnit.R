
#' Unit of Coordinate Reference System (CRS)
#' 
#' Returns the unit of the CRS
#' @param CRSobj [\code{sp::CRS|character}] CRS object (one or more)
#' @return [\code{character}] The unit abbreviation (except for degree)
#' @export
crsUnit <- function(CRSobj){
  if(length(CRSobj) == 1){
    CRSobj <- tryCatch(sf::st_crs(CRSobj),
                       error = function(e){
                         message("Invalid CRS! Try something like 'EPSG:3857'!")
                         return(NA_character_)
                       })
    # CRSobj <- as.character(CRSobj)
    if(CRSobj$IsGeographic){
      return("degree")
    }else{
      return(.checkUnit(CRSobj$ud_unit))
    }
  }else{
    sapply(CRSobj, crsUnit, USE.NAMES = FALSE)
  }
  # sel <- isCRSGeographic(CRSobj)
  # un <- rep("degree", length(CRSobj))
  # if(all(sel)){
  #   return(un)
  # }else{
  #   CRSobj <- CRSobj[!sel]
  #   pattern <- "units=(?<unit>[a-z]+)"
  #   CRSobj_pat <- extractPattern(CRSobj, pattern, start = 0, stop = 0)
  #   un[!sel] <- trimStr(CRSobj_pat)
  #   return(un)
  # }
}