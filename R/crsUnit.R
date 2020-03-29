
#' Unit of Coordinate Reference System (CRS)
#' 
#' Returns the unit of the CRS
#' @param CRSobj [\code{sp::CRS|character}] CRS object (one or more)
#' @return [\code{character}] The unit abbreviation (except for degree)
#' @export
crsUnit <- function(CRSobj){
  CRSobj <- as.character(CRSobj)
  sel <- isCRSLonLat(CRSobj)
  un <- rep("degree", length(CRSobj))
  if(all(sel)){
    return(un)
  }else{
    CRSobj <- CRSobj[!sel]
    pattern <- "units=(?<unit>[a-z]+)"
    CRSobj_pat <- extractPattern(CRSobj, pattern, start = 0, stop = 0)
    un[!sel] <- trimStr(CRSobj_pat)
    return(un)
  }
}