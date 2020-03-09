
#' Unit of Coordinate Reference System (CRS)
#' 
#' Returns the unit of the CRS
#' @param x [\code{character}] CRS (one or more)
#' @return [\code{character}] The unit abbreviation (except for degree)
#' @export
crsUnit <- function(x){
  x <- as.character(x)
  sel <- isCRSLonLat(x)
  un <- rep("degree", length(x))
  if(all(sel)){
    return(un)
  }else{
    x <- x[!sel]
    pattern <- "units=(?<unit>[a-z]+)"
    x_pat <- extractPattern(x, pattern, start = 0, stop = 0)
    un[!sel] <- trimStr(x_pat)
    return(un)
  }
}