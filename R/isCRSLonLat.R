
#' Check wether the Coordinate Reference Systems is lon/lat
#' 
#' Returns \code{TRUE} or \code{FALSE}.
#' @param x [\code{character}] CRS (one or more)
#' @return [\code{logical}] \code{TRUE} if lon/lat else \code{FALSE}
#' @export
isCRSLonLat <- function(x){
  grepl("+proj=longlat", as.character(x))
}