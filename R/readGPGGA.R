#FIXME -> what does it return? A spatialPoints
#' Read GPS file with GPGGA string
#' @param dsn (`character(1)|connection object`) Data source name: 
#'            either the filepath to the GPR data (character),
#'            or an open file connection.
#' @param sep (`character(1)`) The field separator character
#'            (see[read.table()]).
#' @param returnSf (`logical(1)`) If `TRUE` returns an object of class
#'              `sf`. If `FALSE` returns
#'              a `data.frame`.
#' @return (`SpatialPoints`)
#' @export
readGPGGA <- function(dsn, sep = ",", returnSf = TRUE){
  if(!inherits(dsn, "connection")){
    dsn <- file(dsn, 'rb')
  }
  content <- verboseF(readLines(dsn), verbose = FALSE)
  if(length(content) == 0){
    .closeFileIfNot(dsn)
    return(NULL)
  }
  a <- read.table(textConnection(content), 
                  header = FALSE, colClasses = "character",
                  stringsAsFactors = FALSE, sep = ",")
  llz <- getLonLatFromGPGGA(a)
  if(isTRUE(returnSf)){
    llz <- sf::st_as_sf(x = llz,
                        coords = c("lon", "lat"),
                        crs = 4326)
  }
  .closeFileIfNot(dsn)
  return(llz)
}
