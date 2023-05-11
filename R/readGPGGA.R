#FIXME -> what does it return? A spatialPoints
#' Read GPS file with GPGGA string
#' @param dsn [\code{character(1)|connection object}] Data source name: 
#'            either the filepath to the GPR data (character),
#'            or an open file connection.
#' @param sep [\code{character(1)}] The field separator character
#'            (see\code{\link{read.table}}).
#' @param returnSf [\code{logical(1)}] If \code{TRUE} returns an object of class
#'              \code{sf} (see \code{\link{sf}{sf}}). If \code{FALSE} returns
#'              a \code{data.frame}.
#' @return [\code{SpatialPoints}]
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