

matrix2polygon <- function(x){
  x <- sf::st_as_sf(as.data.frame(x), coords = 1:2 )
  x <- sf::st_combine(x)
  return(sf::st_cast(x, 'POLYGON'))
}
