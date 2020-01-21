

setGeneric("spInterpToRaster", function(x, nx = 100, ny = 100, h = 5, ...) 
  standardGeneric("spInterpToRaster"))


#' spInterpToRaster
#' 
#' @name spInterpToRaster
#' @rdname spInterpToRaster
#' @export
setMethod("spInterpToRaster", "GPR", function(x, nx = 100, ny = 100, h = 5, ...){
  # better way to do
  xyz <- do.call(rbind, coords(x))
  
  # rough interpolation (h = 10)
  S <- MBA::mba.surf(xyz, no.X = nx, no.Y = ny, h = h, ...)
  
  return(raster::raster(S$xyz.est))
})