


setGeneric("spExtractFromRaster", function(x, r, method = "bilinear", ...) 
  standardGeneric("spExtractFromRaster"))


#' Extract elevation from raster data
#' 
#' Based on the function \code{extract} from \code{raster} package.
#' Check arguments and help for this function
#' 
#' @name spExtractFromRaster
#' @rdname spExtractFromRaster
#' @export
setMethod("spExtractFromRaster", "GPRsurvey", 
          function(x, r, method = "bilinear", ...){
  coords(x) <- lapply(coords(x), .extractRaster, r, method = method, ...) 
  return(x)
})
          


.extractRaster <- function(x, r, method, ...){
  x[, 3] <- raster::extract(r, x[, 1:2], method = method, ...)
  return(x)
}