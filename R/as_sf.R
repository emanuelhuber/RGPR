#' @export
setAs(from = "GPR", to = "sf",
      def = function (from) as.sf(from))

#' Coerce object to an sf POINT object
#'
#' Coerce object to a simple feature (sf) geometry POINT. 
#' Only coordinates are coerced, all additional information is lost.
#' @param x [\code{class GPR|GPRsurvey}]
#' @return [\code{class sf}] Geometry type is \code{POINT}.
#' @name as.sf
# #' @seealso \code{\link{time0Cor}} to shift the traces such that they start
# #'          at time-zero.
setGeneric("as.sf", function(x) 
  standardGeneric("as.sf"))


#' @rdname as.sf
#' @export
setMethod("as.sf", signature(x = "GPR"), function(x){
  if(length(x@coord) == 0){
    stop("No coordinates. Set first coordinates either with 'coord(x) <-'\n",
         "  or with ' spInterp(x, ...)'.")
  }
  x_sf <- sf::st_as_sf(x      = as.data.frame(x@coord),
                       coords = 1:3,
                       crs    = x@crs)
  x_sf <- sf::st_combine(x_sf)
  x_sf <- sf::st_cast(x_sf, "LINESTRING")
  return(x_sf)
})


#' @rdname as.sf
#' @export
setMethod("as.sf", signature(x = "GPRsurvey"), function(x){
  sel <- sapply(x@coords, function(x) length(x) > 0)
  if(all(!sel)){
    stop("No coordinates. Set first coordinates either with 'coord(x) <-'\n",
         "  or with ' spInterp(x, ...)'.")
    
  }
  x_crs <- x@crs
  if(length(x_crs) == 1){
    x_crs <- rep(x_crs, length(x))
  }else if(length(x_crs) != length(x)){
    stop("'crs' length must be either 1 or 'length(x)' = ", length(x))
  }else{
    warning("Your data have different CRS (check with 'crs()'!\n",
            "  I take the first CRS for the coercion to sf...")
  }
  test <- mapply(.as_LINESTRING, x@coords, x_crs, SIMPLIFY = FALSE)
  x_sf <- do.call(c, test[sel])
  if(any(!sel)){
    warning("I cannot coerce the following GPR data to 'sf' ",
            "(they don't have coordinates):\n", 
            paste0("  #", which(!sel), ": ", x@names[!sel], collapse ="\n"), 
            ".")
  }
  return(x_sf)
})

.as_LINESTRING <- function(x, CRSobj){
  if(length(x) > 0){
    xi_sf <- sf::st_as_sf(x      = as.data.frame(x),
                          coords = 1:3,
                          crs    = CRSobj)
    xi_sf <- sf::st_combine(xi_sf)
    xi_sf <- sf::st_cast(xi_sf, "LINESTRING")
    return(xi_sf)
  }
}
