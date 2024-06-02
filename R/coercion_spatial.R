#' @export
setAs(from = "GPRsurvey", to = "SpatialLines",
      def = function (from) as.spatialLines(from))

#' @export
setAs(from = "GPRsurvey", to = "SpatialPoints",
      def = function (from) as.spatialPoints(from))    


setGenericVerif("as.spatialPoints", function(x) 
  standardGeneric("as.spatialPoints"))

setGenericVerif("as.spatialLines", function(x) 
  standardGeneric("as.spatialLines"))


#' @export
setAs(from = "GPR", to = "sf",
      def = function (from) as.sf(from))

#' Coerce object to an sf POINT object
#'
#' Coerce object to a simple feature (sf) geometry POINT. 
#' Only coordinates are coerced, no additional information.
#' @param x [\code{class GPR|GPRsurvey}]
#' @return [\code{class sf}] Geometry type is \code{POINT}.
#' @name as.sf
#' @concept coercion
setGeneric("as.sf", function(x) 
  standardGeneric("as.sf"))


#' @rdname as.sf
#' @export
setMethod("as.sf", signature(x = "GPR"), function(x){
  if(length(x@coord) == 0){
    stop("No coordinates. Set first coordinates either with 'coordinates(x) <-'\n",
         "  or with ' interpCoords(x, ...)'.")
  }
  .as_LINESTRING(x@coord, x@crs)
})


#' @rdname as.sf
#' @export
setMethod("as.sf", signature(x = "GPRsurvey"), function(x){
  sel <- sapply(x@coords, function(x) length(x) > 0)
  if(all(!sel)){
    stop("No coordinates. Set first coordinates either with 'coordinates(x) <-'\n",
         "  or with ' interpCoords(x, ...)'.")
    
  }
  if(length(x@crs) != 1){
    x@crs <- x@crs[1]
    # stop("'crs' length must be 1")
  }
  test <- lapply(x@coords[sel], .as_LINESTRING,  x@crs)
  x_sfc <- do.call(c, test)
  x_sf <- sf::st_sf(x_sfc)
  x_sf$names <- x@names[sel]
  x_sf$surveymodes <- x@surveymodes[sel]
  x_sf$dates <- x@dates[sel]
  
  if(any(!sel)){
    warning("I cannot coerce the following GPR data to 'sf' ",
            "(they don't have coordinates):\n", 
            paste0("  #", which(!sel), ": ", x@names[!sel], collapse ="\n"), 
            ".")
  }
  return(x_sf)
})

.as_LINESTRING <- function(x, CRS){
  if(length(x) > 0){
    xi_sf <- sf::st_as_sf(x      = as.data.frame(x),
                          coords = 1:3,
                          crs    = CRS)
    xi_sf <- sf::st_combine(xi_sf)
    xi_sf <- sf::st_cast(xi_sf, "LINESTRING")
    return(xi_sf)
  }
}

#------------------------------ SPATIAL LINES ---------------------------------#


#' Coerce object to an object of the class SpatialLines
#'
#' Coerce object to an object of the class SpatialLines
#' @param x [\code{class GPR|GPRsurvey}]
#' @return [\code{SpatialLines object}]
#' @name as.spatialLines
#' @concept coercion
setGeneric("as.spatialLines", function(x) 
  standardGeneric("as.spatialLines"))

#' @rdname as.spatialLines
#' @export
setMethod("as.spatialLines", signature(x = "GPRsurvey"), function(x){
  as.sf(x)
})

#' @rdname as.spatialLines
#' @export
setMethod("as.spatialLines", signature(x = "GPR"), function(x){
  as.sf(x)
})


#------------------------------ SPATIAL POINTS --------------------------------#


#' Coerce object to an object of the class SpatialPoints
#'
#' Coerce object to an object of the class SpatialPoints
#' @param x [\code{class GPR|GPRsurvey}]
#' @return [\code{SpatialPoints}]
#' @name as.spatialPoints
#' @concept coercion
setGeneric("as.spatialPoints", function(x) 
  standardGeneric("as.spatialPoints"))



#' @rdname as.spatialPoints
#' @export
setMethod("as.spatialPoints", signature(x = "GPR"), function(x){
  if(length(x@coord) > 0){
    xdf <- as.data.frame(x@coord)
    if(length(x@fid) == ncol(x)) xdf['markers'] <- x@fid
    if(length(x@ann) == ncol(x)) xdf['ann'] <- x@ann
    if(length(x@time) == ncol(x)) xdf['time'] <- x@time
    
    x_sf <- sf::st_as_sf(x      = xdf,
                         coords = 1:3)
    if(length(x@crs) > 0){
      sf::st_crs(x_sf) <- x@crs
    }
    
    return(x_sf)
  }else{
    stop("'x' has no coordinates. Please add first coordinates to 'x'!")
  }
})

#' @rdname as.spatialPoints
#' @export
setMethod("as.spatialPoints", signature(x = "GPRsurvey"), function(x){
  sel <- sapply(x@coords, function(x) length(x) > 0)
  if(all(!sel)){
    stop("No coordinates. Set first coordinates either with 'coordinates(x) <-'\n",
         "  or with ' interpCoords(x, ...)'.")
    
  }
  if(length(x@crs) != 1){
    x@crs <- x@crs[1]
    # stop("'crs' length must be 1")
  }
  fun <-function(x, x_crs){
    sf::st_as_sf(x      = as.data.frame(x),
                 coords = 1:3,
                 crs    = x_crs)
  }
  Xsf <- lapply(x@coords[sel], fun, x@crs)
  x_sf <- do.call(rbind, Xsf)
  x_nx <- sapply(x@coords, nrow)
  x_sf$names <- rep(x@names[sel],  x_nx[sel])
  x_sf$surveymodes <- rep(x@surveymodes[sel],  x_nx[sel])
  x_sf$dates <- rep(x@dates[sel],  x_nx[sel])
  
  if(any(!sel)){
    warning("I cannot coerce the following GPR data to 'sf' ",
            "(they don't have coordinates):\n", 
            paste0("  #", which(!sel), ": ", x@names[!sel], collapse ="\n"), 
            ".")
  }
  return(x_sf)
  
  
})