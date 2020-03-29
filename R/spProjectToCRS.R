#' Trace projection
#'
#' Project the trace coordinates give a coordinate reference system.
#' 
#' Modified slots
#' \itemize{
#'   \item \code{coord} the trace coordinates
#'   \item \code{x} the local trace position (along profile)
#'   \item \code{crs} the coordinate reference system.
#'   \item \code{spunit} the spatial units are updated accroding to the new
#'                       coordinate reference system.
#' }
#' @param x Object of the class GPR
#' @param CRSobj object of class \link{CRS}, or of class \code{character} in
#'               which case it is converted to \link{CRS}.
#' @name spProjectToCRS
setGeneric("spProjectToCRS", function(x, CRSobj)
    standardGeneric("spProjectToCRS"))

#' @rdname spProjectToCRS
#' @export
setMethod("spProjectToCRS", "GPR", function(x, CRSobj){
  #---- check some stuff
  msg <- c()
  if(length(x@coord) == 0){
    msg <- c(msg, paste0("Cannot project 'x' if 'x' has no coordinates:\n",
             "Use either 'coord()<-' or 'spInterp()' to set coordinates."))
  }
  if(is.na(x@crs)){
    msg <- c(msg, paste0("Cannot project 'x' if 'x' has no CRS:\n",
             "Use 'crs()<-' to set coordinates"))
  }
  if(length(msg) > 0){
    stop(paste0(paste0(seq_along(msg), ". "), msg, sep = "\n"))
  } 
  #------------------------ -
  
  x_sf     <- sf::st_transform(as.sf(x), CRSobj)
  crs(x)   <- sf::st_crs(x_sf)
  coord(x) <- sf::st_coordinates(x_sf)

  return(x)
})


#' @rdname spProjectToCRS
#' @export
setMethod("spProjectToCRS", "GPRsurvey", function(x, CRSobj){
  x_crs <- x@crs
  if(length(x@crs) == 1) x_crs <- rep(x@crs, length(x))
  for(i in seq_along(x)){
    if(length(x@coords[[i]]) > 0 ){
      xi_sf <- sf::st_as_sf(x      = as.data.frame(x@coords[[i]]),
                            coords = 1:3,
                            crs    = x_crs[i])
      xi_sf     <- sf::st_transform(xi_sf, CRSobj)
      x@coords[[i]] <- as.matrix(sf::st_coordinates(xi_sf))
      x@xlengths[i]  <- pathLength(x@coords[[i]])
    }else{
      warning(x@names[i], ": cannot be projected (no coordinates).")
    }
    crs(x) <- CRSobj
    x@intersections <- list()
    # x <- coordref(x)
    x <- spIntersection(x)
    # crs(x)   <- sf::st_crs(x_sf)
  }
  return(x)
})
