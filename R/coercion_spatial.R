
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

#------------------------------ SPATIAL LINES ---------------------------------#

# FIXME: change coercion to sp -> via sf!!!
#' @export
setAs(from = "GPRsurvey", to = "SpatialLines",
      def = function (from) as.SpatialLines(from))

#' @export
setAs(from = "GPR", to = "SpatialLines",
      def = function (from) as.SpatialLines(from))

#' Coerce object to an object of the class SpatialLines
#'
#' Coerce object to an object of the class SpatialLines
#' @param x [\code{class GPR|GPRsurvey}]
#' @return [\code{SpatialLines object}]
#' @name as.SpatialLines
# #' @seealso \code{\link{time0Cor}} to shift the traces such that they start
# #'          at time-zero.
setGeneric("as.SpatialLines", function(x) 
  standardGeneric("as.SpatialLines"))

#' @rdname as.SpatialLines
#' @export
setMethod("as.SpatialLines", signature(x = "GPRsurvey"), function(x){
  # remove NULL from list
  # FIXME
  # Filter(Negate(is.null), x) = alternative
  isNotNull <- !sapply(x@coords, is.null)
  if(any(isNotNull)){
    xyz <- x@coords[isNotNull]
    lineList <- lapply(unname(xyz), .xyToLine)
    linesList <- lapply(seq_along(lineList), .lineToLines, lineList, 
                        x@names[isNotNull])
    mySpatLines <- sp::SpatialLines(linesList)
    sp::proj4string(mySpatLines) <- .getCheckedCRS(x)
    return(mySpatLines)
  }else{
    warning("no coordinates!")
    return(NULL)   
  }
})

#' @rdname as.SpatialLines
#' @export
setMethod("as.SpatialLines", signature(x = "GPR"), function(x){
  myLine <- sp::Line(x@coord[,1:2])
  myLines <- sp::Lines(list(myLine), ID = x@name)
  mySpatLines <- sp::SpatialLines(list(myLines))
  sp::proj4string(mySpatLines) <- .getCheckedCRS(x)
  return(mySpatLines)
})


#------------------------------ SPATIAL POINTS --------------------------------#

#' @export
setAs(from = "GPRsurvey", to = "SpatialPoints",
      def = function (from) as.SpatialPoints(from))    

#' Coerce object to an object of the class SpatialPoints
#'
#' Coerce object to an object of the class SpatialPoints
#' @param x [\code{class GPR|GPRsurvey}]
#' @return [\code{SpatialPoints}]
#' @name as.SpatialPoints
# #' @seealso \code{\link{time0Cor}} to shift the traces such that they start
# #'          at time-zero.
# \link{SpatialPoints-class} sp package
setGeneric("as.SpatialPoints", function(x) 
  standardGeneric("as.SpatialPoints"))



#' @rdname as.SpatialPoints
#' @export
setMethod("as.SpatialPoints", signature(x = "GPRsurvey"), function(x){
  allTopo <- do.call(rbind, x@coords)  #  N, E, Z
  allTopo2 <- as.data.frame(allTopo)
  names(allTopo2) <- c("x", "y", "z")
  sp::coordinates(allTopo2) <- ~ x + y
  
  sp::proj4string(allTopo2) <- .getCheckedCRS(x)
  
  return(allTopo2)
})

.xyToLine <- function(x){
  sp::Line(x[,1:2])
}

.lineToLines <- function(i,pp, myNames){
  sp::Lines(pp[i],myNames[i])
}
