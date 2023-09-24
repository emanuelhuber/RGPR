setGeneric("spInterpToRaster", function(x, nx = 100, ny = 100, h = 5, ...) 
  standardGeneric("spInterpToRaster"))


#' Interpolate trace elevation to raster
#' 
#' @name spInterpToRaster
#' @rdname spInterpToRaster
#' @export
setMethod("spInterpToRaster", "GPRsurvey", function(x, nx = 100, ny = 100, h = 5, ...){
  # better way to do
  stop("I disabled this function. Please contact me:\n emanuel.huber@pm.me")
  # xyz <- do.call(rbind, coords(x))
  # 
  # rr <- as.vector(apply(xyz[, 1:2], 2, range, na.rm = TRUE))
  # D <- 0.05 * (rr[c(2, 4)] - rr[c(1, 3)])
  # 
  # test <- chull(xyz[,1], xyz[, 2])
  # 
  # p = sp::Polygon(xyz[test, 1:2])
  # ps = sp::Polygons(list(p),1)
  # sps = sp::SpatialPolygons(list(ps))
  # 
  # spsb <- raster::buffer(sps, width = min(D))
  # 
  # # raster::plot(spsb, add = TRUE)
  # 
  # # rough interpolation (h = 10)
  # S <- MBA::mba.surf(xyz, no.X = nx, no.Y = ny, h = h, extend = TRUE,
  #                    b.box = rr + c(-D[1], D[1], -D[2], D[2]))
  # 
  # r <- raster::raster(S$xyz.est)
  # raster::crs(r) <- .getCheckedCRS(x)
  # 
  # r <- raster::mask(r, spsb)
  return(r)
})

#' @name spInterpToRaster
#' @rdname spInterpToRaster
#' @export
setMethod("spInterpToRaster", "matrix", function(x, nx = 100, ny = 100, h = 5, CRSobj, ...){
  # better way to do
  stop("I disabled this function. Please contact me:\n emanuel.huber@pm.me")
  # xyz <- do.call(rbind, coords(x))
  # xyz <- x
  # 
  # rr <- as.vector(apply(xyz[, 1:2], 2, range, na.rm = TRUE))
  # D <- 0.05 * (rr[c(2, 4)] - rr[c(1, 3)])
  # 
  # test <- chull(xyz[,1], xyz[, 2])
  # 
  # p = sp::Polygon(xyz[test, 1:2])
  # ps = sp::Polygons(list(p),1)
  # sps = sp::SpatialPolygons(list(ps))
  # 
  # spsb <- raster::buffer(sps, width = min(D))
  # 
  # # raster::plot(spsb, add = TRUE)
  # 
  # # rough interpolation (h = 10)
  # S <- MBA::mba.surf(xyz, no.X = nx, no.Y = ny, h = h, extend = TRUE,
  #                    b.box = rr + c(-D[1], D[1], -D[2], D[2]))
  # 
  # r <- raster::raster(S$xyz.est)
  # raster::crs(r) <- CRSobj
  # 
  # r <- raster::mask(r, spsb)
  return(r)
})

