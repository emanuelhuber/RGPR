
# devtools::install_local("/mnt/data/RGPR/CODE/RGPR", force = TRUE)

# library(RGPR)

LINES <- file.path("/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2011_10_10_flagogna",
                   paste0("XLINE", sprintf("%03d", 0:5), ".DT1"))


 
x <- GPRsurvey(LINES, verbose = FALSE) 

plot(x)
x_obbox <- spOBB(x[2:6])
x_obbox <- spOBB(x)
lines(x_obbox, type = "l")

spAngle(x)

xr <- spGeoref(x)

xyz <- x@coords
xyz <- Filter(Negate(is.null), xyz)
xyz <- xyz[sapply(xyz, function(x) length(x)> 0)]

orb <- spOBB(x)
dEN <- orb[1,] - orb[2,]
i <- which.max(diff(pathRelPos(orb)))[1]
dOBB <- orb[i + 1,] - orb[i,]
angl_OBB <- atan2(dOBB[2], dOBB[1])
# # angl_OBB/pi * 180
# # abs(angl_EN - angl_OBB) / pi * 180
# if(pi * 6/5 > abs(angl_EN - angl_OBB) && abs(angl_EN - angl_OBB)  > pi* 4 /5){
#   angl_OBB <- angl_OBB + pi
#   if(angl_OBB > pi) angl_OBB <- angl_OBB - 2*pi
# }
 
X <- interpSlices(x[2:6], dx = 0.1, dy = 0.1, dz = 10)
X <- interpSlices(x[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = TRUE)
X <- interpSlices(x[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = TRUE, buffer = c(0, 0))
 
#plot3D::image2D(X@data[,,15])

# FIXME:
# EXTEND = TRUE
# EXTEND = chull + buffer
# EXTEND = lines + buffer

X@center
Xx <- X@center[1] + seq(0, by = X@dx, length.out = dim(X@data)[1])
Xy <- X@center[2] + seq(0, by = X@dy, length.out = dim(X@data)[2])

plot3D::image2D(x = Xx, y = Xy, z = X@data[,,15], asp = 1)
plot(x, add = TRUE)


Xx - X@center[1]


dim(X@data)

slotNames(X)


dx = 1
dy = 1
dz = 1
extend = TRUE


 x <- x[2:6]
# default z-values (get elevation range)
x_zi0 <- defVz(x)
x_zi <- seq(min(x_zi0), by = dz, to = max(x_zi0))
if(all(isZunitLength(x)) ){
  x_zi <- sort(x_zi, decreasing = TRUE)
}

if(is.null(dx)){
  dx <- mean(sapply(x@coords, function(x) mean(diff(pathRelPos(x)))))
  # nx <- (max(sapply(x@coords, function(x) max(x[,1]))) -
  #          min(sapply(x@coords, function(x) min(x[,1])) )) / dxy
  # nx <- round(nx)
}
if(is.null(dy)){
  dy <- dx
}
if(is.null(dz)){
  dz <- abs(diff(x_zi))
}
#Z <- list()
V <- list()
for(i in seq_along(x)){
  if(isZunitLength(x[[i]])){
    if(length(unique(x[[i]]@coord[,3])) > 1){
      stop("The traces have different elevation!")
    } 
    x_z   <- x[[i]]@coord[1,3] - x[[i]]@z
  }else{
    x_z   <- x[[i]]@z
  }
  x_data <- x[[i]]@data
  x_data[is.na(x_data)] <- 0
  # interpolation
  V[[i]] <- apply(x_data, 2, trInterp, z = x_z, zi = x_zi )
  # Z[[i]] <- x_zi   # x[[i]]@depth
}

# vj <- seq(dz, by = dz, to = length(x_zi))

val <- list()
# positions obervations
xpos <- unlist(lapply(x@coords, function(x) x[,1]))
ypos <- unlist(lapply(x@coords, function(x) x[,2]))

# # define bounding box + number of points for interpolation
# obb <- spOBB(x)
# bbox <- c(min(obb[,1]), max(obb[,1]), min(obb[,2]), max(obb[,2]))
# bbox <- c(floor(min(obb[,1])), 
#           ceiling(max(obb[,1])), 
#           floor(min(obb[,2])), 
#           ceiling(max(obb[,2])))
# bbox <- c(min(xpos), max(xpos), min(ypos), max(ypos))

# define bounding box (axis-aligned)
buffer <- NULL

para <- getbbox_nx_ny(xpos, ypos)

SL <- array(dim = c(para$nx, para$ny, length(x_zi)))


# bbox[1] < min(xpos)

j <- 1
h <- 6
n <- 1
m <- 1
ratio_x_y <- bbox_dy / bbox_dx
ratio_x_y <- (para$bbox[4] - para$bbox[3]) / (para$bbox[2] - para$bbox[1])
if(ratio_x_y < 1){
  m <- round(1/ratio_x_y)
}else{
  n <- round(ratio_x_y)
}
if(m < 1) m <- 1L
if(n < 1) n <- 1L


for(j in  seq_along(x_zi)){
  val[[j]] <- unlist(lapply(V, function(v, k = j) v[k,]))
  S <- MBA::mba.surf(cbind(xpos, ypos, val[[j]]), para$nx , para$ny, n = n, m = m, 
                     extend = extend, h = h, b.box = para$bbox)$xyz.est
  SL[,,j] <- S$z
}
S$x
diff(S$x)
sum(diff(S$x))
length(S$x)
nx
S$y
diff(S$y)

plot3D::image2D(x = S$x, y = S$y, z = SL[,,15], asp = 1)
plot(x, add = TRUE)

return(list(x = S$x, y = S$y, z = SL, vz = x_zi, x0 = xpos, y0 = ypos, z0 = val))

getbbox_nx_ny <- function(xpos, ypos, buffer = NULL){
  xpos_rg <- range(xpos)
  ypos_rg <- range(ypos)
  
  if(is.null(buffer)){
    buffer <- c(diff(xpos_rg) * 0.05,
                diff(ypos_rg) * 0.05)
  }
  xpos_pretty <- pretty( xpos_rg + c(-1, 1) * buffer[1])
  ypos_pretty <- pretty(ypos_rg + c(-1, 1) * buffer[2])
  bbox <- c(xpos_pretty[1], tail(xpos_pretty, 1), 
            ypos_pretty[1], tail(ypos_pretty, 1))
  
  # define the number of cells
  bbox_dx <- bbox[2] - bbox[1]
  bbox_dy <- bbox[4] - bbox[3]
  nx <- ceiling(bbox_dx / dx )
  ny <- ceiling(bbox_dy / dy )
  
  # correct bbox (such that dx, dy are correct)
  Dx <- (nx * dx - bbox_dx)/2
  Dy <- (ny * dy - bbox_dy)/2
  bbox[1:2] <- bbox[1:2] + c(-1, 1) * Dx
  bbox[3:4] <- bbox[3:4] + c(-1, 1) * Dy 
  bbox_dx <- bbox[2] - bbox[1]
  bbox_dy <- bbox[4] - bbox[3]
  nx <- nx + 1
  ny <- ny + 1
  return(list(bbox = bbox, nx = nx, ny = ny))
}


# CHECK CHECK CHECK CHECK CHECK CHECK CHECK 
# 
# # #' Interpolate trace elevation to raster
# # #' 
# # #' @name spInterpToRaster
# # #' @rdname spInterpToRaster
# # #' @export
# setMethod("spInterpToRaster", "GPRsurvey", function(x, nx = 100, ny = 100, h = 5, ...){
#   # better way to do
#   xyz <- do.call(rbind, coords(x))
#   
#   rr <- as.vector(apply(xyz[, 1:2], 2, range, na.rm = TRUE))
#   D <- 0.05 * (rr[c(2, 4)] - rr[c(1, 3)])
#   
#   test <- chull(xyz[,1], xyz[, 2])
#   
#   p = sp::Polygon(xyz[test, 1:2])
#   ps = sp::Polygons(list(p),1)
#   sps = sp::SpatialPolygons(list(ps))
#   
#   spsb <- raster::buffer(sps, width = min(D))
#   
#   # raster::plot(spsb, add = TRUE)
#   
#   # rough interpolation (h = 10)
#   S <- MBA::mba.surf(xyz, no.X = nx, no.Y = ny, h = h, extend = TRUE,
#                      b.box = rr + c(-D[1], D[1], -D[2], D[2]))
#   
#   r <- raster::raster(S$xyz.est)
#   raster::crs(r) <- .getCheckedCRS(x)
#   
#   r <- raster::mask(r, spsb)
#   return(r)
# })
# 
# # #' @name spInterpToRaster
# # #' @rdname spInterpToRaster
# # #' @export
# setMethod("spInterpToRaster", "matrix", function(x, nx = 100, ny = 100, h = 5, CRSobj, ...){
#   # better way to do
#   # xyz <- do.call(rbind, coords(x))
#   xyz <- x
#   
#   rr <- as.vector(apply(xyz[, 1:2], 2, range, na.rm = TRUE))
#   D <- 0.05 * (rr[c(2, 4)] - rr[c(1, 3)])
#   
#   test <- chull(xyz[,1], xyz[, 2])
#   
#   p = sp::Polygon(xyz[test, 1:2])
#   ps = sp::Polygons(list(p),1)
#   sps = sp::SpatialPolygons(list(ps))
#   
#   spsb <- raster::buffer(sps, width = min(D))
#   
#   # raster::plot(spsb, add = TRUE)
#   
#   # rough interpolation (h = 10)
#   S <- MBA::mba.surf(xyz, no.X = nx, no.Y = ny, h = h, extend = TRUE,
#                      b.box = rr + c(-D[1], D[1], -D[2], D[2]))
#   
#   r <- raster::raster(S$xyz.est)
#   raster::crs(r) <- CRSobj
#   
#   r <- raster::mask(r, spsb)
#   return(r)
# })
