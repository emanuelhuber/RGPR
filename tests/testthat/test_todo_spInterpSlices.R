# # # # 
# # # # # devtools::install_local("/mnt/data/RGPR/CODE/RGPR", force = TRUE)
# # # # 
# # # # # library(RGPR)
# 
# LINES <- file.path("/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2011_10_10_flagogna",
#                    paste0("XLINE", sprintf("%03d", 0:5), ".DT1"))
# 
# 
# 
# x <- GPRsurvey(LINES, verbose = FALSE)
# 
# x[[1]] + x[[1]]
# spAngle(x)
# 
# xr <- spGeoref(x)
# xr <- spGeoref(x, alpha = spAngle(x))
# 
# plot(x, parMarkers = NULL, parArrows = NULL, parIntersect = NULL)
# plot( spOBB(x), add = TRUE)
# plot(xr, parMarkers = NULL, parArrows = NULL, parIntersect = NULL, add = TRUE, parLines = list(col = "red"))
# plot(spOBB(xr), add = TRUE, border = "red")
# 
# 
# xsf_chull <- spConvexHull(x)
# 
# xsf_chull <- sf::st_buffer(xsf_chull, 10)
# 
# xsf_chull_bbox <- spOBB(xsf_chull)
# plot(xsf_chull, add = TRUE)
# 
# X <- interpSlices(x[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "obbox")
# X <- interpSlices(x[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "obbox", buffer = NULL)
# X <- interpSlices(x[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "obbox", buffer = 5)
# X <- interpSlices(x[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "obbox", shp = xsf_chull)
# X <- interpSlices(x[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "bbox", shp = xsf_chull)
# X <- interpSlices(x[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "chull")
# X <- interpSlices(x[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "chull", shp = xsf_chull)
# X <- interpSlices(x[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "chull", shp = xsf_chull, buffer = 2)
# X <- interpSlices(x[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "obbox", buffer = 15)
# X <- interpSlices(x[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "bbox")
# X <- interpSlices(x[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "bbox", buffer = 5)
# X <- interpSlices(x[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "buffer", buffer = 5)
# X <- interpSlices(x[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "bbox", rot = TRUE)
# 
# X@dz
# 
# 
# # Xx <- X@center[1] + seq(0, by = X@dx, length.out = dim(X@data)[1])
# # Xy <- X@center[2] + seq(0, by = X@dy, length.out = dim(X@data)[2])
# 
# plot3D::image2D(x = X@x, y = X@y, z = X@data[,,15], asp = 1, ylim = c(5118160, 5118240))
# #plot3D::image2D(x = Xx, y = Xy, z = X@data[,,15], asp = 1, ylim = c(5118160, 5118240))
# plot(x, add = TRUE)
# 
# plot(xsf_chull, add = TRUE)
# 
# X15 <- X[,, 15]
# 
# plot3D::image2D(x = X10@x, y = X10@y, z = X15@data, asp = 1, ylim = c(5118160, 5118240))
# 
# plot(X)
# plot(X[,,1], add = TRUE)
# plot(X[,,12])
# plot(X[100,,])
# plot(X[,120,])
# 
# 
# 
# X[,,1]
# # 
# 
# dim(X)
# 
# length(X)
# ncol(X)
# nrow(X)
# mean(X, na.rm = TRUE) - mean(X@data, na.rm = TRUE)
# median(X)
# summary(X)
# 
# 
# 
# 
# 
# 
# 




# 
# LINES <- file.path("/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2011_10_10_flagogna",
#                    paste0("XLINE", sprintf("%03d", 0:5), ".DT1"))
# 
# z <- GPRsurvey(LINES, verbose = FALSE)
# zr <- spGeoref(z)
# X1 <- interpSlices(z[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "obbox", rot = FALSE)
# X1 <- interpSlices(z[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "obbox", rot = FALSE, buffer = 15)
# X2 <- interpSlices(z[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "bbox", buffer = 15, rot = TRUE)
# X2 <- interpSlices(z[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "chull", buffer = 15, rot = TRUE)
# X2 <- interpSlices(z[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "bbox", buffer = 0, rot = TRUE)
# 
# plot(X1[,,1])
# plot(z, add = TRUE)
# 
# plot(X2[,,1])
# plot(z, add = TRUE)
# 
# 
# plot(X2[,10,])
# plot(X2[10,,])
# 
# plot(X2[200:500,20:120,][,,1])











#----------------------------------------------------------------#
# 
# plot(z)
# plot( spOBB(z), add = TRUE)
# 
# X <- X2
# x <- X[,,1]
# vx <- x@center[1] + seq(0, by = x@dx, length.out = dim(x@data)[1])
# vy <- x@center[2] + seq(0, by = x@dy, length.out = dim(x@data)[2])
# 
# 
# rot_center <- .georef(matrix(X@center[1:2], nrow = 1, ncol = 2),
#                       alpha = -zr@transf[5],
#                       cloc  = zr@transf[3:4],
#                       creg  = zr@transf[1:2])
# 
# points(X@rot[1], X@rot[2], pch = 20, col = "green", cex = 2)
# points(rot_center[1],rot_center[2], pch = 20, col = "yellow", cex = 2)
# points(X@center[1], X@center[2], pch = 20, col = "blue", cex = 2)
# abline(v = X@rot[1], h = X@rot[2])
# 
# 
# xy_corners <- rbind(c(vx[1], vy[1]),
#                     c(vx[1], tail(vy, 1)),
#                     c(tail(vx, 1), vy[1]),
#                     c(tail(vx, 1), tail(vy, 1)))
# xy_cornersnew <- .georef(xy_corners,
#                          alpha = -zr@transf[5],
#                          cloc  = zr@transf[3:4],
#                          creg  = zr@transf[1:2])
# 
# points(xy_cornersnew)
# points(xy_corners, col = "red")
# 
# x_norm <- (x@data - min(x@data, na.rm = TRUE))/(max(x@data, na.rm = TRUE) - min(x@data, na.rm = TRUE))
# uu <- as.raster(t(x_norm))
# uu[] <- (palCol(t(x_norm[,ncol(x_norm):1])))
# 
# xclip <- NULL
# if(is.null(xclip)){
#   xclip <- c(quantile(as.vector(x@data), 0.99, na.rm = TRUE),
#              quantile(as.vector(x@data), 0.01, na.rm = TRUE))
# }
# x <- clip(x, xclip = xclip, track = FALSE)
# x_max <- max(abs(x@data), na.rm = TRUE)
# x_nrm <- (x@data + x_max)/(2 * x_max)
# uu <- as.raster(t(x_nrm))
# uu[] <- (palCol(t(x_nrm[,ncol(x_nrm):1])))
# 
# col <- palGPR(n = 101)
# range(x_nrm *(length(col) - 1) + 1)
# 
# # return(col[ CCY*(length(col) - 1) + 1 ] 
#    
#        
# plot(z)
# plot( spOBB(z), add = TRUE)
# rasterImage(uu,
#             xleft = rot_center[1],
#             ybottom = rot_center[2],
#             xright = rot_center[1] + (tail(vx, 1) - vx[1]),
#             ytop = rot_center[2]  + (tail(vy, 1)  - vy[1]),
#             angle = x@rot[3]/pi * 180)
# 
# plot(z, add = TRUE)
# 
# 
# image(as.matrix(palCol(x_norm)))
# 
# xy <- cbind(vx, vy)
# plot(vx, vy)
# vx_new <- .georef(cbind(vx, vy[1]),
#                       alpha = -zr@transf[5],
#                       cloc  = zr@transf[3:4],
#                       creg  = zr@transf[1:2])
# vy_new <- .georef(cbind(vx[1], vy),
#                       alpha = -zr@transf[5],
#                       cloc  = zr@transf[3:4],
#                       creg  = zr@transf[1:2])
# 
# plot(vx_new[,1],vy_new[,2])
# 
# plot3D::image2D(X@data[,,1],  theta = -x@rot[3]/pi * 180, asp = 1)
# u <- plot3D::image2D(x = vx - min(vx), y = vy - min(vy), z = X@data[,,1],  theta = -x@rot[3]/pi * 180, asp = 1)
# plot3D::image2D(x = vx, y = vy, z = X@data[,,1],  theta = -x@rot[3]/pi * 180, asp = 1)
# plot3D::image2D(x = vx_new[,1], y = vy_new[,2], z = X@data[,,1],  theta = -x@rot[3]/pi * 180, asp = 1)
# 
# plot(z, add = TRUE)
# 
# 
# rasterImage(as.raster(x_norm),
#             xleft = vx[1] + rot_center[1],
#             ybottom = vy[1] + rot_center[2],
#             xright = tail(vx, 1) + rot_center[1],
#             ytop = tail(vy, 1) + rot_center[2],
#             angle = x@rot/pi * 180,
#             asp = 1)
# 
# 
# 
# # z <- X[100,,]
# # z@z
# # diff(z@x)
# # z@xlab
# # z@zlab
# # z@zunit
# # z@xunit
# # z@vel
# # X@vel
# #
# # plot3D::image2D(z@data)
# #
# # dim(z@data)
# # dim(X@data)
