# 
# 
library(RGPR)
dsn0 <- c("/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.HD",
          "/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.DT1",
          "/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.GPS")


zgps <- readGPS(dsn0[3])

x <- readGPR(dsn0)



plot(x)
plot(x)
plot(x[, 1])
plot(x[1,])
plot(x[, 1])

plot(x, xlab = "lkj")
plot(x, ylim = c(0, 150))
plot(x, xlab = "lkj", ylim = c(0, 150))
plot(x, xlab = "lkj", xlim = c(0, 50))
plot(x, xlab = "lkj", ylim = c(0, 150))
plot(x, xlab = "lkj", ylim = c(0, 150))
plot(x, xlab = "lkj", xlim = c(100, 156))
plot(x, xlab = "lkj", add = TRUE)
plot(x, xlab = "lkj", xlim = c(70, 156))

plot(x, xlab = "lkj", ylim = c(20, 356), cbar = NULL)
plot(x, xlab = "lkj", ylim = c(20, 356), cbar = FALSE)
plot(x, xlab = "lkj", ylim = c(20, 356), cbar = TRUE)
plot(x, xlab = "lkj", ylim = c(20, 356), asp = 1)
plot(x, xlab = "lkj", ylim = c(10, 56), asp = 2)
plot(x, xlab = "lkj", ylim = c(10, 56), asp = 0.5)
plot(x, xlab = "lkj", asp = NA)
plot(x, xlab = "lkj", asp = 0.5)
plot(x, xlab = "lkj", asp = 2)
plot(x, xlab = "lkj", asp = 1)
plot(x, xlab = "lkj", secaxis = FALSE)
plot(x, xlab = "lkj", secaxis = TRUE)
plot(x, xlab = "lkj", secaxis = TRUE, ylim = c(30, 150))
plot(x, xlab = "lkj", secaxis = TRUE, ylim = c(30, 100))
plot(x, xlab = "lkj", ylim = c(-120, 256))
plot(x, xlab = "lkj", ylim = c(0, 256), z0 = FALSE)
plot(x, xlab = "lkj", ylim = c(0, 256), z0 = TRUE)
plot(x, xlab = "lkj", ylim = c(0, 256), z0 = list(lwd = 4, col = "firebrick"))
plot(x, xlab = "lkj", ylim = c(-120, 256), markers = FALSE)
plot(x, xlab = "lkj", ylim = c(-120, 256), markers = list(bgSymbols = "blue"))

plot(x, type = "wiggles", xlim = c(0, 26))

z <- convertTimeToDepth(x)

plot(z)
plot(z, secaxis = FALSE)
plot(z, secaxis = TRUE, elev = TRUE)


# LINES <- file.path("/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2011_10_10_flagogna",
#                     paste0("XLINE", sprintf("%03d", 0:5), ".DT1"))
# 

# z <- GPRsurvey(LINES, verbose = FALSE)
# 
# 
# #------------------- PLOT GPRsurvey
# plot(z, col = "red")
# 
# plot(z, parLines = NULL, parMarkers = NULL, parArrows = NULL, parIntersect = NULL)
# plot(z, parLines = NULL, parMarkers = NULL, parIntersect = NULL)  # arrows
# plot(z, parMarkers = NULL, parArrows = NULL, parIntersect = NULL) # lines
# plot(z, parLines = NULL, parArrows = NULL, parIntersect = NULL)   # markers
# plot(z, parLines = NULL, parArrows = NULL, parMarkers = NULL)   # intersections
# plot(z, parLines = NULL, parArrows = NULL, parMarkers = list(pch = 3), parIntersect = NULL)   # markers
# plot(z, parMarkers = NULL)
# plot(z, parLines = list(col = "red"), parMarkers = NULL)
# plot(z, parLines = list(col = "red", type = "p"), parMarkers = NULL)
# 
# 
# #------------------- PLOT SLICE
# # zr <- spGeoref(z)
# X1 <- interpSlices(z[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "obbox", rot = FALSE)
# X2 <- interpSlices(z[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "bbox", buffer = 0, rot = TRUE)
# 
# plot(X1[,,1])
# plot(z, add = TRUE)
# 
# plot(X2[,,1])
# plot(z, add = TRUE)
# 
# plot(X2[,10,])
# plot(X2[10,,])
# 
# plot(X2[200:500,20:120,][,,1])
# plot(z, add = TRUE)
# 
# X1 <- interpSlices(z[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "obbox", rot = FALSE, buffer = 15)
# X2 <- interpSlices(z[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "bbox", buffer = 15, rot = TRUE)
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
# plot(X2[300:900, 20:420,][,,1])
# plot(z, add = TRUE)
# 
# X2 <- interpSlices(z[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "chull", buffer = 15, rot = TRUE)
# 
# plot(X2[,,1])
# plot(z, add = TRUE)
# 
# plot(X2[400:900,200:420,])
# plot(z, add = TRUE)
# 
# plot(X2[,10,])
# plot(X2[10,,])
# 
# 
# 
# #------------------- PLOT GPR
# plot(z[[1]])
# plot(z[[2]], type = "wiggles", col = "green")
# plot(z[[2]], type = "contour")
# plot(z[[2]])
# 
# 
# #------------------- PLOT GPR - 1D
# plot(z[[2]][,10], lwd = 4, col = "red")
# plot(z[[2]][,10], lwd = 1, col = "red", type = "b")
# plot(z[[2]][,10], lwd = 1, col = "red", type = "p")
# plot(z[[2]][,10], lwd = 1, col = "red", type = "o", pch = 20)
# 
# 
# #------------------- PLOT TR
# plotTr(z[[2]], add = TRUE, col = "red")
# plotTr(z[[2]])
# plotTr(z[[2]], type = "p", pch = 20)
