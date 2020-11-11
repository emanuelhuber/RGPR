# LINES <- file.path("/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2011_10_10_flagogna",
#                     paste0("XLINE", sprintf("%03d", 0:5), ".DT1"))
# 
# x <- readGPR(LINES[1]) 
# 
# x2  <- x - min(x@data)
# x2b <- -min(x@data) + x
# x2c <- min(x@data) - x
# plot(x2)
# plot(x2b)
# plot(x2c)
# plot(-x)
# plot(+x)
# 
# # add a trace
# v <- seq(0, by = 1, length.out = nrow(x))
# v <- rowMeans(x@data)
# length(v)
# dim(x)
# 
# plot(x - exp(0.015* v))
# plot(x + v)
# 
# # add a row
# w <- seq(0, by = 1, length.out = ncol(x))
# plot(x + w)
# 
# 
# 
# X <- array(dim = c(dim(x), 2))
# X[,,1] <- x@data
# X[,,2] <- -x@data
# 
# 
# X2 <- X + w
# X2 <- X + v
# 
# plot3D::image2D(X2[,,1])
# plot3D::image2D(X2[,,2])
