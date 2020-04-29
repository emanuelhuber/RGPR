# # 
# # # devtools::install_local("/mnt/data/RGPR/CODE/RGPR", force = TRUE)
# # 
# # # library(RGPR)
# 
# LINES <- file.path("/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2011_10_10_flagogna",
#                    paste0("XLINE", sprintf("%03d", 0:5), ".DT1"))
# 
# 
# 
# x <- GPRsurvey(LINES, verbose = FALSE)
# 
# 
# 
# X <- interpSlices(x[2:6], dx = 0.1, dy = 0.1, dz = 10, extend = "obbox")
# 
# Xx <- X@center[1] + seq(0, by = X@dx, length.out = dim(X@data)[1])
# Xy <- X@center[2] + seq(0, by = X@dy, length.out = dim(X@data)[2])
# 
# plot3D::image2D(x = Xx, y = Xy, z = X@data[,,15], asp = 1, ylim = c(5118160, 5118240))
# plot(x, add = TRUE)
# 
# 
# plot(X)
# 
