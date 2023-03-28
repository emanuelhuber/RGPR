

dsn0 <- c("/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.HD",
          "/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.DT1",
          "/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.GPS")

dsn1 <- c("/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/LINE02.HD",
          "/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/LINE02.DT1",
          "/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/LINE02.txt")

# double Frequency
dsn_db_freq <- "/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/dzt/data_Hide_double_frequencies/FILE____001.DZT"

x <- readGPR(dsn0)
x

# 
# x <- readGPR(dsn1)
# x_clip <- x
# x_clip@data <- .clipMat(x@md$clip, n = nrow(x))
# plot(x_clip)
# 
# plot(x_clip[,1:20])
# x_clip1 <- x[, 1:20]
# x_clip1@data <- .clipMat(x_clip1@md$clip, n = nrow(x_clip1))
# plot(x_clip1)
# 
# plot(x_clip[1:200,])
# x_clip1 <- x[1:200,]
# x_clip1@data <- .clipMat(x_clip1@md$clip, n = nrow(x_clip1))
# plot(x_clip1)
# 
# 
# plot(x_clip[1:200,1:20])
# 
# dim(x)
# 
# x <- readGPR(dsn_db_freq)
# x
# 
# # CMP
# dsn_cmp <- "/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/CMP.DT1"
# x <- readGPR(dsn_cmp)
# plot(x)
# 
# # WARR
# dsn_warr <- "/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/WAR_Guillaume/Line0058.DT1"
# x <- readGPR(dsn_warr)
# x
# 
# plot(x, xaxis = "r")
# 
# plot(x[1:100, ])
# 
# 
# plot3D::image2D(t(as.matrix(x)), xaxis = "r")
# image(as.matrix(x))
# 
# # plot(x)
# # plot(x[100:400,30:200,])
# # 
# # plotTr(x)
# # plotTr(x[,,1], add = TRUE, col = "blue")
# # plotTr(x[,,2], add = TRUE, col = "red")
# # 
# # x@freq
# # x@y
# 
# test_that("'readGPR()': test on argument 'dsn'",{
#   expect_message(readGPR(dsn0)) # Coordinates interpolation from GPS data
#   expect_silent(readGPR(dsn0, verbose = FALSE)) 
#   expect_silent(readGPR(dsn0, interpGPS  = FALSE))
#   
#   expect_silent(readGPR(dsn1, interpGPS  = FALSE)) 
#   expect_silent(readGPR(dsn1, interpGPS  = TRUE))  # no .GPS files
#   
#   dsn_1 <- file(dsn0[1], 'rb')
#   expect_error(readGPR(dsn_1))
#   close(dsn_1)
#   dsn_1 <- file(dsn0[2], 'rb')
#   expect_error(readGPR(dsn_1))
#   close(dsn_1)
#   dsn_1 <- file(dsn0[3], 'rb')
#   expect_error(readGPR(dsn_1))
#   close(dsn_1)
# 
#   
#   dsn <- lapply(dsn0, file, 'rb')
#   expect_silent(readGPR(dsn[1:2]))
#   lapply(dsn, .closeFileIfNot) 
#   
#   dsn <- lapply(dsn0, file, 'rb')
#   expect_silent(readGPR(dsn[1:2], verbose = FALSE))
#   lapply(dsn, .closeFileIfNot)
#   
#   
#   dsn <- lapply(dsn0, file, 'rb')
#   expect_error(readGPR(dsn[1]))
#   lapply(dsn, .closeFileIfNot)
#   
#   dsn <- lapply(dsn0, file, 'rb')
#   expect_message(readGPR(dsn)) # Coordinates interpolation from GPS data
#   
#   dsn <- lapply(dsn0, file, 'rb')
#   expect_silent(readGPR(dsn, verbose = FALSE)) 
#   
#   dsn <- lapply(dsn0, file, 'rb')
#   expect_silent(readGPR(dsn, interpGPS  = FALSE)) 
#   
#   expect_error(readGPR(dsn, verbose  = FALSE))  # connections are closed...
# })
# 
# test_that("'readGPR()': test on other arguments",{
#   expect_silent(readGPR(dsn0, verbose = FALSE, Vmax = NULL)) 
#   expect_silent(readGPR(dsn0, verbose = FALSE, Vmax = -50)) 
#   expect_error(readGPR(dsn0, verbose = FALSE, Vmax = c(-50, 50))) 
#   
#   expect_error(readGPR(123.234, verbose  = FALSE))
#   
#   expect_error(readGPR(dsn0, desc  = c("test", "test")))
#   
#   expect_error(readGPR(dsn0, interpGPS  = c("test", "test")))
#   expect_error(readGPR(dsn0, interpGPS  = NULL))
#   expect_error(readGPR(dsn0, interpGPS  = rep(FALSE, 2)))
# 
#   expect_error(readGPR(dsn0, verbose  = c("test", "test")))
#   expect_error(readGPR(dsn0, verbose  = NULL))
#   expect_error(readGPR(dsn0, verbose  = rep(FALSE, 2)))
#  })
# 
# test_that("'readGPR()': test metadata",{
#   x <- readGPR(dsn0)
#   expect(is.null(metadata(x)$GPS), "metadata(x)$GPS should be NULL")
#   x <- readGPR(dsn0, interpGPS = FALSE)
#   expect(!is.null(metadata(x)$GPS), "metadata(x)$GPS shouldn't be NULL")
# })
# 

