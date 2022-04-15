

dsn0 <- c("/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.HD",
          "/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.DT1",
          "/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODEDEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.GPS")

dsn1 <- c("/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/LINE02.HD",
          "/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/LINE02.DT1",
          "/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/LINE02.txt")

# double Frequency
dsn_db_freq <- "/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/dzt/data_Hide_double_frequencies/FILE____001.DZT"

x <- readGPR(dsn0)
x


x <- readGPR(dsn1)
x_clip <- x
x_clip@data <- .clipMat(x@md$clip, n = nrow(x))
plot(x_clip)

plot(x_clip[,1:20])
x_clip1 <- x[, 1:20]
x_clip1@data <- .clipMat(x_clip1@md$clip, n = nrow(x_clip1))
plot(x_clip1)

plot(x_clip[1:200,])
x_clip1 <- x[1:200,]
x_clip1@data <- .clipMat(x_clip1@md$clip, n = nrow(x_clip1))
plot(x_clip1)

plot(x_clip[1:200,1:30])
x_clip1 <- x[1:200,1:30]
x_clip1@data <- .clipMat(x_clip1@md$clip, n = nrow(x_clip1))
plot(x_clip1)

plot(x_clip[1:190,1:30])
x_clip1 <- x[1:190,1:30]
x_clip1@data <- .clipMat(x_clip1@md$clip, n = nrow(x_clip1))
plot(x_clip1)

