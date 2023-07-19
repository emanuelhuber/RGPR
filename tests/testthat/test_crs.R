
dsn0 <- c("/home/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.HD",
          "/home/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.DT1",
          "/home/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.GPS")

dsn1 <- c("/home/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/LINE02.HD",
          "/home/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/LINE02.DT1",
          "/home/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/LINE02.txt")


zgps <- readGPS(dsn0[3])

x <- readGPR(dsn0)
x@spunit
x@crs

crs(x) <- 3857
x@crs
x@spunit

crs(x) <- 4326
x@crs
x@spunit


# st_crs(3857)$epsg
# st_crs(3857)$proj4string
# 
# st_crs("+proj=utm +zone=10 +datum=WGS84")$epsg
# st_crs("+proj=utm +zone=10 +datum=WGS84")$proj4string
# 


test_that("test 'crs()",{
  expect_silent(.checkCRS(sf::st_crs(zgps)))
  expect_silent(.checkCRS(3857))
  # expect_silent(.checkCRS("3857"))
  # expect_silent(.checkCRS("+init=epsg:3857 +units=m"))
  expect_silent(.checkCRS("+proj=merc +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +nadgrids=@null +units=m +no_defs "))
  #expect_silent(.checkCRS(st_crs("+init=epsg:3857 +units=m")))
  #expect_silent(.checkCRS(st_crs("+init=epsg:3857 +units=m")[[1]]))
  expect_silent(.checkCRS(st_crs(3857)[[1]]))
  # expect_silent(.checkCRS(sp::CRS("+init=epsg:28992")))
  # expect_silent(.checkCRS(sp::CRS("+proj=utm +zone=10 +datum=WGS84")))
  expect_silent(.checkCRS(st_crs(3857)$proj4string))
  expect_silent(.checkCRS(st_crs(3857)$epsg))
  # expect_silent(.checkCRS(""))
  expect_silent(.checkCRS(NA))
  expect_warning(.checkCRS(st_crs("+init=epsg:3857 +units=m")[[1]]))
  expect_silent(.checkCRS(st_crs("+init=epsg:3857 +units=m")[1]))
})

