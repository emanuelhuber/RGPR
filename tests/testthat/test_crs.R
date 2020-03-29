

dsn0 <- c("/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.HD",
          "/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.DT1",
          "/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.GPS")

dsn1 <- c("/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/LINE02.HD",
          "/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/LINE02.DT1",
          "/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/LINE02.txt")


zgps <- readGPS(dsn0[3])


test_that("test 'crs()",{
  expect_silent(.checkCRS(sf::st_crs(zgps)))
  expect_silent(.checkCRS(3857))
  expect_silent(.checkCRS("3857"))
  expect_silent(.checkCRS(st_crs(3857)$proj4string))
  expect_silent(.checkCRS(st_crs(3857)$epsg))
  expect_silent(.checkCRS(""))
  expect_silent(.checkCRS(NA))
  expect_warning(.checkCRS(st_crs("+init=epsg:3857 +units=m")[1]))
})
