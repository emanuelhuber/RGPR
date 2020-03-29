# devtools::install_local("/mnt/data/RGPR/CODE/RGPR", force = TRUE)

# library(RGPR)

dsn0 <- c("/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.HD",
          "/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.DT1",
          "/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.GPS")


dsn1 <- c("/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/LINE02.HD",
          "/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/LINE02.DT1",
          "/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/LINE02.txt",
          "/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/FID_LINE02.txt")

gjson <- "/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/xy_south.geojson"
gjson <- "/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/xy.geojson"



test_that("Manual interpolation of GPS data",{
  # 1. From GPS data 
  x <- readGPR(dsn0[1:2], interpGPS = FALSE)
  x_gps <- readGPS(dsn0[3])
  x_int <- spInterp(x, topo = x_gps)
  expect(nrow(x_int@coord) == ncol(x_int), "not same number coordinates as traces!!")
  
  # 2. From meta-data
  x <- readGPR(dsn0, interpGPS = FALSE)
  x_md <- metadata(x)
  x_int <- spInterp(x, topo = x_md$GPS)
  expect(nrow(x_int@coord) == ncol(x_int), "not same number coordinates as traces!!")
  
  x <- readGPR(dsn1, interpGPS = FALSE)
  # interpolate FIDs
  expect_error(FID <- readFID(dsn1[3]))  # error
  expect_warning(FID <- readFID(dsn1[4]))
  x_int <- spInterp(x, topo = FID)
  expect(nrow(x_int@coord) == ncol(x_int), "not same number coordinates as traces!!")
  
  # interpolate topo
  topo <- readTopo(dsn1[3])
  x_int <- spInterp(x, topo = topo)
  expect_true( all.equal(coord(x_int), as.matrix(topo), check.attributes = FALSE) )
  
  # coord(x_int) <- topo
  
  # interpolate geojson
  xyz <- geojsonsf::geojson_sf(gjson)
  x_int <- spInterp(x, topo = xyz)
  expect(nrow(x_int@coord) == ncol(x_int), "not same number coordinates as traces!!")
  # plot(x_gps)
  
  # interpolate GPGGA files
  gpgga <- "/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/SEGY/Ali_Khankan/2019-10-10-006.txt"
  xyz <- readGPGGA(gpgga)
  
  tt <- as.character(as.POSIXct(xyz$time))
  x_int <- spInterp(x, topo = xyz, tt = tt)
  tt <- as.character(as.numeric(xyz$time))
  x_int <- spInterp(x, topo = xyz, tt = tt)
  x_int <- spInterp(x, topo = xyz, tt = as.numeric(xyz$time) )
  x_int <- spInterp(x, topo = xyz, tt = xyz$time) 
  
})

