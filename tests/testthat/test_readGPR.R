

dsn0 <- c("/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.HD",
          "/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.DT1",
          "/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/daniel2/LINE06.GPS")

dsn1 <- c("/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/LINE02.HD",
          "/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/LINE02.DT1",
          "/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2014_04_25_frenke/LINE02.txt")

dsn_db_freq <- "/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/dzt/data_Hide_double_frequencies/FILE____001.DZT"

x <- readGPR(dsn0)
x
x <- readGPR(dsn_db_freq)
x

paste0("survey mode: ", x@mode, "; ",
       "line length: ", diff(range(x@x)), " ", x@xunit, "; ",
       "window length = ", diff(range(x@z)), " ", x@zunit, "; ",
       "frequency: ", x@freq, " MHz;")

test_that("'readGPR()': test on argument 'dsn'",{
  expect_message(readGPR(dsn0)) # Coordinates interpolation from GPS data
  expect_silent(readGPR(dsn0, verbose = FALSE)) 
  expect_silent(readGPR(dsn0, interpGPS  = FALSE))
  
  expect_silent(readGPR(dsn1, interpGPS  = FALSE)) 
  expect_silent(readGPR(dsn1, interpGPS  = TRUE))  # no .GPS files
  
  dsn_1 <- file(dsn0[1], 'rb')
  expect_error(readGPR(dsn_1))
  close(dsn_1)
  dsn_1 <- file(dsn0[2], 'rb')
  expect_error(readGPR(dsn_1))
  close(dsn_1)
  dsn_1 <- file(dsn0[3], 'rb')
  expect_error(readGPR(dsn_1))
  close(dsn_1)

  
  dsn <- lapply(dsn0, file, 'rb')
  expect_silent(readGPR(dsn[1:2]))
  lapply(dsn, .closeFileIfNot) 
  
  dsn <- lapply(dsn0, file, 'rb')
  expect_silent(readGPR(dsn[1:2], verbose = FALSE))
  lapply(dsn, .closeFileIfNot)
  
  
  dsn <- lapply(dsn0, file, 'rb')
  expect_error(readGPR(dsn[1]))
  lapply(dsn, .closeFileIfNot)
  
  dsn <- lapply(dsn0, file, 'rb')
  expect_message(readGPR(dsn)) # Coordinates interpolation from GPS data
  
  dsn <- lapply(dsn0, file, 'rb')
  expect_silent(readGPR(dsn, verbose = FALSE)) 
  
  dsn <- lapply(dsn0, file, 'rb')
  expect_silent(readGPR(dsn, interpGPS  = FALSE)) 
  
  expect_error(readGPR(dsn, verbose  = FALSE))  # connections are closed...
})

test_that("'readGPR()': test on other arguments",{
  expect_silent(readGPR(dsn0, verbose = FALSE, Vmax = NULL)) 
  expect_silent(readGPR(dsn0, verbose = FALSE, Vmax = -50)) 
  expect_error(readGPR(dsn0, verbose = FALSE, Vmax = c(-50, 50))) 
  
  expect_error(readGPR(123.234, verbose  = FALSE))
  
  expect_error(readGPR(dsn0, desc  = c("test", "test")))
  
  expect_error(readGPR(dsn0, interpGPS  = c("test", "test")))
  expect_error(readGPR(dsn0, interpGPS  = NULL))
  expect_error(readGPR(dsn0, interpGPS  = rep(FALSE, 2)))

  expect_error(readGPR(dsn0, verbose  = c("test", "test")))
  expect_error(readGPR(dsn0, verbose  = NULL))
  expect_error(readGPR(dsn0, verbose  = rep(FALSE, 2)))
 })

test_that("'readGPR()': test metadata",{
  x <- readGPR(dsn0)
  expect(is.null(metadata(x)$GPS), "metadata(x)$GPS should be NULL")
  x <- readGPR(dsn0, interpGPS = FALSE)
  expect(!is.null(metadata(x)$GPS), "metadata(x)$GPS shouldn't be NULL")
})


