
# devtools::install_local("/mnt/data/RGPR/CODE/RGPR", force = TRUE)

# library(RGPR)


LINES <- file.path("/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2011_10_10_flagogna",
                   paste0("XLINE", sprintf("%03d", 0:5), ".DT1"))
LINES_GPS <- file.path("/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2011_10_10_flagogna",
                       paste0("XLINE", sprintf("%03d", 0:5), ".GPS"))
LINES_rds <- file.path("/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2011_10_10_flagogna/test",
                       paste0("XLINE", sprintf("%03d", 0:5), ".rds"))

test_that("'GPRsurvey()'",{
  expect_silent(x <- GPRsurvey(LINES, verbose = FALSE))
  expect_length(length(x@crs), 1L)
  expect_message(x <- GPRsurvey(LINES_rds))
  expect_length(length(x@crs), 1L)
  expect_true(length(x@names) == length(LINES) )
  expect_warning(x <- GPRsurvey(LINES))
  expect_length(length(x@crs), 1L)
  y <- readGPR(LINES[2])
})

