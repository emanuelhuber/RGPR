
# devtools::install_local("/mnt/data/RGPR/CODE/RGPR", force = TRUE)

# library(RGPR)

LINES <- file.path("/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2011_10_10_flagogna",
                   paste0("XLINE", sprintf("%03d", 0:5), ".DT1"))



test_that("'spIntersection' for GPRsurvey",{
  expect_silent( x <- GPRsurvey(LINES, verbose = FALSE) )
  x@crs <- rep(x@crs, length(x))
  x@crs[2] <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
  
  expect_warning(as.sf(x))
  expect_warning( x_int <- spIntersection(x) )
  expect_null( x_int@intersections[[1]] )
})
