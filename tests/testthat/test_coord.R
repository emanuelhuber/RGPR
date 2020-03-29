# 
# devtools::install_local("/mnt/data/RGPR/CODE/RGPR", force = TRUE)
# 
# library(RGPR)
# 

LINES <- file.path("/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2011_10_10_flagogna",
                   paste0("XLINE", sprintf("%03d", 0:5), ".DT1"))

x <- GPRsurvey(LINES, verbose = FALSE)
x_coords <- x@coords


test_that("'coord' for GPRsurvey",{
  expect_identical(x_coords, coord(x))
  
  # i.O.
  expect_silent(coord(x)[2] <- list(matrix(nrow = 0, ncol =0) )  )
  # i.O.
  expect_silent(coord(x)[[3]] <- matrix(nrow = 0, ncol =0) )
  # Error
  expect_error(coord(x)[[4]] <- matrix(10, nrow = 10, ncol = 3) )
  # i.O
  expect_silent(coord(x)[[4]] <- matrix(10, nrow = 124, ncol = 3) )
  # error
  expect_error(coord(x)[[5]] <- matrix(10, nrow = 10, ncol = 5) )

  
  expect_silent(coord(x) <- x_coords )
  
})

