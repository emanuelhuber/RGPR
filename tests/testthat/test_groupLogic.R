LINES <- file.path("/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2011_10_10_flagogna",
                    paste0("XLINE", sprintf("%03d", 0:5), ".DT1"))

x <- readGPR(LINES[1], verbose = FALSE)
x2 <- readGPR(LINES[2], verbose = FALSE)

x <- x[1:20, 1:20]
x2 <- x2[1:20, 1:20]

x3 <- x2 > x 
x4 <- x == x2

# !x3

test_that("'coord' for GPRsurvey",{
  expect_equivalent((x3 | x4)@data,
                   x3@data | x4@data)
  
  expect_equivalent((x3 & x4)@data,
                    x3@data & x4@data)
  
  expect_equivalent((!x3)@data,
                    !x3@data )
  
})

