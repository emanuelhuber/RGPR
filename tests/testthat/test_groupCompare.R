LINES <- file.path("/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2011_10_10_flagogna",
                    paste0("XLINE", sprintf("%03d", 0:5), ".DT1"))

x <- readGPR(LINES[1])
x2 <- readGPR(LINES[2])

x <- x[1:20, 1:20]
x2 <- x2[1:20, 1:20]

# !(x2 > x)

test_that("'coord' for GPRsurvey",{
  expect_equivalent((x2 > x)@data,
                   x2@data > x@data)
  
  expect_equivalent((x2 < x)@data,
                   x2@data < x@data)
  
  expect_equivalent((x2 >= x)@data,
                   x2@data >= x@data)
  
  
  expect_equivalent((x2 <= x)@data,
                   x2@data <= x@data)
  
  
  expect_equivalent((x2 == x)@data,
                   x2@data == x@data)
  
  expect_equivalent((x2 != x)@data,
                   x2@data != x@data)
  
})

