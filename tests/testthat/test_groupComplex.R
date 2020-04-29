LINES <- file.path("/mnt/data/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2011_10_10_flagogna",
                    paste0("XLINE", sprintf("%03d", 0:5), ".DT1"))

x <- readGPR(LINES[1])

x2  <- x + complex(imaginary = 1)


test_that("'coord' for GPRsurvey",{
  expect_equivalent(Im(x2)@data[1:5,1:5],
                   matrix(1, nrow = 5, ncol = 5))
  expect_equivalent(Re(x2)@data[1:5,1:5],
                   x@data[1:5,1:5])
  expect_equivalent(Conj(x2)@data[1:5,1:5],
                    x@data[1:5,1:5] - complex(imaginary = 1))
  expect_equivalent(Arg(x2)@data[1:5,1:5],
                    atan2(Im(x2),Re(x2))@data[1:5,1:5])
  expect_equivalent(Mod(x2)@data[1:5,1:5],
                    sqrt(x@data[1:5,1:5]^2 + 1))
  
})

