# 
# devtools::install_local("/mnt/data/RGPR/CODE/RGPR", force = TRUE)
# 
# library(RGPR)
# 

LINES <- file.path("/home/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2011_10_10_flagogna",
                   paste0("XLINE", sprintf("%03d", 0:5), ".DT1"))
LINES_GPS <- file.path("/home/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2011_10_10_flagogna",
                   paste0("XLINE", sprintf("%03d", 0:5), ".GPS"))

x <- GPRsurvey(LINES, verbose = FALSE)
x_coords <- x@coords




# 
# x <-readGPR(LINES[2], interpGPS = FALSE)
# x_gps <- readGPS(LINES_GPS[2])
# x2 <- interpCoords(x, x_gps)
# plot(x)
# plot(x2)
# plot(coordinates(x), asp = 1)
# plot(sf::st_geometry(x_gps), add = TRUE, col = "red")
# 
# plot(sf::st_geometry(x_gps), col = "red", type = "o")
# points(coordinates(x2), cex = 0.75, pch = 20, col = "green")
# 
# xc <- sf::st_coordinates(x_gps)
# xcd <- dropDuplicatedCoords(xc)
# 
# plot(xc, type = "o", asp = 1)
# points(xcd, pch = 20, col = "green")
# # x <- readGPR(dsn0[1:2], interpGPS = FALSE)
# # x_gps <- readGPS(dsn0[3])
# x <- readGPR(LINES[2], verbose = TRUE)
# x_gps <- readGPS(LINES_GPS[2])
# x_int <- spInterp(x, topo = x_gps)
# expect(nrow(x_int@coord) == ncol(x_int), "not same number coordinates as traces!!")
# 
# pathRelPos(sf::st_coordinates(x_gps))
# spPathRel(x)
# 
# x@coord <- sf::st_coordinates(x_gps)
# 
# RGPR:::.updateXpos(x)


y <- x[2]

coordinates(y) <- list(matrix(nrow = 0, ncol =3, dimnames = list(NULL, c("x", "y", "z"))) )

test_that("'coord' for GPRsurvey",{
  expect_identical(x_coords, coordinates(x))
  
  # i.O.
  expect_silent(coordinates(x)[2] <- list(matrix(nrow = 0, ncol =3, dimnames = list(NULL, c("x", "y", "z"))) )  )
  
  # somehow not working
  expect_error(coordinates(x[2]) <- list(matrix(nrow = 0, ncol =3, dimnames = list(NULL, c("x", "y", "z"))) )  )
  # but that works
  expect_silent(coordinates(y) <- list(matrix(nrow = 0, ncol =3, dimnames = list(NULL, c("x", "y", "z"))) )  )
  
  
  # i.O.
  expect_silent(coordinates(x)[[3]] <- matrix(nrow = 0, ncol =3, dimnames = list(NULL, c("x", "y", "z")))  )
  # Error
  expect_error(coordinates(x)[[4]] <- matrix(10, nrow = 10, ncol = 3) )
  # i.O
  expect_silent(coordinates(x)[[4]] <- matrix(10, nrow = 124, ncol = 3) )
  # error
  expect_error(coordinates(x)[[5]] <- matrix(10, nrow = 10, ncol = 5) )

  
  expect_silent(coordinates(x) <- x_coords )
  
})

