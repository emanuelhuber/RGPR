
DIR <- system.file(c("inst", "2014_03_10_isola_grid"), package = "RGPR")

dsn <- list.files(
  path = DIR,
  pattern = "\\.DT1$",
  full.names = TRUE
)

x <- readGPR(dsn[3])
plot(x)

plot(coordinates(x))

z <- GPRsurvey(dsn, "survey.h5", overwrite = TRUE)

plot(z)
z@coords
xstart <- rep(0, 40)
xstart[c(3, 5)] <- 1
gridCoords(z) <- list(xlines = 1:40,
                           x   = seq(0, by = 2, length.out = 40),
                           xstart = xstart,
                           ylines = 40 + (1:6),
                           y   = c(0, 1, 2, 4, 6, 7.6),
                           ystart = c(-2, 0, 0, 0, 0, 0))
xstart <- rep(0, 40)
xstart[c(3, 5)] <- 1plot(SU_img, markers = NULL)
