
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


