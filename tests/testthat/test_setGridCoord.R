DIR <- "/mnt/data/RGPR/CODE/RGPR-gh-pages/GPRdata-master/exampleDataCube/Grid-dir1-Rawdata"

LINES <- file.path(DIR,  paste0("FILE____", sprintf("%03d", 1:46), ".DZT"))


SU <- GPRsurvey(LINES, verbose = FALSE)

setGridCoord(SU) <- list(xlines = seq_along(SU),
                         xpos   = seq(0, 
                                      by = 0.2, 
                                      length.out = length(SU)),
                         xstart = runif(length(SU), -2, 2),
                         ylines = NULL)

plot(SU, type = "l", parFid = NULL)

SU@fids <- NULL


setGridCoord(SU) <- list(ylines = seq_along(SU),
                         ypos   = seq(0, 
                                      by = 0.2, 
                                      length.out = length(SU)),
                         ystart = runif(length(SU), -2, 2),
                         xlines = NULL)
plot(SU, type = "l", parFid = NULL)


setGridCoord(SU) <- list(ylines = 1:20,
                         ypos   = seq(2, 
                                      by = 0.2, 
                                      length.out = 20),
                         ystart = runif(20, -2, 2),
                         xlines = 21:length(SU),
                         xpos = seq(2, by = 0.2, length.out = length(SU) - 20),
                         xstart = runif(length(SU) - 20, -1, 1))

plot(SU, type = "l", parFid = NULL, asp = 1)

