
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



# 
# 4326
# 
# sf::st_crs(4326)$proj4string
# 
# trimStr(sf::st_crs(4326)$proj4string) == "+proj=longlat +datum=WGS84 +no_defs" 
# 
# x_crs <- sapply(x@crs, .checkCRS, USE.NAMES = FALSE)
# ucrs <- unique(x_crs[!is.na(x_crs)])
# if(length(ucrs) == 1){
#   return( ucrs )
# }else if(length(ucrs) == 0){
#   return(NA_character_)
# }else{
#   return( x_crs )
# }
# 
# .checkCRSsurvey(x@crs)
# x@crs
# sf::st_crs(x@crs)$epsg
# sf::st_crs(x@crs)$proj4string
# 
# sfc = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
# sf = st_sf(a = 1:2, geom = sfc)
# st_crs(sf) = 4326
# st_crs(sf)$epsg
# st_crs(sf) = x@crs
# st_crs(sf)$epsg
# 
# z <- readGPR(LINES[2])
# z@crs
# x@crs
# 
# x <- c(NA_character_, rep(z@crs, 5))
# x@crs(x)
# 
# .checkCRSsurvey <- function(x){
#   ucrs <- unique(x[!is.na(x)])
#   if(length(ucrs) == 1){
#     return( .checkCRS(ucrs) )
#   }else if(length(ucrs) == 0){
#     return(NA_character_)
#   }else{
#     sapply(x, .checkCRS, USE.NAMES = FALSE)
#   }
# }
# dim(y@coord)
# dim(x[[2]]@coord)
# y@ann
# x[[2]]@ann
# x[[2]]@coord == y@coord

# 
# 
# x@coords
# x@intersections[[3]]
# y <- x[[3]]
# y <- getGPR(x, id = 3)
# ann(y)
# 
# plot(y)
# 
# y
# 
# id <- 3
# if(length(id)>1){
#   warning("Length of id > 1, I take only the first element!\n")
#   id <- id[1]
# }
# if(is.numeric(id)){
#   no <- id
#   gpr <- readGPR(x@paths[[id]])
# }else if(is.character(id)){
#   no <- which(x@names == trimStr(id))
#   if(length(no > 0)){
#     id <- no
#     gpr <- readGPR(x@paths[[id]])
#   }else{
#     stop("There is no GPR data with the name '", trimStr(id),"'\n")
#   }
# }
# # if(length(x@coords[[i]])>0){
# gpr@coord <- x@coords[[id]]
# #}
# if(length(x@intersections[[id]]) > 0 ){
#   print("lkjlkjljljölkj")
#   FUN <- function(y, x){
#     closestTr(x, y = y)
#   }
#   x_tr <- apply(x@intersections[[id]], 1, FUN, gpr)
#   ann(gpr) <- cbind(x_tr, as.character(x@intersections[[id]]$name))
# 
#   # ann(gpr) <- cbind(x@intersections[[id]]$trace,
#   # x@intersections[[id]]$name)
# }
# 
# 
# # x <- GPRsurvey(LINES, verbose = FALSE)
# # x@intersections
# # x[[3]]@ann
# # 
# # 
# # i <- 2
# # xi <- x[[2]]
# # xi_ann <- x@intersections[[i]]
# # 
# # xi_coord <- x@coords[[i]]
# # 
# # closestTr(x[[i]], y = xi_ann[1,1:2])
# # 
# # FUN <- function(y, x){
# #   closestTr(x, y = y)
# # }
# # 
# # x_tr <- apply(xi_ann[, 1:2], 1, FUN, x[[i]])
# # 
# # data.frame(x_tr, xi_ann[,4])
# # 
# # xi@ann <- rep("", ncol(xi))
# # 
# # ann(xi) <- cbind(x_tr, as.character(xi_ann$name))
# # xi@ann[x_tr] <- as.character(xi_ann$name)
# # 
# # plot(coord(x[[i]]))
# # 
# # points(t(coord(x[[i]])[129, ]), col = "red", pch = 20)
# # 
# # points((xi_ann[1, 1:2]), col = "green", pch = 20)
# 
# 
# 
# 
