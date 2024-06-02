

#' @name exportCoord
#' @rdname exportCoord
#' @export
setGenericVerif("exportCoord",  
                function(x, type = c("SpatialPoints", "SpatialLines", "ASCII"),
                         fPath = NULL, driver = "ESRI Shapefile", ...) standardGeneric("exportCoord"))



#' Export the trace coordinates.
#'
#' @name exportCoord
#' @rdname exportCoord
#' @export
setMethod("exportCoord", "GPR", 
          function(x, type = c("SpatialPoints", "SpatialLines", "ASCII"),
                   fPath = NULL, driver = "ESRI Shapefile", ...){
            type <- match.arg(type, c("SpatialPoints", "SpatialLines", "ASCII"))
            # fPath <- ifelse(is.null(fPath), x@name, 
            #                 file.path(dirname(fPath), .fNameWExt(fPath))) 
            if(type=="SpatialLines"){  
              xs <- as.spatialLines(x)
              sf::st_write(xs, fPath, delete_dsn = TRUE)
              # dfl <- data.frame(z=c(1), row.names = x@name)
              # spldf <- sp::SpatialLinesDataFrame(mySpatLines, dfl , 
              #                                    match.ID = TRUE)
              # rgdal::writeOGR(obj = spldf, dsn = dirname(fPath), layer = basename(fPath), 
              #                 driver = driver, check_exists = TRUE, 
              #                 overwrite_layer = TRUE, delete_dsn = TRUE)
            }else if(type=="SpatialPoints"){  
              xs <- as.spatialPoints(x)
              sf::st_write(xs, fPath, delete_dsn = TRUE)
              # spp <- as.SpatialPoints(x)
              # rgdal::writeOGR(obj = spp, dsn = dirname(fPath), layer = basename(fPath), 
              #                 driver = driver, check_exists = TRUE, 
              #                 overwrite_layer = TRUE, delete_dsn = TRUE)
            }else if(type == "points"){
              stop("use type = SpatialPoints instead.\n")
            }else if(type == "lines"){
              stop("use type = SpatialLines instead.\n")
            }else if(type == "ASCII"){
              xCoord <- x@coord
              colnames(xCoord) <- c("x", "y", "z")
              # fPath <- paste0(fPath, ".txt")
              write.table(xCoord, fPath, row.names = FALSE, 
                          col.names = TRUE, quote = FALSE, ...)
            }
          })

#' @export
setMethod("exportCoord", "GPRsurvey",
          function(x, type = c("SpatialPoints", "SpatialLines", "ASCII"),
                   fPath = NULL, driver = "ESRI Shapefile", ...){
            type <- match.arg(type, c("SpatialPoints", "SpatialLines", "ASCII"))
            if(type=="SpatialLines"){  
              xs <- as.spatialLines(x)
              if(grepl("(.shp)$", fPath)) xs <- sf::st_zm(xs)
              sf::st_write(xs, fPath, delete_dsn = TRUE)
              # dfl <- data.frame(z=c(1), row.names = x@name)
              # spldf <- sp::SpatialLinesDataFrame(mySpatLines, dfl , 
              #                                    match.ID = TRUE)
              # rgdal::writeOGR(obj = spldf, dsn = dirname(fPath), layer = basename(fPath), 
              #                 driver = driver, check_exists = TRUE, 
              #                 overwrite_layer = TRUE, delete_dsn = TRUE)
            }else if(type=="SpatialPoints"){  
              xs <- as.spatialPoints(x)
              sf::st_write(xs, fPath, delete_dsn = TRUE)
              # spp <- as.SpatialPoints(x)
              # rgdal::writeOGR(obj = spp, dsn = dirname(fPath), layer = basename(fPath), 
              #                 driver = driver, check_exists = TRUE, 
              #                 overwrite_layer = TRUE, delete_dsn = TRUE)
            }else if(type == "ASCII"){
              mainDir <- dirname(fPath)
              if(mainDir =="." || mainDir =="/" ){
                mainDir <- ""
              }
              subDir <- basename(fPath)
              if ( !dir.exists( file.path(mainDir, subDir) )) {
                warning("Create new director ", subDir, " in ", mainDir, "\n")
                dir.create(file.path(mainDir, subDir))
              }
              for( i in seq_along(x)){
                gpr <- verboseF( x[[i]] , verbose = FALSE)
                fPath <- file.path(mainDir, subDir, gpr@name)
                exportCoord(gpr, fPath = fPath, type = "ASCII", ...)
              }
            }
          })
