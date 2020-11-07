#' RGPR: A package for processing and visualising ground-penetrating data 
#' radar (GPR) data.
#'
#' The RGPR package provides two classes GPR and GPRsurvey
#' @import stats
#' @import graphics
#' @import utils 
#' @import grDevices 
#' @import methods
#' @import rgdal
#' @import sf
"_PACKAGE"



.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("Don't hesitate to contact me if you ",
                               "have any question:\n",
                               "emanuel.huber@pm.me"))
}



# @references Several books!
# @section Reading/writing/export functions:
# \itemize{
#   \item \code{readGPR()}: format DT1 (Sensors&Software), rds (R-format)
#   \item \code{writeGPR()}: format DT1 (Sensors&Software), rds (R-format)
#   \item \code{exportPDF()}
#   \item \code{exportDelineations()}
#   \item \code{exportFid()}: ASCII-file
#   \item \code{exportCoord()}: ASCII, SpatialLines or SpatialPoints
#   \item \code{exportProc()}: ASCII-file
# }
# 
# @section Plot functions:
# \itemize{
#   \item \code{plot()}: raster or wiggles.
#   \item \code{plot3D()}:
#   \item \code{plotAmpl()}
#   \item \code{plotDelineations()}
# }
# 
# @section Coercion:
# \itemize{
#   \item \code{as.matrix()}:
#   \item \code{as.numeric()}:
#   \item \code{as.list()}:
#   \item \code{as.SpatialPoints()}:
#   \item \code{as.SpatialLines()}:
# }
# 
# @section Delineation:
# \itemize{
#   \item \code{delineate()}:
#   \item \code{plotDelineations()}:
#   \item \code{delineations()}: list of the delineations
#   \item \code{addDelineation}:
#   \item \code{rmDelineations}:
#   \item \code{exportDelineations}:
#   \item \code{plotDelineations3D}:
#   \item \code{identifyDelineation}:
# }