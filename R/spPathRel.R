#' Relative positions along path
#' 
#' @param x [\code{GPR|GPRsurvey}] An object of the class \code{GPR} or
#'          \code{GPRsurvey}
#' @return [\code{numeric{m}}] Relative trace position along GPR line.
#' @name spPathRel
setGeneric("spPathRel", function(x) 
  standardGeneric("spPathRel"))



#' @rdname spPathRel   
#' @export
setMethod("spPathRel", "GPR", function(x){
  if(length(x@coord) > 0){
    # if(isCRSLonLat(x)){ 
    #   dx <- verboseF(geodist::geodist(x@coord[,1:2], paired = FALSE, 
    #                                   sequential = TRUE, pad = FALSE, 
    #                                   measure = "geodesic"),
    #                  verbose = FALSE)
    #   x@x <- c(0, cumsum(dx))
    # }else{
      return(pathRelPos(x@coord[,1:2], lonlat = isCRSLonLat(x)))
    # }
    # return(x)
  }else{
    stop("No coordinates")
    # return(numeric())
  }
})

# #' @rdname spPathRel   
# #' @export
# setMethod("spPathRel", "GPRsurvey", function(x){
#   if(length(x@coords) > 0){
#     xyz <- x@coords
#     # xyz <- Filter(Negate(is.null), xyz)
#     test <- sapply(xyz, function(x) length(x)> 0)
#     for(i in seq_along)
#     xyz <- xyz[]
#     p <- do.call(rbind, xyz)
#     x_obb <- .OBB(p[,1:2])
#     # return(.OBB(p[,1:2]))
#     return(matrix2polygon(x_obb))
#   }else{
#     stop("x has no coordinates.")
#   }
# })
