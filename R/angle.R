
#' Angle of the GPR data
#'
#' The angle is computed based on the orientation of the oriented bounding
#' box (\code{\link{obbox}}).
#' @param x [\code{GPR|GPRsurvey}]
#' @return [\code{numeric(1)}] The angle of the oriented bounding box.
#' @name angle
#' @concept spatial computation
setGeneric("angle", function(x) 
  standardGeneric("angle"))



#' @rdname angle
#' @export
setMethod("angle", "GPR", function(x){
  if(length(x@coord) > 0){
    dEN <- x@coord[1,1:2] - tail(x@coord[,1:2],1)
    angl_EN <- atan2(dEN[2], dEN[1])
    # angl_EN/pi * 180
    orb <- sf::st_coordinates(obbox(x))
    dEN <- orb[1,] - orb[2,]
    i <- which.max(diff(pathRelPos(orb)))[1]
    dOBB <- orb[i + 1,] - orb[i,]
    angl_OBB <- atan2(dOBB[2], dOBB[1])
    # angl_OBB/pi * 180
    # abs(angl_EN - angl_OBB) / pi * 180
    if(pi * 6/5 > abs(angl_EN - angl_OBB) && abs(angl_EN - angl_OBB)  > pi* 4 /5){
      angl_OBB <- angl_OBB + pi
      if(angl_OBB > pi) angl_OBB <- angl_OBB - 2*pi
    }
    return(angl_OBB)
  }else{
    stop("x has no coordinates.")
  }
})



#' @rdname angle
#' @export
setMethod("angle", "GPRsurvey", function(x){
  if(length(x@coords) > 0){
    orb <- sf::st_coordinates(obbox(x))
    dEN <- orb[1,] - orb[2,]
    i <- which.max(diff(pathRelPos(orb)))[1]
    dOBB <- orb[i + 1,] - orb[i,]
    angl_OBB <- atan2(dOBB[2], dOBB[1])
    # # angl_OBB/pi * 180
    # # abs(angl_EN - angl_OBB) / pi * 180
    # if(pi * 6/5 > abs(angl_EN - angl_OBB) && abs(angl_EN - angl_OBB)  > pi* 4 /5){
    #   angl_OBB <- angl_OBB + pi
    #   if(angl_OBB > pi) angl_OBB <- angl_OBB - 2*pi
    # }
    return(angl_OBB)
  }else{
    stop("x has no coordinates.")
  }
})
