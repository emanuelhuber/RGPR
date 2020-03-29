
#' Rotate coordinates of the GPR traces
#' 
#' Rotate coordinates of the GPR traces... 
#' @param x [\code{GPR|GPRsurvey}] 
#' @param alpha [\code{numeric(1)}] The rotation angle in radians. 
#'              If \code{alpha = NULL}, \code{alpha} is estimated from the 
#'              pairs of points in the local reference system (\code{ploc}) 
#'              and in the regional reference system (\code{preg}).
#' @param cloc [\code{numeric(2)}] the coordinate
#'             center of the local reference system
#' @param creg [\code{numeric(2|3)}] the coordinate
#'             center of the regional reference system. Setting 
#'             \code{creg = NULL} (default) is equivalent to apply a rotation
#'             of angle \code{alpha} and center \code{cloc}.
#' @param ploc [\code{numeric(2|3)}] the first two columns corresponding
#            to coordinates in the local reference system.
#' @param preg [\code{numeric(2|3)}]
#' @param FUN [\code{function}]
#' @name spGeoref
setGeneric( "spGeoref", function(x, alpha = NULL, cloc = NULL, creg = NULL,
           ploc = NULL, preg = NULL, FUN = mean) 
  standardGeneric("spGeoref"))

#' @rdname spGeoref
#' @export
setMethod("spGeoref", "spPRsurvey", 
          function(x, alpha = NULL, cloc = NULL, creg = NULL,
                   ploc = NULL, preg = NULL, FUN = mean) {
            if(is.null(cloc)){
              cloc <- .centroid(x)[1:2]
            }
            if(is.null(alpha) && is.null(ploc) && is.null(preg)){
              alpha <- spAngle(x)
            }
            # here I cannot write FUN = FUN because FUN is an argument of
            # lapply...
            xyz  <- lapply(x@coords, .georef, alpha = alpha, cloc = cloc,
                           creg = creg, ploc = ploc, preg = preg, FUN)
            coord(x) <- xyz
            # x@intersections <- list()
            # x <- coordref(x)
            return(x)
          })

.centroid <- function(x){
  pos <- do.call(rbind, x@coords)
  return(colMeans(pos))
}




#-- not exported!
# Georeferencing
#
# Perform on a set of x,y coordinates
# (1) a translation by \code{-cloc}, then
# (2) a rotation by \code{alpha} (radian), and (3)
# a translation by \code{creg}. If \code{creg}
# is \code{NULL}, then \code{creg} is set equal
# to \code{cloc}.
# @param x A matrix with the first two columns corresponding
#          to coordinates.
# @param alpha A length-one numeric vector corresponding to 
#              the rotation angle in radians. If \code{alpha = NULL},
#              \code{alpha} is estimated from the pairs of points in
#              the local reference system (\code{ploc}) and in the
#              regional reference system (\code{preg}).
# @param cloc A length-two numeric vector corresponding to the coordinate
#             center of the local reference system
# @param creg A length-two numeric vector corresponding to the coordinate
#             center of the regional reference system. Setting 
#             \code{creg = NULL} (default) is equivalent to apply a rotation
#             of angle \code{alpha} and center \code{cloc}.
# @param ploc A matrix with the first two columns corresponding
#            to coordinates in the local reference system.
# @param preg A matrix with the first two columns corresponding
#             to coordinates in the regional reference system.
# @param FUN If \code{alpha = NULL}, a function to estimate the rotation angle
#            from the angles computed for each pairs of coordinates of
#            \code{ploc}-\code{preg}.
.georef <- function(x, alpha = NULL, cloc = c(0,0), creg = NULL,
                    ploc = NULL, preg = NULL, FUN = mean){
  x0 <- as.matrix(unname(x[, 1:2, drop = FALSE]))
  if(is.null(alpha)){
    cloc <- as.double(cloc)
    creg <- as.double(creg)
    ploc <- as.matrix(ploc)
    preg <- as.matrix(preg)
    alphaloc <- atan2(ploc[,1] - cloc[1], ploc[,2] - cloc[2])
    alphareg <- atan2(preg[,1] - creg[1], preg[,2] - creg[2])
    alpha <- alphareg - alphaloc
    message(paste0("rotation angles: ", 
                   paste0(round(alpha,4), collapse = ", "), "."))
    alpha <- FUN(alpha)
  }
  ROT <- matrix(c( cos(alpha), sin(alpha),
                   -sin(alpha), cos(alpha)), nrow=2, ncol=2)
  TRL <-  matrix(as.double(cloc[1:2]), nrow = nrow(x0), 
                 ncol = 2, byrow = TRUE)
  if(is.null(creg)){
    TRL2 <- TRL
  }else{
    TRL2 <-  matrix(creg[1:2], nrow = nrow(x0), ncol = 2, byrow = TRUE)
  }
  x[,1:2] <- (x0 - TRL) %*% ROT + TRL2
  return(x)
}
