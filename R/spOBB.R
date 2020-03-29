#' Oriented bounding box
#' 
#' Returns the oriented bounding box of the trace position of the survey.
#' 
#' The algorithm you are looking for is known in polygon generalisation as 
#' "smallest surrounding rectangle".
#' Compute the convex hull of the cloud.
#' For each edge of the convex hull:
#' compute the edge orientation (with arctan),
#' rotate the convex hull using this orientation in order to compute easily 
#' the bounding rectangle area with min/max of x/y of the rotated convex hull,
#' Store the orientation corresponding to the minimum area found,
#' Return the rectangle corresponding to the minimum area found.
#' In 3D, the same applies, except:
#'   The convex hull will be a volume,
#'   The orientations tested will be the orientations (in 3D) of the convex hull faces.
#' @source  source "whuber" from stackexchange.com, 
#' https://gis.stackexchange.com/questions/22895/finding-minimum-area-rectangle-for-given-points/181883#181883
#' @param x      [\code{GPR class}] An object of the class \code{GPR}
#' @return [\code{matrix(5,2)}] The coordinates of the corners of the oriented 
#'          bounding box, whereby the last row is identical to the first row.
#'          FIXME!!
#' @name spOBB
setGeneric("spOBB", function(x) 
  standardGeneric("spOBB"))



#' @rdname spOBB   
#' @export
setMethod("spOBB", "GPR", function(x){
  if(length(x@coord) > 0){
    return(.OBB(x@coord[,1:2]))
  }else{
    stop("x has no coordinates.")
  }
})

#' @rdname spOBB   
#' @export
setMethod("spOBB", "GPRsurvey", function(x){
  if(length(x@coords) > 0){
    xyz <- x@coords
    # xyz <- Filter(Negate(is.null), xyz)
    xyz <- xyz[sapply(xyz, function(x) length(x)> 0)]
    p <- do.call(rbind, xyz)
    return(.OBB(p[,1:2]))
  }else{
    stop("x has no coordinates.")
  }
})

#' @rdname spOBB   
#' @export
setMethod("spOBB", "matrix", function(x){
  .OBB(x[,1:2])
})



# source "whuber" from stackexchange.com
# https://gis.stackexchange.com/questions/22895/finding-minimum-area-rectangle-for-given-points/181883#181883
# Oriented Bounding Box
.OBB <- function(p) {
  # Analyze the convex hull edges     
  a <- chull(p)                                   # Indexes of extremal points
  a <- c(a, a[1])                                 # Close the loop
  e <- p[a[-1],] - p[a[-length(a)], ]             # Edge directions
  norms <- sqrt(rowSums(e^2))                     # Edge lengths
  v <- e / norms                                  # Unit edge directions
  w <- cbind(-v[,2], v[,1])                       # Normal directions to the edges
  
  # Find the MBR
  vertices <- p[a, ]                              # Convex hull vertices
  x <- apply(vertices %*% t(v), 2, range)         # Extremes along edges
  y <- apply(vertices %*% t(w), 2, range)         # Extremes normal to edges
  areas <- (y[1,]-y[2,])*(x[1,]-x[2,])            # Areas
  k <- which.min(areas)                           # Index of the best edge (smallest area)
  
  # Form a rectangle from the extremes of the best edge
  cbind(x[c(1,2,2,1,1),k], y[c(1,1,2,2,1),k]) %*% rbind(v[k,], w[k,])
}