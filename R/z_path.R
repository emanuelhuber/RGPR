
# previously posLine(x, last = FALSE)


#' Relative position on a path
#'
#' Relative position of each coordinates (knots) along a path (polyline).
#' @param xy (`matrix`) Each column corresponding to a coordinate
#' @param lonlat (`logical[1]`) If `TRUE` computes geodistance. 
#'               Otherwise Euclidean distance.
#' @return Relative position of the coordinates (cumulative distance).
#' @export
pathRelPos <- function(xy, lonlat = FALSE){
  xy <- as.matrix(xy)
  if(isTRUE(lonlat)){
    dx <- verboseF(geodist::geodist(xy[,1:2], paired = FALSE, 
                                    sequential = TRUE, pad = FALSE, 
                                    measure = "geodesic"),
                   verbose = FALSE)
    return( c(0, cumsum(dx)) )
  }else{
    relDist <- cumsum(c(0, sqrt(apply(diff(xy)^2, 1, sum))))
    # if(last){
    # return(tail(all_dist, 1))
    # }else{
    return(as.numeric(relDist))
    # }
  }
}


# previously posLine(x, last = TRUE)

#' Path length
#'
#' Path length
#' @param xy (`matrix`) Each column corresponding to a coordinate
#' @param lonlat (`logical(1)`) If `TRUE` computes geodistance. 
#'               Otherwise Euclidean distance.
#' @return Path length.
#' @export
pathLength <- function(xy, lonlat = FALSE){
  return(tail(pathRelPos(xy, lonlat = lonlat), 1))
}
