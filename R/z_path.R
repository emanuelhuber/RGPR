
# previously posLine(x, last = FALSE)


#' Relative position on a path
#'
#' Relative position of each coordinates (knots) along a path (polyline).
#' @param xy [\code{matrix}] Each column corresponding to a coordinate
#' @return Relative position of the coordinates (cumulative distance).
#' @export
pathRelPos <- function(xy){
  xy <- as.matrix(xy)
  relDist <- cumsum(c(0, sqrt(apply(diff(xy)^2, 1, sum))))
  # if(last){
  # return(tail(all_dist, 1))
  # }else{
  return(as.numeric(relDist))
  # }
}


# previously posLine(x, last = TRUE)

#' Path length
#'
#' Path length
#' @param xy [\code{matrix}] Each column corresponding to a coordinate
#' @return Path length.
#' @export
pathLength <- function(xy){
  return(tail(pathRelPos(xy), 1))
}