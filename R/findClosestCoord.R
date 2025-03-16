#' Closest trace
#' 
#' Return the indice of the closest trace to the point `y`
#' @param x Object of the class GPR.
#' @param y Length-two numeric vector of the (x, y)-coordinates.
#' @return Indice (integer) of the closest trace.
#' @export
#' @concept spatial computing
findClosestCoord <- function(x, y){
  ymat <- matrix(as.numeric(y[1:2]), 
                 nrow = length(x),
                 ncol = 2,
                 byrow = TRUE)
  which.min(rowSums((ymat - x@coord[,1:2])^2))
  
  # FIXME:
#   use
#   # Use findInterval for multiple values
#   idx_x <- findInterval(x, d)
#   
#   # Adjust indices where the next value in d is closer
#   idx_x <- ifelse(
#     idx_x < length(d) & abs(d[idx_x + 1] - x) < abs(d[idx_x] - x),
#     idx_x + 1,
#     idx_x
#   )
}
