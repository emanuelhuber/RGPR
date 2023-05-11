#' Closest trace
#' 
#' Return the indice of the closest trace to the point \code{y}
#' @param x Object of the class GPR.
#' @param y Length-two numeric vector of the (x, y)-coordinates.
#' @return Indice (integer) of the closest trace.
#' @export
findClosestCoord <- function(x, y){
  ymat <- matrix(as.numeric(y[1:2]), 
                 nrow = length(x),
                 ncol = 2,
                 byrow = TRUE)
  which.min(rowSums((ymat - x@coord[,1:2])^2))
}
