setGenericVerif("inspect", function(x, ...) 
  standardGeneric("inspect"))

#' Inspect GPR data plot
#'
#' Reads the position of the graphics cursor when the (first) mouse button is 
#' pressed and return position as well as amplitude.
#' @param x [\code{class GPR}]
#' @param x ... arguments for the \code{locator} function.
#' @name inspect
#' @rdname inspect
#' @export
setMethod("inspect", "GPR", function(x,  ...){
    xy <- do.call(cbind, locator(...))
    getAmplFromCoords(x, xy)
    # i <- sapply(xy[, 1], .closest, x@pos)
    # j <- sapply(xy[, 2], .closest, x@depth)
    # A <- x@data[cbind(j, i)]
    # mat <- matrix( nrow = nrow(xy), ncol = 5)
    # mat[, 1] <- j
    # mat[, 2] <- i
    # mat[, 3] <- xy[, 1]
    # mat[, 4] <- xy[, 2]
    # mat[, 5] <- A
    # colnames(mat) <- c("col", "row", "x", "y", "ampl")
    # return(mat)
  }
)

#' Get amplitude from GPR coordinates
#'
#' Return the amplitude of the points defined by plot coordinates
#' 
#' @param x [\code{class GPR}]
#' @param coords [\code{matrix(n,2)}] \code{n} rows containing the plot coordinates
#'               (x and two-way travel time or depth)
#' @name getAmplFromCoords
#' @rdname inspect
#' @export
getAmplFromCoords <- function(x, coords){
  i <- sapply(coords[, 1], .closest, x@pos)
  j <- sapply(coords[, 2], .closest, x@depth)
  A <- x@data[cbind(j, i)]
  mat <- matrix( nrow = nrow(coords), ncol = 5)
  mat[, 1] <- j
  mat[, 2] <- i
  mat[, 3] <- coords[, 1]
  mat[, 4] <- coords[, 2]
  mat[, 5] <- A
  colnames(mat) <- c("row", "col", "x", "y", "ampl")
  return(mat)
}

.closest <- function(a, x){
  dx <- x - a
  which.min(abs(dx))
}

