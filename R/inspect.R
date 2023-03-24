setGenericVerif("inspect", function(x, ...) 
  standardGeneric("inspect"))

#' Inspect GPR data plot
#'
#' Reads the position of the graphics cursor when the (first) mouse button is 
#' pressed and return position as well as amplitude.
#'
#' @name inspect
#' @rdname inspect
#' @export
setMethod("inspect", "GPR", function(x,  ...){
    xy <- do.call(cbind, locator(...))
    
    i <- sapply(xy[, 1], .closest, x@pos)
    j <- sapply(xy[, 2], .closest, x@depth)
    A <- x@data[cbind(j, i)]
    mat <- matrix( nrow = nrow(xy), ncol = 5)
    mat[, 1] <- j
    mat[, 2] <- i
    mat[, 3] <- xy[, 1]
    mat[, 4] <- xy[, 2]
    mat[, 5] <- A
    colnames(mat) <- c("col", "row", "x", "y", "ampl")
    return(mat)
  }
)

.closest <- function(a, x){
  dx <- x - a
  which.min(abs(dx))
}

