
# plot(x)
# 
# xwin <- window(x, xlim = c(10, 30), zlim = c(50, 200))
# plot(xwin)


#' Subset GPR data
#' 
#' Based on the units...
#' @param x (`GPR`)
#' @param xlim (`numeric[2]`) Data range along x-axis
#' @param ylim (`numeric[2]`) Data range along y-axis
#' @param zlim (`numeric[2]`) Data range along z-axis
#' @param ... Not used.
#' @return (`GPR`)
#' @method window GPRvirtual 
#' @name window
#' @export
window.GPRvirtual <- function(x, xlim = NULL, ylim = NULL, zlim = NULL, ...){
  if(is.null(xlim)) xlim <- range(x@x, na.rm = FALSE)
  if(is.null(ylim)) xlim <- range(x@y, na.rm = FALSE)
  if(is.null(zlim)) xlim <- range(x@z, na.rm = FALSE)
  x <- x[x@z >= zlim[1] & x@z <= zlim[2], x@x >= xlim[1] & x@x <= xlim[2]]
  return(x)
}

#' @method window GPR 
#' @name window
#' @export
window.GPR <- function(x, xlim = NULL, zlim = NULL, ...){
  if(is.null(xlim)) xlim <- range(x@x, na.rm = FALSE)
  if(is.null(zlim)) zlim <- range(x@z, na.rm = FALSE)
  x <- x[x@z >= zlim[1] & x@z <= zlim[2], x@x >= xlim[1] & x@x <= xlim[2]]
  return(x)
}
