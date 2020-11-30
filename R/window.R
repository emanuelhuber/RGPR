
# plot(x)
# 
# xwin <- window(x, xlim = c(10, 30), zlim = c(50, 200))
# plot(xwin)


#' Subset GPR data
#' 
#' Based on the units...
#' @param x [\code{GPR}]
#' @param xlim [\code{numeric(2)}] Data range along x-axis
#' @param ylim [\code{numeric(2)}] Data range along y-axis
#' @param zlim [\code{numeric(2)}] Data range along z-axis
#' @param ... Not used.
#' @return [\code{GPR}]
#' @method window GPRvirtual 
#' @name window
#' @export
window.GPRvirtual <- function(x, xlim, ylim, zlim, ...){
  x[x@z >= zlim[1] & x@z <= zlim[2], x@x >= xlim[1] & x@x <= xlim[2]]
}

#' @method window GPR 
#' @name window
#' @export
window.GPR <- function(x, xlim, zlim, ...){
  x[x@z >= zlim[1] & x@z <= zlim[2], x@x >= xlim[1] & x@x <= xlim[2]]
}