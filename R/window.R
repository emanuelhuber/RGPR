
# plot(obj)
# 
# xwin <- window(obj, xlim = c(10, 30), zlim = c(50, 200))
# plot(xwin)


#' Subset GPR data
#' 
#' Based on the units...
#' @param obj (`GPR`)
#' @param xlim (`numeric[2]`) Data range along x-axis
#' @param ylim (`numeric[2]`) Data range along y-axis
#' @param zlim (`numeric[2]`) Data range along z-axis
#' @param ... Not used.
#' @return (`GPR`)
#' @method window GPRvirtual 
#' @name window
#' @export
window.GPRvirtual <- function(obj, xlim = NULL, ylim = NULL, zlim = NULL, ...){
  if(is.null(xlim)) xlim <- range(obj@x, na.rm = FALSE)
  if(is.null(ylim)) xlim <- range(obj@y, na.rm = FALSE)
  if(is.null(zlim)) xlim <- range(obj@z, na.rm = FALSE)
  obj <- obj[obj@z >= zlim[1] & obj@z <= zlim[2], obj@x >= xlim[1] & obj@x <= xlim[2]]
  return(obj)
}

#' @method window GPR 
#' @name window
#' @export
window.GPR <- function(obj, xlim = NULL, zlim = NULL, ...){
  if(is.null(xlim)) xlim <- range(obj@x, na.rm = FALSE)
  if(is.null(zlim)) zlim <- range(obj@z, na.rm = FALSE)
  obj <- obj[obj@z >= zlim[1] & obj@z <= zlim[2], obj@x >= xlim[1] & obj@x <= xlim[2]]
  return(obj)
}
