# FIXME DOCUMENTATION in plot_GPR.R
#' @method plot GPRcube 
#' @name plot
#' @export
plot.GPRcube <- function(x, ...){
  plot(x[,,1])
}