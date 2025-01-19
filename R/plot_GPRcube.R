# FIXME DOCUMENTATION in plot_GPR.R

#' @name plot
#' @method plot GPRcube
#' @export
plot.GPRcube <- function(x, ...){
  plot(x[,,1])
}
