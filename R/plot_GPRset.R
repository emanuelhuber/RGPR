#' @name plot
#' @method plot GPRset
#' @export
plot.GPRset <- function(x, ...){
  plot(x[,,1],  ...)

}
