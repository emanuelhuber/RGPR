#' @method plot GPRset 
#' @name plot
#' @export
plot.GPRset <- function(x, ...){
  plot(x[,,1],  ...)

}