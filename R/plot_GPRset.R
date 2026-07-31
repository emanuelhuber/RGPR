#' @name plot
#' @method plot GPRset
#' @export
plot.GPRset <- function(x, ...){
  message("I am plotting x[,,1].")
  plot(x[,,1],  ...)

}
