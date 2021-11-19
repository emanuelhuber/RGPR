
#' GG-plot of GPR data
#' 
#' Grammar of graphic (GG) plot of GPR data. 
#' @param x GPR object
#' @param col Color palette
#' @export

ggplotGPR <- function(x, col = palGPR()){
  Xm <- data.frame(x = rep(pos(x), nrow(x)),
                   y = rep(depth(x), each = ncol(x)),
                   value =  as.vector(t(as.matrix(x)[nrow(x):1, ])))
  ggplot2::ggplot(Xm) +
    ggplot2::geom_tile(aes(x = x, y = y, fill = value)) +
    ggplot2::scale_x_continuous("X", expand = c(0, 0)) +
    ggplot2::scale_y_continuous("Y", expand = c(0, 0)) +
    ggplot2::scale_fill_gradientn("Z", colours = col) 
}
