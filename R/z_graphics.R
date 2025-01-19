# from https://stackoverflow.com/questions/38439211/figure-labels-add-text-on-graphs-in-the-same-location-despite-figure-size
# example:
# par(xpd = NA)
# legend(x = line2user(line = 8, side = 2), y = line2user(line = 5.5, side = 3), lab,
#        horiz = FALSE, col=rev(cols),
#        fill=rev(cols),
#        bty = "n", ncol = 2
# )
# par(xpd = FALSE)

#' Get user coordinates from margin line location.
#'
#' Get user coordinates from margin line location.
#' @param line (`numeric[1]`) margin line location.
#' @param side (`integer[1]`) an integer specifying which side of the plot 
#' the axis is to be drawn on. The axis is placed as follows: 1=below, 2=left, 
#' 3=above and 4=right.
#' @return (`numeric[2]`) Coordinates in user coordinate system.
#' @export 
line2user <- function(line, side) {
  lh <- par('cin')[2] * par('cex') * par('lheight')
  x_off <- diff(grconvertX(c(0, lh), 'inches', 'npc'))
  y_off <- diff(grconvertY(c(0, lh), 'inches', 'npc'))
  switch(side,
         '1' = grconvertY(-line * y_off, 'npc', 'user'),
         '2' = grconvertX(-line * x_off, 'npc', 'user'),
         '3' = grconvertY(1 + line * y_off, 'npc', 'user'),
         '4' = grconvertX(1 + line * x_off, 'npc', 'user'),
         stop("Side must be 1, 2, 3, or 4", call.=FALSE))
}
