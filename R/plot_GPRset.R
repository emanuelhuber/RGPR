#' @method plot GPRset 
#' @name plot
#' @export
plot.GPRset <- function(x, 
                     col = NULL,
                     NAcol = "white",
                     clim = NULL,  # plot.GPRslice
                     add = FALSE, 
                     relTime0 = FALSE,
                     note = NULL, 
                     addMarkers = TRUE,
                     addAnn = TRUE,
                     addTime0 = TRUE,
                     addDepth0 = TRUE,
                     addAmpl0 = TRUE,
                     addTopo = FALSE,
                     xclip = NULL,
                     ratio = 1,
                     barscale = TRUE, 
                     wsize = 1,   # wiggles
                     wside = 1,   # wiggles
                     pdfName = NULL,
                     ...){
  plot(x[,,1],  
       col = col,
       NAcol = NAcol,
       clim = clim,  # plot.GPRslice
       add = add, 
       relTime0 = relTime0,
       note = note, 
       addMarkers = addMarkers,
       addAnn = addAnn,
       addTime0 = addTime0,
       addDepth0 = addDepth0,
       addAmpl0 = addAmpl0,
       addTopo = addTopo,
       xclip = xclip,
       ratio = ratio,
       barscale = barscale, 
       wsize = wsize,   # wiggles
       wside = wside,   # wiggles
       pdfName = pdfName,
       ...)

}