
inch2xRight <- function(v){
  # dx <- diff(par("usr")[1L:2])
  # dx * v / par()$pin[1] + par("usr")[2L]
  xinch(v) + par("usr")[2L]
  # xlim <- diff(range(x@x))
  # xlim * v / par()$pin[1] + xlim
}

inch2yTop <- function(v){
  yinch(v) + par("usr")[4L]
  # ylim <- diff(range(x@z))
  # ylim * v / par()$pin[2] + ylim
}

# clabpar: list possible values = top + arg for text (adj = NULL, pos = NULL, 
#                                       offset = 0.5, vfont = NULL, cex = 1, 
#                                       col = NULL, font = NULL, ...)
colorbar <- function(col, clim, zlim,
                     clab = "", 
                     clabpar = NULL, 
                     cbar = NULL, 
                     cb_left = 0.1, 
                     cb_width = 0.15, 
                     txt_left = 0.15){
  par(xpd=TRUE)
  rasterImage(col, 
              xleft = inch2xRight(cb_left), 
              xright = inch2xRight(cb_left + cb_width), 
              ytop = max(zlim),
              ybottom = min(zlim))
  rect(inch2xRight(cb_left), min(zlim), inch2xRight(cb_left + cb_width), max(zlim))
  # cval <- pretty(x@data, eps.correct = 0)
  cval <- pretty(clim, eps.correct = 0)
  # czlim <- range(x@data)
  # cval <- cval[cval >= czlim[1] & cval <= czlim[2]]
  cval <- cval[cval >= clim[1] & cval <= clim[2]]
  ylim <- range(zlim)
  # cpos <- (cval - czlim[1])  * diff(ylim)/diff(czlim)
  cpos <- (cval - clim[1])  * diff(ylim)/diff(clim)
  
  text(x = inch2xRight( cb_left + cb_width + txt_left), 
       y = cpos, 
       labels = cval , 
       adj = 0)
  segments(x0 = inch2xRight(cb_left + cb_width),
           y0 = cpos,
           x = inch2xRight(cb_left + cb_width + 0.5 * txt_left),
           y = cpos)
  
  # clab ==================
  if(clab != ""){
    clabx <- inch2xRight( cb_left + cb_width/2)
    claby <- inch2yTop(0.05)
    if(is.null(clabpar)){
      text(x = clabx,
           y = claby,
           labels = clab,
           pos = 3,
           adj = 0.5)
    }else{
      clabpar$x <- clabx
      if(is.null(clabpar$top)){
        clabpar$y <- claby
      }else{
        clabpar$y <- inch2yTop(clabpar$top)
        clabpar$top <- NULL
      }
      clabpar$pos <- ifelse(is.null(clabpar$pos), 3, clabpar$pos) 
      clabpar$labels <- clab
      do.call(text, clabpar)
    }
  }
  
  par(xpd=FALSE)
}

#' Faster plot
#' 
#' Function to plot faster GPR data
#' @param x [GPR]
#' @param col Color palette
#' @param colorbar [logical]
#' @param interpolate [logical]
#' @export
plotFast <- function(x, col = palGPR(), colorbar = TRUE, interpolate = TRUE){
  
  ## TODO
  ## give space on the right without using mar/mai!!!
  xlab <- "position (m)"
  ylab <- "depth (m)"
  xlim <- range(x@pos)
  ylim <- range(x@depth)
  cw_inch <- 4.1 # in
  cw_user <- diff(xlim) * cw_inch / par()$pin[1]
  
  
  cb_left = 0.1
  cb_width = 0.15
  txt_left = 0.15
  # par(mar = c(5.1, 4.1, 4.1, 4.1))
  # par()$mai
  op <- par(no.readonly=TRUE)
  if(isTRUE(colorbar) && par()$mar[4] < 5){
    par(mar = c(par()$mar[1:3], 5))
  }
  plt <- par()$plt
  
  plot(0, type = "n", xaxs = "i", yaxs = "i",
       xaxt = "n", yaxt = "n", xlim = xlim, ylim = ylim, 
       xlab = xlab, ylab = ylab, bty = "n",
       mgp = c(2, 0.5, 0))
  rasterImage(palCol(x@data, col = col), 
              xleft = min(x@pos), 
              xright = max(x@pos), 
              ytop = max(x@depth),
              ybottom = min(x@depth),
              interpolate = interpolate)
  
  title(x@name)
  mtext(x@filepath, side = 1, line = 3.5, adj = 0.5, cex = 0.65)
  grid()
  axis(1, tck = 0.01, mgp = c(2, 0.5, 0))
  axis(2, tck = 0.01, mgp = c(2, 0.5, 0))
  box()
  if(isTRUE(colorbar)){
    dim(col) <- c(length(col), 1)
    colorbar(col = col, clim = range(x), zlim = ylim, clab = "mV", clabpar = list(top = 0.05, adj = 0.5))
  }
}

