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
# offset = 0.5, vfont = NULL, cex = 1, 
#  col = NULL, font = NULL, ...)
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
  # cpos <- (cval - czlim[1]) * diff(ylim)/diff(czlim)
  cpos <- (cval - clim[1]) * diff(ylim)/diff(clim) + ylim[1]
  
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
#' @param x [\code{class GPR}]
#' @param col Color palette
#' @param colorbar [\code{color palette}]
#' @param interpolate [\code{logical(1)}]
#' @param xlab [\code{charachter(1)}]
#' @param sym [\code{logical(1)}] if \code{TRUE} the colorbar is symmetric
#' @param xlim [\code{numeric(2)}] the x limits (x1, x2) of the plot.
#' @param ylim [\code{numeric(2)}] the y limits (y1, y2) of the plot.
#' @export
plotFast <- function(x, col = palGPR(), colorbar = TRUE, interpolate = TRUE,
                     xlab = NULL, ylab = NULL, sym = TRUE, ylim = NULL, xlim = NULL){
  
  ## TODO
  ## give space on the right without using mar/mai!!!
  # xlab <- "position (m)"
  # ylab <- "depth (m)"
  if(is.null(ylab)){
    if(grepl("[m]$", x@depthunit)){
      ylab <- paste0("depth (",x@depthunit,")")
    }else if(grepl("[s]$", x@depthunit)){
      ylab <- paste0("two-way travel time (",x@depthunit,")")
    }
  }
  if(is.null(xlab)){
    xlab <- paste0("position (", x@depthunit, ")")
  }
  if(is.null(xlim)) xlim <- range(x@pos)
  if(is.null(ylim)) ylim <- range(x@depth)
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
       xaxt = "n", yaxt = "n", xlim = xlim, ylim = rev(ylim), 
       xlab = xlab, ylab = ylab, bty = "n",
       mgp = c(2, 0.5, 0))
  rasterImage(palCol(x@data, col = col, sym = sym), 
              xleft = min(x@pos), 
              xright = max(x@pos), 
              ytop = min(x@depth),
              ybottom = max(x@depth),
              interpolate = interpolate)
  
  title(x@name)
  if(length(x@filepath) > 0){
    mtext(x@filepath, side = 1, line = 3.5, adj = 0.5, cex = 0.65)
  }
  grid()
  axis(1, tck = 0.01, mgp = c(2, 0.5, 0))
  axis(2, tck = 0.01, mgp = c(2, 0.5, 0))
  box()
  if(isTRUE(colorbar)){
    if(isTRUE(sym)){
      clim <- max(abs(x), na.rm = TRUE) * c(-1, 1)
    }else{
      clim <- range(x, na.rm = TRUE)
    }
    dim(col) <- c(length(col), 1)
    colorbar(col = col, clim = clim, zlim = rev(ylim), clab = "mV", clabpar = list(top = 0.05, adj = 0.5))
  }
}
