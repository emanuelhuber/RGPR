
#FIXME: ONE SINGLE HELP FILE
# in details explanation about
# - '...'
# - 


#' Plot the GPR object.
#'
#' \code{plot}: If the GPR object consists of a single trace, wiggle plot 
#' is shown. For CMP, the position of the traces on the x-axis is defined
#' by the antenna separation (\code{antsep(x)}).
#' 
#' Additional arguments
#' \itemize{
#'   \item \code{type}:
#'   \itemize{
#'      \item 1D plot: \code{p}, \code{l}, 
#'             \code{b}, \code{c}, \code{o}, \code{h}, \code{s}, \code{S}, 
#'             \code{n} (see argument \code{type} in
#'             \code{\link[graphics]{plot}}).
#'      \item  2D plot: \code{"raster"} (default), \code{"wiggles"} or
#'             \code{contour}.
#'   }
#'   \item \code{col}: Color. Default 1D: "black". 2D: \code{palGPR(n = 101)}
#' }
#' @param x Object of class \code{GPR}
#' @param add [\code{logical(1)}] If \code{TRUE}, add to current plot.
#' @param NAcol color of NA values
#' @param note note of something...
#' @param clim [\code{numeric(2)}] Value range for the color palette.
#' @param relTime0 logical. If \code{TRUE}, adjust vertical axis to time-zero.
#'                 If time-zero varies from trace to trace, the vertical axis
#'                 is adjusted to the mean time-zero. Apply first the function
#'                 \code{time0Cor()} to shift the traces to their time-zero.
#' @param addMarkers logical. Add fiducial marks
#' @param addAnn logical. Add GPR annotations (line intersections)
#' @param addTime0 logical. Add time-zero line
#' @param addDepth0 logical. Add depth-zero line
#' @param addAmpl0 logical. Add an horizontal line on 1D plot
#' @param addTopo logical. For 2D plot, add topography (if the data are sampled
#'                         in time unit, the data are migrated with a static
#'                         migration)
#' @param xclip numeric. If length-one numeric vector, clip the amplitudes 
#'                      larger than \code{xclip} and smaller than \code{-xclip}.
#'                      If length-two numeric vector, xclip the amplitudes
#'                      smaller than \code{xclip[1]} and larger than 
#'                      \code{xclip[2]}. Per default, values below the 
#'                      0.01-quantile and above the 0.99-quantile are clipped.
#'                      If \code{xclip = FALSE} the data are not clipped.
#' @param ratio logical.
#' @param barscale logical. Add a colorbar scale
#' @param wsize length-one numeric. Size of the wiggles (default = \code{1}).
#' @param wside length-one numeric. If positive the right part of the wavelet
#'              is colored. If negative, the left part of the wavelet is
#'              colored.
#' @param pdfName length-one character. Name/path of the PDF to export 
#'                without extension
#' @param col [\code{character}] FIXME Color palette.
#' @param xlab [\code{character(1)}] A title for the x axis.
#' @param ylab [\code{character(1)}] A title for the y axis.
#' @param asp [\code{numeric(1)}] The y/x aspect ratio.
#' @param main [\code{character(1)}] A main title for the plot,.
#' @param ... additional arguments passed to the plotting methods 
#'            \code{\link[graphics]{plot}} for 1D plot and 
#'            \code{\link[plot3D]{Image}} for 2D plot. See also  \code{details}.
#' @examples 
#' \dontrun{
#' ## PLOT GPRsurvey data (trace positions of several GPR data)
#' plot(x, parLines = NULL, parMarkers = NULL, parArrows = NULL, parIntersect = NULL)
#' # plot only arrows
#' plot(x, parLines = NULL, parMarkers = NULL, parIntersect = NULL)  
#' # plot only lines
#' plot(x, parMarkers = NULL, parArrows = NULL, parIntersect = NULL) 
#' # plot only markers
#' plot(x, parLines = NULL, parArrows = NULL, parIntersect = NULL)   
#' # plot only intersections
#' plot(x, parLines = NULL, parArrows = NULL, parMarkers = NULL)   
#' # plot only markers with other parameters
#' plot(x, parLines = NULL, parArrows = NULL, parMarkers = list(pch = 3), parIntersect = NULL)   
#' plot(x, parMarkers = NULL)
#' }
#' @method plot GPR 
#' @name plot
#' @export
#Default 1D: "black". 2D: \code{palGPR(n = 101)}
# ##' @param y \code{NULL,} not used
# options: type=c(raster,wiggles), addTopo, clip, normalize
plot.GPR <- function(x, 
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
  # print(list(...))
  if(length(x@vel) > 0){  
    # FIXME: 
    #   - v <- getVel()
    #   - v <- mean(v)
    v <- x@vel[[1]]  
  }else{
    v <- 0
  }
  dots <- list(...)
  type <- dots[["type"]]
  dots[["type"]] <- NULL
  #------------------------ trace plot (1D) -----------------------------------#
  # if(any(dim(x) == 1)){
  # FIXME: plot1D GPR on multiplot, e.g.: 
  # par(mfrow = c(1, 2))
  # plot(x[,1])
  # plot(x[,2])
  if(ncol(x) == 1){
    # print("1D")
    x[is.infinite(x) | is.na(x)] <- 0
    if(is.null(type)) type <- "l"
    if(isTRUE(add)){
      lines(x, ...)
    }else{
      .plotGPR1D(x, type, add, col, dots, v, relTime0, addDepth0, addAmpl0, addTime0)
    }
  }else if(nrow(x) == 1){
    x[is.infinite(x) | is.na(x)] <- 0
    par(mar = c(5, 4, 2, 2) + 0.1, oma = c(0, 0, 0, 0), mgp = c(2, 0.5, 0))
    if(is.null(col)) col <- "black"
    
    if(is.null(dots$xlab)) dots$xlab <- .xlab(x)
    if(is.null(dots$type)) dots$type <- "l"
    if(is.null(dots$col))  dots$col <- col
    if(is.null(dots$ylab)) dots$ylab <- .dlab(x)
    dotsxaxt <- dots$xaxt 
    if(is.null(dots$xaxt)) dots$xaxt <- "n"
    if(is.null(dots$xaxs)) dots$xaxs <- "i"
    if(is.null(dots$main)){
      myMain <- paste0(x@name, ": ", x@dlab, " (", x@dunit, ")", 
                       " @ ", round(x@z, 2), " ", x@zunit)
    }else{
      myMain <- dots$main
      dots$main <- NULL
    } 
    if(!is.null(dots[["log"]]) && dots[["log"]] == "y"){
      dots[["log"]] <- ""
      x@data <- log(x@data)
    }
    if(isTRUE(add)){
      do.call(lines, c(list(x = x@x, y = x@data), dots))
    }else{
      do.call(plot, c(list(x = x@x, y = x@data), dots))
      title(myMain, outer = FALSE)
      axis(side = 1,  tck = +0.02)
      axis(side = 3,  tck = +0.02, labels = FALSE)
      axis(4)
    }
    
    # plot(x@x, x@data[], ..., type = "l")
  #------------------------ radargram plot (2D) -------------------------------#
  }else{
    if(is.null(type)) type <- "raster"
    type <- match.arg(type, c("raster", "wiggles", "contour"))
   if(grepl("[s]$", x@zunit) && addTopo)    x <- migration(x)
    
    if(isTRUE(relTime0)){
      x <- shiftToTime0(x, track = FALSE)
    }
    
    if(!isFALSE(xclip)){ # if xclip == FALSE, no clip at all
      if(is.null(xclip)){
        if(x@mode == "velSpec"){
          # print("before")
          xclip <- range(as.vector(x@data), finite = TRUE) #, na.rm = TRUE)
          # print("after")
        }else{
          x_vec <- as.vector(x@data)
          x_vec <- x_vec[is.finite(x_vec)]
          xclip <- c(quantile(x_vec, 0.99, na.rm = TRUE),
                     quantile(x_vec, 0.01, na.rm = TRUE))
        }
      }
      # print(xclip)
      x <- clip(x, xclip = xclip, track = FALSE)
    }
    if(addMarkers == FALSE)  x@markers <- character(length(x@markers))
    if(is.null(note))    note <- x@path
    
    # time_0 <- x@z0    
    t0 <- median(x@z0)
    z <- t( x@data) 
    
    xvalues <- x@x
    yvalues <- x@z

    if(is.null(dots$xlab)) dots$xlab <- .xlab(x)
    if(is.null(dots$ylab)) dots$ylab <- .zlab(x)
    # if(is.null(dots$xlim)) dots$xlim <- range(xvalues)
    if(is.null(dots[["xlim"]])){
      xlim1 <- (xvalues[2] - xvalues[1])/2
      xlim2 <- (xvalues[length(xvalues)] - xvalues[length(xvalues) - 1])/2
      dots[["xlim"]] <- range(xvalues) + c(-xlim1, xlim2)
    }
    # if(is.null(dots$ylim)) dots$ylim <- rev(range(yvalues))
    if(is.null(dots[["ylim"]])){
      ylim1 <- (yvalues[2] - yvalues[1])/2
      ylim2 <- (yvalues[length(yvalues)] - yvalues[length(yvalues) - 1])/2
      dots[["ylim"]] <- range(yvalues) + c(-ylim1, ylim2)
    } 
    
    if(dots$ylim[1] < dots$ylim[2]) dots$ylim <- rev(dots$ylim)
    if(!is.null(dots$main)){
      mymain <- dots$main
      dots$main <- NULL
    }else{
      mymain <- x@name
    }
    # if(is.null(dots$type)) dots$type <- "raster"
    # dots$type <- match.arg(dots$type, c("raster", "wiggles", "contour"))
    
    op <- par(no.readonly=TRUE)
    
    colkeyDist <- 0.05
    if(barscale == FALSE){
      mai <- c(1.1, 1.02, 1.02, 1.02)
    }else{
      mai <- c(1.1, 1.02, 1.02, 1.4)
      if(grepl("[s]$", x@zunit) && toupper(x@mode) == "CO"){
        mai <- c(1.1, 1.02, 1.02, 2)
        colkeyDist <- 0.05  # 0.09
      }
    }
    
    mai <- mai + c(0, 0, 0, 0)
    mgp <- c(2.5, 0.75, 0)
    fac <- 0.2
    omi <- par()$omi
    
    if(!is.null(pdfName)){
      # if the zunit are "meters"
      xlim <- dots$xlim
      ylim <- dots$ylim
      if(grepl("[m]$", x@zunit)){
        heightPDF <- fac * abs(diff(ylim)) + sum(omi[c(1,3)] + mai[c(1,3)])
        widthPDF <- fac * abs(diff(xlim)) * ratio +  
          sum(omi[c(2,4)] + mai[c(2,4)])
      }else{
        heightPDF <- fac * abs(diff(ylim)) * v/2 + 
          sum(omi[c(1,3)] + mai[c(1,3)])
        widthPDF <- fac * abs(diff(xlim)) * ratio + 
          sum(omi[c(2,4)] + mai[c(2,4)])
      }
      Cairo::CairoPDF(file = paste0(pdfName, ".pdf"),
                      width = widthPDF,
                      height = heightPDF,
                      bg = "white",
                      pointsize=10,
                      title = pdfName)
    }
    #------------------------------ RASTER ------------------------------------#
    if(type %in% c("raster", "contour")){
      dots$NAcol <- NAcol
      if(is.null(dots$clab))        dots$clab <- .dlab(x)
      if(is.null(dots$xaxs))        dots$xaxs <- "i"
      if(is.null(dots$yaxs))        dots$yaxs <- "i"
      if(is.null(dots$yaxt))        dots$yaxt <- "n"
      if(is.null(dots[["colkey"]])) dots[["colkey"]] <- FALSE
      
      
      if(!is.null(dots$rasterImage) && isTRUE(rasterImage)){
        test1 <- abs(diff(range(diff(xvalues)))) > sqrt(.Machine$double.eps)
        test2 <- abs(diff(range(diff(yvalues)))) > sqrt(.Machine$double.eps)
        if(test1 && test2){ # all not equal
          dots$rasterImage <- FALSE
        }
      }
      
      #--- value range for the color palette
      if(is.null(clim)){
        if(!all(is.na(z)) ){
          # if(diff(range(z, na.rm = TRUE)) == 0){
          if(diff(range(z, finite = TRUE)) == 0){
            clim <- rep(0, 2)
            z[!is.na(z)] <- 0
          }else if( min(z, na.rm = TRUE) >= 0 ){
            # to plot amplitudes for example...
            # clim <- range(z, na.rm = TRUE)
            clim <- range(z, finite = TRUE)
            # if I use 'dots$col', R returns 'dots$colkey'!!!!
            if(is.null(col) && diff(range(z, na.rm = TRUE)) != 0){
              col <- palGPR("slice", n = 101)
            } 
          }else if( tolower(x@mode) %in% c("cmp", "co")){
            clim <- c(-1, 1) * max(abs(z), na.rm = TRUE)
          }else{
            clim <- range(z[is.finite(z)], na.rm = TRUE)
          }
        }else{
          clim <- rep(0, 2)
        }
      }
      dots$clim <- clim
      
      # if I use 'dots$col', R returns 'dots$colkey'!!!!
      if(is.null(col)) col <- palGPR(n = 101)
      
      
      if(type == "contour"){
        if(is.null(col))      col <- "black"
        if(length(col) == 1)  barscale <- FALSE
        if(add == TRUE){
          addDepth0 <- FALSE
          addAmpl0 <- FALSE
          addTime0 <- FALSE
          addMarkers <- FALSE
          addAnn <- FALSE
          note <- ""
        }
      }
      if(add == TRUE){ 
        dots$add <- TRUE
        barscale <- FALSE
        dots[["colkey"]] <- FALSE
        dots[["main"]] <- ""
        addDepth0 <- FALSE
        addAmpl0 <- FALSE
        addTime0 <- FALSE
        addMarkers <- FALSE
        addAnn <- FALSE
        mymain <- ""
        note <- ""
        dots[["ann"]] <- FALSE
        dots[["axes"]] <- FALSE
      }else{
        par( mai = mai)
      }
      
      dots[["col"]] <- col
      
      
      if(type == "contour"){
        # dots$type <- NULL
        do.call(plot3D::contour2D, c(list(x = xvalues, y = yvalues, z = z), 
                                     dots))
      }else if(type == "raster"){
        # dots$type <- NULL
        do.call(plot3D::image2D, c(list(x = xvalues, y = yvalues, z = z), dots))
      }
      #------------------------------ WIGGLES -----------------------------------#
    }else if(type == "wiggles"){
      
      # dots$type <- NULL
      barscale <- FALSE
      
      op <- par(no.readonly = TRUE) 
      dx <- mean(diff(xvalues)) # estimated x-step
      z <- x@data
      z[is.na(z) | is.infinite(z)] <- 0
      z <- z/max(abs(z)) * dx
      nr <- nrow(z)
      nc <- ncol(z)
      y0 <- 0
      topo <- rep(0L,nc)
      
      dots$xlim <- dots$xlim + c(-1, 1) * dx
      # dots$xlim <- NULL
      
      # ylim <- dots$ylim
      # dots$ylim <- NULL
      
      if(is.null(dots$side)) side <- 1
      yaxt <- "s"
      bty <- "o"
      
      # col and lwd have no influence on plot
      # col <- dots$col
      if(is.null(col)) col <- "black"
      lwd  <- dots$lwd
      if(is.null(dots$lwd)) lwd <- 0.5
      
      par(mai = mai)
      
      do.call(plot, c( list( x = 0, type = "n", xaxs = "i", yaxs = "i", 
                             yaxt = "n", bty = "n"), dots))
      
      if(wside > 0){
        for(i in rev(seq_along(xvalues))){
          y2   <- yvalues + topo[i]
          wig  <- cbind(wsize * z[,i] + xvalues[i], y2)
          wig1 <- rbind(c(xvalues[i], y2[1]), wig, c(xvalues[i], tail(y2,1) ))
          polygon(wig1, col = col, border = NA)
          rect(min(wig1[,1]), dots$ylim[1], xvalues[i], dots$ylim[2], 
               col = "white", border = NA)
        }
      }else{
        for(i in (seq_along(xvalues))){
          y2   <- yvalues + topo[i]
          wig  <- cbind(wsize * z[,i] + xvalues[i], y2)
          wig1 <- rbind(c(xvalues[i], y2[1]), wig, c(xvalues[i], tail(y2,1) ))
          polygon(wig1, col = col, border = NA)
          rect(max(wig1[,1]), dots$ylim[1], xvalues[i], dots$ylim[2], 
               col = "white", border = NA)
        }
      }
      for(i in (seq_along(xvalues))){
        y2 <- yvalues + topo[i]
        lines(xvalues[i] + wsize * z[, i], y2, lwd = lwd, col = col)  
      }
      box(bty = bty)
    }
    #--------------------------------------------------------------------------#
    if(is.null(dots$ann) || dots$ann != FALSE){
      yat <- axis(side = 2)
      if(grepl("[m]$", x@zunit) || !grepl("CO", toupper(x@mode)) || anyNA(x@antsep)){
        axis(side = 4)
      }else if(grepl("[s]$", x@zunit)){
        .depthAxis(x, yvalues, v, t0, yat, side = 4 )
        if(isTRUE(addDepth0)){
          depth_0 <- t0 + depth0(0, v, antsep = x@antsep)
          abline(h = depth_0, col = "grey", lty = 3)
          axis(side = 4, at = depth_0, labels = "0", tick = FALSE)
        }
      }
    }
    
    if(isTRUE(addTime0) && !grepl("CMPANALYSIS", toupper(x@mode))){
      dx <- diff(xvalues)/2
      xt0 <- c(xvalues[1] - dx[1],  xvalues + c(dx,  tail(dx, 1)))
      lines(xt0, c(x@z0, tail(x@z0, 1)), type = "s", col = "chartreuse",
            lwd = 2)
    }
    
    # plot note
    if(!is.null(note) && length(note) > 0){
      mtext(note, side = 1, line = 4, cex=0.6)
    }
    
    # plot fiducial markers
    test <- ( xvalues >= dots$xlim[1] & xvalues <= dots$xlim[2] )
    if(isTRUE(addMarkers) && !is.null(x@markers) && length(x@markers) > 0){
      .plotFid(x@markers[test], xvalues[test])
    }
    
    # plot annotations
    testAnn <- FALSE
    if(isTRUE(addAnn) && !is.null(x@ann) && length(x@ann) > 0){
      testAnn <- .plotAnn(x@ann[test], xvalues[test])
    }
    
    # plot title
    if(isTRUE(addAnn) && isTRUE(testAnn)){
      title(mymain, outer = TRUE, line = 1)
    }else{
      title(mymain)  
    }
    
    if(barscale){
      if(is.null(clim)){
        clim <- c(-0.1, 0.1)
      }else if(clim[1] == clim[2]){
        clim <- clim + c(-0.1, 0.1)
      } 
      fields::image.plot(zlim = clim, 
                         legend.only = TRUE, 
                         col = col, 
                         legend.shrink = 1)
    }
    if(!is.null(pdfName)){
      dev.off()
    }
  }
}


#' Add a GPR trace on a plot
#' @param x [\code{GPR class}]
#' @param relTime0 [\code{logical(1)}] If \code{TRUE}, shift \code{x} to time-
#'                                      zero. 
#' @param ... Additional parameters to be passed to \code{\link{lines}}.
#' @name lines
#' @export
lines.GPR <- function(x, relTime0 = FALSE, ...){
  if(length(x@vel) > 0){ 
    # FIXME: 
    #   - v <- getVel()
    #   - v <- mean(v)
    v <- x@vel[[1]]
  }else{
    v <- 0
  }
  if(any(dim(x) == 1)){
    z <- x@z
    t0 <- x@z0
    if(isTRUE(relTime0)){
      z <- x@z - x@z0
      t0 <- 0
    }
    # z <- seq(-x@z0, by = x@dz, length.out = length(x@data))
    #z <- x@z - x@z0
    dots <- list(...)
    if(!is.null(dots[["log"]]) && dots[["log"]] == "y"){
      dots[["log"]] <- NULL
      x@data <- log(x@data)
    }
    dots[["x"]] <- z
    dots[["y"]] <- x@data
    invisible( do.call(lines, dots) )
    #lines(z, x@data,...)
  }else{
    stop("x must a vector!")
  }
}

#' Add a GPR trace points on a plot
#'
#' @param x [\code{GPR class}]
#' @param relTime0 [\code{logical(1)}] If \code{TRUE}, shift \code{x} to time-
#'                                      zero. 
#' @param ... Additional parameters to be passed to \code{\link{points}}.
#' @name points
#' @export
points.GPR <- function(x, relTime0 = FALSE, ...){
  if(length(x@vel) > 0){  
    # FIXME: 
    #   - v <- getVel()
    #   - v <- mean(v)
    v <- x@vel[[1]]  
  }else{
    v <- 0
  }
  if(any(dim(x) == 1)){
    z <- x@z
    t0 <- x@z0
    if(isTRUE(relTime0)){
      z <- x@z - x@z0
      t0 <- 0
    }
    # z <- seq(-x@z0, by = x@dz, length.out = length(x@data))
    #z <- x@z - x@z0
    dots <- list(...)
    if(!is.null(dots[["log"]]) && dots[["log"]] == "y"){
      dots[["log"]] <- NULL
      x@data <- log(x@data)
    }
    dots[["x"]] <- z
    dots[["y"]] <- x@data
    invisible( do.call(points, dots) )
    #lines(z, x@data,...)
  }else{
    stop("x must a vector!")
  }
}



#' \code{contour} extends \code{plot3D::contour2D} and creates a contour plot.
#' @method contour GPR 
#' @name contour
#' @rdname plot
#' @export
# options: type=c(raster,wiggles), addTopo, clip, normalize
contour.GPR <- function(x, 
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
  
  plot.GPR(x, 
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
           pdfName = pdfName,
           type = "contour",
           ...)
}




.plotGPR1D <- function(x, type, add, col, dots, v, relTime0, addDepth0, addAmpl0, addTime0){
  # par(mar = c(5, 4, 3, 2) + 0.1)
  par(mai = c(1.1, 1.02, 1.02, 1.02))
  z <- x@z
  t0 <- x@z0
  if(isTRUE(relTime0)){
    z <- x@z - x@z0
    t0 <- 0
  }
  if(is.null(col)) col <- "black"
  
  if(is.null(dots$xlab)) dots$xlab <- .zlab(x)
  if(is.null(dots$type)) dots$type <- type
  if(is.null(dots$col))  dots$col <- col
  if(is.null(dots$ylab)) dots$ylab <- .dlab(x)
  dotsxaxt <- dots$xaxt 
  if(is.null(dots$xaxt)) dots$xaxt <- "n"
  if(is.null(dots$xaxs)) dots$xaxs <- "i"
  if(is.null(dots$main)){
    myMain <- paste0(x@name, ": trace #", colnames(x@data)," @ ", round(x@x, 2), 
                     " ", x@xunit)
  }else{
    myMain <- dots$main
    dots$main <- NULL
  } 
  if(!is.null(dots[["log"]]) && dots[["log"]] == "y"){
    dots[["log"]] <- ""
    x@data <- log(x@data)
  }
  if(isTRUE(add)){
    do.call(lines, c(list(x = z, y = x@data), dots))
  }else{
    do.call(plot, c(list(x = z, y = x@data), dots))
  }
  
  if(is.null(dots$ann) || dots$ann != FALSE){
    if(is.null(dotsxaxt) || dotsxaxt != "n"){
      x_axis <- pretty(z, 10)
      xat <- axis(side = 1,  tck = +0.02)
      if(grepl("[m]$", x@zunit) || !grepl("CO", toupper(x@mode)) || anyNA(x@antsep) ){
        axis(side = 3, tck = +0.02)
      }else if(grepl("[s]$", x@zunit)){
        .depthAxis(x, z, v, t0, xat, side = 3 )
        if(isTRUE(addDepth0)){
          depth_0 <- t0 + depth0(0, v, antsep = x@antsep)
          abline(v = depth_0, col = "grey", lty = 3)
          
          axis(side = 4, at = depth_0, labels = "0", tick = FALSE) # necessary?
        }
      }
    }
  }
  title(myMain, outer = FALSE, line = 3)
  if(isTRUE(addAmpl0))  abline(h = 0, lty = 3, col = "grey")
  if(isTRUE(addTime0))  abline(v = t0, col = "red")
}



# we use the Sensors & Software method to plot the depth axis
# when the data are in time domain: because of the offset between
# transmitter and receiver, there is an offset between time zero and depth,
# the depth axes is squished.
.depthAxis <- function(x, z, v, t0, xat, side = 3 ){
  d <- seq(0.1, by = 0.1, 0.9)
  depthat2 <- depthToTime(d, 0, v, antsep = x@antsep)
  if(max(z)*v/2 > 1.3){
    depth <- pretty(xat * v / 2, 10)
    depthat <- depthToTime(depth, 0, v, antsep = x@antsep)
    axis(side = side, at = t0 + depthat, labels = depth, tck = +0.02)
    labels_0To1 <- FALSE
  }else{
    labels_0To1 <- d
  }
  axis(side = side, at = t0 + depthat2, labels = labels_0To1, tck = +0.01)
  mtext(paste0("depth (", x@xunit, "),  v = ", round(v, 4), " ", 
               x@xunit, "/", x@zunit), side = side, line = 2)
}



.plotAnn <- function(ann, x, line = 1.7){
  if(length(ann) > 0){
    testann <- (ann != "")
    if(sum(testann) > 0){
      posann <- x
      ann <- gsub("#", "\n", ann)
      abline(v = posann[testann], col = "red", lwd = 1)
      mtext(ann[testann], side = 3, line = line, at = posann[testann],
            col = "red", cex = 0.9)
    }
    return(TRUE)
  }else{
    return(FALSE)
  }
}

.plotFid <- function(fid, x){
  usr <- par()$usr
  pin <- par()$pin  # inch
  if(!is.null(fid) && length(fid)>0 && any(fid!="")){
    cin <- par()$cin[2]
    posfid <- x
    testfid <- (fid != "")
    yr <- diff(usr[3:4])/(pin[2])
    if(sum(testfid)>0){  
      par(xpd=TRUE)
      cst <- yr*cin
      points(posfid[testfid],cst/2*0.75+rep(usr[4],sum(testfid)),pch=25,
             col="red",bg="yellow",cex=1)
      text(posfid[testfid],cst+rep(usr[4],sum(testfid)),fid[testfid],cex=0.6)
      #,pos=3,offset =0)
      par(xpd=FALSE)
    }
  }
}
