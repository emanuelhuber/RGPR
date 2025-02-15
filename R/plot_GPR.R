
#FIXME: ONE SINGLE HELP FILE
# in details explanation about
# - '...'
# - 

#FIXME cbar = FALSE -> no colorscale -> remove space
#      cbar = NULL -> no colorscale but space is kept 

#' Plot GPR data
#'
#' Nice function to plot data. If the GPR object consists of a single trace, wiggle plot 
#' is shown. For CMP, the position of the traces on the x-axis is defined
#' by the antenna separation (`antsep(x)`).
#' 
#' The argument `col` is :
#' * 1D plot: a single color
#' * 2D plot: a color palette. Default = `palGPR()`
#' 
#' The argument `type` is :
#' * 1D plot: `p`, `l`, `b`, `c`, `o`, `h`, `s`, `S`, `n` 
#'   (see argument `type` in [graphics::plot()]).
#' * 2D plot: `"raster"` (default), `"wiggles"` or `"contour"`.
#' 
#' The argument `wiggles` is a list with following items:
#' * `side` Either `1` or `-1` defining on which side the 
#'          wiggles are drawn.
#' * `size` Size of the wiggles (default = 0.2).
#' * `col` Color of the wiggles.
#' * `lwd` Line thickness.
#'
#' The argument `markers` is a list with following items:
#' * `lineSymbols` Symbol position on margin lines (starting at 0,
#'            counting outwards).
#' * `pch` plotting ‘character’, i.e., symbol to use. See 
#'          [graphics::points()].
#' * `colSymbols` Symbol color
#' * `bgSymbols` Background color for the open plot symbols given by 
#'          `pch = 21:25`.
#' * `cexSymbols` Symbol expansion: a numerical value.
#' * `lineText` Text position on margin lines (starting at 0, counting outwards).
#' * `cexText` Character expansion: a numerical value.
#' * `colText` Text color.
#' 
#' The argument `ann` is a list with following items:
#' * `lineText` Text position on margin lines (starting at 0,
#'            counting outwards).
#' * `colLine` Line color.
#' * `colText` Text color.
#' * `cexText` Character expansion: a numerical value.
#' * `lwd` Line thickness.
#' 
#' The argument `z0` is a list with following items:
#' * `lwd` Line thickness.
#' * `col` Line color.
#' * `lty` Line type. 
#'  
#' The argument `cbar` is a list with following items:
#' * `w` colorbar width in lines (default = 1)
#' * `pos` left position of the colorbar in lines (default = 1)
#' * `hst` space between colorbar and text in lines (default = 0.5)
#' * `fticks` length factor for the ticks (default = 0.5)
#' * `vclab` length factor for the ticks (if `vclab = 1`, then
#'       the tick length is equal to the space between 
#'       the colorbar and the text.
#' * `clab` The label of the colorbar that is plotted above the 
#'       colorbar (default = NULL, the label is inferred from the 
#'       GPR data `x`)
#'
#' 
#' @param x (`class GPR`)
#' @param col Color palette. See details.
#' @param type (`character[1]`) Plot type. See details
#' @param horiz (`logical[1]`) Only for 1D plot. If `TRUE`, the 1D
#'              plot is horizontal (else vertical).
#' @param interpolate (`logical[1]`)
#' @param sym (`logical[1]`) if `TRUE` the colorscale is symmetric
#'             if the amplitudes vary around zero. FIXME
#' @param clim (`numeric[2]`) The range of the color values, used in the 
#'             color palette.
#' @param add (`logical[1]`) If `TRUE`, add to current plot.
#' @param asp (`numeric[1]`) The y/x aspect ratio.
#' @param secaxis (`logical[1]`) If `TRUE`, add a secondary axis. 
#'                If the GPR data was acquired in common-offset mode, 
#'                a secondary depth axis is added (based on the average wave
#'                velocity). Else a secondary time axis is added.
#'                Note that we use the Sensors and 
#'                Software method to plot the depth axis
#'                when the data are in time domain: because of the offset 
#'                between transmitter and receiver, there is an offset between 
#'                time-zero and depth, the depth axes is squished.
#' @param elev (`logical[1]`) If `TRUE`, plot signal as a function of
#'             elevation (datum).
#' @param export (`NULL|character[1]`) If `export` is a filename with
#'               png or pdf extension, the plot is exported in a png/pdf with
#'               filename equal to `export`.
#' @param fac (`numeric[1]`) Factor to set the size of the pdf 
#'            (aspect ratio is defined by `asp`).
#' @param wiggles (`list`) Parameter list for plotting wiggles. Only used
#'                when `type = "wiggles"`. See details below.
#' @param markers (`list|NULL`) If not `NULL`, display the fiducial 
#'                markers according to the parameter list. See details below.
#' @param ann (`list|NULL`) If not `NULL`, display the fiducial 
#'                markers according to the parameter list. See details below.            
#' @param z0 (`list|NULL`) If not `NULL`, display a line corresponding
#'           to time-zero. See details.
#' @param cbar (`list|NULL`)  If not `NULL`, display a colorbar.
#'             See details
#' @param dirArrows (`list|NULL`) If not `NULL`, display an arrow
#'                  indicating the survey direction (only for plot of GPRsurvey)
#'                  data.
#' @param ... additional arguments passed to the plotting methods 
#'            [graphics::plot()] for 1D plot and 
#'            [plot3D::image2D()] for 2D plot. See also  `details`.
#'
#' @method plot GPR
#' @name plot      
#' @rdname plot
#' @export
plot.GPR <- function(x,
                     col = NULL,
                     type = NULL,
                     horiz = TRUE,
                     interpolate = TRUE,
                     sym = TRUE,
                     clim = NULL,
                     add = FALSE,
                     asp = NA,
                     secaxis = TRUE,
                     elev = FALSE,
                     export = NULL,
                     fac = 1,
                     wiggles = list(side = 1, size = 0.2, col = "black", lwd = 0.5),
                     markers = list(lineSymbols = 0.35, pch = 25, 
                                    colSymbols = "red", 
                                    bgSymbols = "yellow", 
                                    cexSymbols = 1,
                                    lineText = 0.9, cexText = 0.6, colText = "red"),
                     ann = list(lineText = 1., colLine = "red", colText = "red", cexText = 0.75, lwd = 1),
                     z0 = list(lwd = 1, col = "green", lty = 1),
                     cbar = list(w = 1,
                                 pos = 1,
                                 hst = 0.5,
                                 fticks = 0.5,
                                 vclab = 0.5,
                                 clab = NULL),
                     ...){
  x[is.infinite(x) | is.na(x)] <- 0
  
  if(ncol(x) == 1){
    .plot1D(x, 
            add = add, 
            secaxis = secaxis,
            z0 = z0, 
            horiz = horiz,
            col = col,
            type = type, 
            ...)
    
  }else if(nrow(x) == 1){
    par(mar = c(5, 4, 2, 2) + 0.1, oma = c(0, 0, 0, 0), mgp = c(2, 0.5, 0))
    
    dots <- list(...)
    # if(is.null(col)) col <- "black"
    
    if(is.null(dots$xlab)) dots$xlab <- .xlab(x)
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
    # if(is.null(dots$type)) 
    dots$type <- ifelse(is.null(type), "l", type)
    dots$col <- ifelse(is.null(col), "black", col)
    # if(is.null(col)){
    #   dots$col <- "black"
    # }else{
    #   dots$col <- col
    # }
    if(isTRUE(add)){
      do.call(lines, c(list(x = x@x, y = x@data), dots))
    }else{
      do.call(plot, c(list(x = x@x, y = x@data), dots))
      title(myMain, outer = FALSE)
      grid()
      axis(side = 1,  tck = +0.02, mgp = c(1,0.5,0), lwd = -1, lwd.ticks = 1)
      axis(side = 3,  tck = +0.02, labels = FALSE, lwd = -1, lwd.ticks = 1)
      axis(side = 4, tck = +0.02, mgp = c(1,0.5,0), lwd = -1, lwd.ticks = 1)
    }
  }else{
    .plot2D(x,
            col = col,
            type = type,
            interpolate = interpolate,
            sym = sym,
            clim = clim,
            add = add,
            asp = asp,
            secaxis = secaxis,
            elev = elev,
            export = export,
            fac = fac,
            wiggles = wiggles,
            markers = markers,
            ann = ann,
            z0 = z0,
            cbar = cbar,
            ...)
  }
  
}


.plot1D <- function(x, 
                    type = "l", 
                    col = "black", 
                    interpolate, # 2D
                    add = FALSE, 
                    secaxis = TRUE,
                    z0 = list(lwd = 1, col = "green", lty = 1), 
                    horiz = TRUE,
                    ...){
  # par(mar = c(5, 4, 3, 2) + 0.1)
  par(mai = c(1.1, 1.02, 1.02, 1.02))
  z <- x@z
  t0 <- x@z0
  
  if(is.null(col)) col <- "black"
  if(is.null(type)) type <- "l"
  
  # defaults <- list(xaxt = "n",
  #                  yaxt = "n",
  #                  xlim = range(x@x),
  #                  ylim = range(x@z),
  #                  note = x@path
  # )
  
  # if(missing(col)) col <- "black"
  
  dots <- list(...)
  
  dots$type <- type
  dots$col <-  col 
  dotsxaxt <- dots$xaxt 
  dotsyaxt <- dots$yaxt 
  if(is.null(dots$xaxt)) dots$xaxt <- "n"
  if(is.null(dots$yaxt)) dots$yaxt <- "n"
  if(is.null(dots$main)){
    if(length(x@x) > 0){
      myMain <- paste0(x@name, " @ ", round(x@x, 4), " ", x@xunit)
    }else{
      myMain <- x@name
    }
  }else{
    myMain <- dots$main
    dots$main <- NULL
  } 
  if(!is.null(dots[["log"]]) && dots[["log"]] == "y"){
    dots[["log"]] <- ""
    x@data <- log(x@data)
  }
  if(isTRUE(horiz)){
    y = x@data
    if(is.null(dots$ylab)) dots$ylab <- .dlab(x)
    if(is.null(dots$xlab)) dots$xlab <- .zlab(x)
    if(is.null(dots$xaxs)) dots$xaxs <- "i"
    # axis_1 <- 1
    # axis_2 <- 2
    # axis_3 <- 3
  }else{
    if(is.null(dots$xaxs)) dots$yaxs <- "i"
    y = z
    z = x@data
    if(is.null(dots$xlab)) dots$xlab <- .dlab(x)
    if(is.null(dots$ylab)) dots$ylab <- .zlab(x)
    if(is.null(dots$ylim)) dots$ylim <- rev(range(y))
    # axis_1 <- 2
    # axis_2 <- 1
    # axis_3 <- 4
  }
  
  if(is.null(dots$mgp)) dots$mgp = c(2,0.5,0)
  
  if(isTRUE(add)){
    do.call(lines, c(list(x = z, y = y), dots))
  }else{
    do.call(plot, c(list(x = z, y = y), dots))
  }
  grid()
  
  
  if(is.null(dots$ann) || dots$ann != FALSE){
    if(is.null(dotsyaxt) || dotsyaxt != "n"){
      yat <- axis(side = 2,  tck = +0.01, mgp = c(1,0.5,0), lwd = -1, lwd.ticks = 1)
    }
    if(is.null(dotsxaxt) || dotsxaxt != "n"){
      x_axis <- pretty(z, 10)
      xat <- axis(side = 1,  tck = +0.01, mgp = c(1,0.5,0), lwd = -1, lwd.ticks = 1)
      
      if(isTRUE(secaxis)){
      #if(grepl("[m]$", x@zunit) || !isCommonOffset(x) || anyNA(x@antsep) ){
        if(isZDepth(x) || length(x@vel) == 0 ){
          axis(side = 3, tck = +0.01, mgp = c(1,0.5,0), lwd = -1, lwd.ticks = 1)
        }else if(isZTime(x)){
          vmean <- mean(.getVel(x, type = "vrms", strict = FALSE))
          if(isTRUE(horiz)){
            .depthAxis(x, z, vmean, t0, xat, side = 3 )
          }else{
            .depthAxis(x, y, vmean, t0, yat, side = 4 )
          }
            depth_0 <- t0 + depth0(0, vmean, antsep = x@antsep)
            if(isTRUE(horiz)){
              abline(v = depth_0, col = "grey", lty = 3)
            }else{
              abline(h = depth_0, col = "grey", lty = 3)
            }
            #axis(side = 4, at = depth_0, labels = "0", tick = FALSE) # necessary?
        }
      }
    }
  }
  title(myMain, outer = FALSE, line = 3)
  if(isTRUE(horiz)){
    if(isTRUE(secaxis))  abline(h = 0, lty = 3, col = "grey")
    if(!is.null(z0))  do.call(abline, c(list(v = t0), z0))
  }else{
    if(isTRUE(secaxis))  abline(v = 0, lty = 3, col = "grey")
    if(isTRUE(z0))  do.call(abline, c(list(h = t0), z0))
  }
}





# we use the Sensors & Software method to plot the depth axis
# when the data are in time domain: because of the offset between
# transmitter and receiver, there is an offset between time zero and depth,
# the depth axes is squished.
# FIXME: clean, use only x as argument.

.secaxis <- function(x, antsep, z, v, z0, xat, side = 3 ){
  if(length(antsep) > 1){
    warning("Antenna separation is not constant! I take the first value.")
    antsep = 1
  }
  d <- seq(0.1, by = 0.1, 0.9)
  depthat2 <- depthToTime(d, 0, v, antsep = antsep)
  if(max(z)*v/2 > 1.3){
    depth <- pretty( c(0, diff(xat)) * v / 2, n = 6)
    depth <- c(0, depth[depth > 0.5])
    depthat <- depthToTime(depth, 0, v, antsep = antsep)
    axis(side = side, at = z0 + depthat, labels = depth, tck = +0.01, mgp = c(2,0.5,0))
    labels_0To1 <- FALSE
  }else{
    labels_0To1 <- d
  }
  axis(side = side, at = z0 + depthat2, labels = labels_0To1, tck = +0.01)
  mtext(paste0("depth (", x@xunit, "),  v = ", round(v, 4), " ", 
               x@xunit, "/", x@zunit), side = side, line = 1.5)
}


.plot2D <- function(x, 
                    type = c("raster", "wiggles", "contour"),
                    col = NULL, 
                    interpolate = TRUE,
                    sym = TRUE,
                    clim = NULL,
                    add = FALSE,
                    asp = NA,
                    secaxis = TRUE,
                    elev = FALSE,
                    export = NULL,
                    fac = 1,
                    wiggles = list(side = 1, size = 0.2, col = "black", lwd = 0.5),
                    markers = list(lineSymbols = 0.35, pch = 25, 
                                   colSymbols = "red", 
                                   bgSymbols = "yellow", 
                                   cexSymbols = 1,
                                   lineText = 0.9, cexText = 0.6),
                    ann = list(lineText = 1.7, colLine = "red", colText = "red", cexText = 0.9, lwd = 1),
                    z0 = list(lwd = 1, col = "green", lty = 1),
                    cbar = list(w = 1,
                                pos = 1,
                                hst = 0.5,
                                fticks = 0.5,
                                vclab = 0.5,
                                clab = NULL),
                    ...){
  
  
  op <- par(no.readonly=TRUE)
  
  # # if(isTRUE(secaxis) && !isCommonOffset(x)){
  #   # message("'secaxis' set equal to 'FALSE': antenna separation not constant")
  #   secaxis = FALSE
  # } 
  
  if(isTRUE(elev) && isZDepth(x) && length(x@coord) > 0){
    zmax <- max(x@coord[, 3])
    x <- shift(x, z = -(zmax - x@coord[, 3]), crop = FALSE)
    x@z <- zmax - x@z
    x@zlab <- "elevation"
  }else{
    elev <- FALSE
  }
  
  defaults <- list(xlab = .xlab(x),
                   ylab = .zlab(x),
                   xlim = range(x@x),
                   ylim = range(x@z),
                   main = x@name,     #FIXME Not working when plot(x, main = "lkjl")
                   note = x@path,
                   mpg = c(2, 1, 0)  #c(2, 0.5, 0)
  )
  cbardefaults <- list(w = 1, pos = 1, hst = 0.5, fticks = 0.5, vclab = 0.5, clab = NULL)
  z0defaults <- list(lwd = 1, col = "green", lty = 1)
  mrkdefaults <- list(lineSymbols = 0.35, pch = 25, 
                      colSymbols = "red", 
                      bgSymbols = "yellow", 
                      cexSymbols = 1,
                      lineText = 0.9, cexText = 0.6)
  wigglesdefaults <- list(side = 1, size = 0.2, col = "black", lwd = 0.5)
  
  cbar <- setDefaultListValues(cbar, cbardefaults)
  markers <- setDefaultListValues(markers, mrkdefaults)
  wiggles <- setDefaultListValues(wiggles, wigglesdefaults)
  z0 <- setDefaultListValues(z0, z0defaults)
  
  dots <- list(...)
  if(length(dots) > 0)   defaults <- setDots(dots, defaults)
  
  
  
  if(is.null(type)) type <- "raster"
  type <- match.arg(type[1], c("raster", "wiggles", "contour"))
  if(type %in% c("wiggles", "contour")) cbar <- NULL
  
  if(is.null(col)){
    if(type == "contour"){
      col <- "black"
    }else{
      col <- palGPR()
    }
  }
  
  parMar0 <- par()$mar
  
  if(!is.null(cbar) && par()$mar[4] < 5 && isFALSE(add)){
    par(mar = c(par()$mar[1:3], 5))
  }
  if(isTRUE(secaxis)){
    p <- ifelse(isZTime(x), 1, 0)
    par(mar = par()$mar + c(0,0,0,1 + p))
    if(!is.null(cbar)){
      if(!is.null(cbar[["pos"]])){
        cbar[["pos"]] <- cbar[["pos"]] + 1 + p
      }else{
        cbar[["pos"]] <- 2 + p
      }
    }
  }
  
  pleaseExport <- FALSE
  # export = lkjdf.pdf or sldf.png
  if(!is.null(export)){
    ext <- substr(export, nchar(export) - 2, nchar(export))
    if(tolower(ext) %in% c("png", "pdf")){
      pleaseExport <- TRUE
      asp <- ifelse(is.null(dots$asp), 1, dots$asp) 
      dots$asp <- NULL
      vmean <- 2
      if(isZTime(x))  vmean <- mean(.getVel(x, type = "vrms", strict = FALSE))
      heightPDF <- fac * 0.2 * abs(diff(defaults$ylim)) * asp * vmean/2 + sum(par()$omi[c(1,3)] + par()$mai[c(1,3)])
      widthPDF <- fac * 0.2 * abs(diff(defaults$xlim))  +                 sum(par()$omi[c(2,4)] + par()$mai[c(2,4)])
      if(tolower(ext) == "pdf"){
        Cairo::CairoPDF(file = export,
                        width = widthPDF,
                        height = heightPDF,
                        bg = "white",
                        pointsize=10,
                        title = defaults$main)
        
      }else{
        Cairo::CairoPNG(file = export,
                        width = round(widthPDF),
                        height = round(heightPDF),
                        bg = "white",
                        pointsize=10,
                        title = defaults$main)
      }
    }
  }
  
  # sd(diff(x@x)) == 0
  
    
  if(isFALSE(add) && type != "contour"){
    plot(0, type = "n", xaxs = "i", yaxs = "i",
         xaxt = "n", yaxt = "n", xlim = defaults$xlim, 
         ylim = ifelse(c(elev, elev), defaults$ylim, rev(defaults$ylim)), 
         xlab = defaults$xlab, ylab = defaults$ylab, bty = "n",
         mgp = defaults$mpg, asp = asp)
  }
  if(type == "raster"){
    if( isTRUE(all.equal(diff(x@x), 0)) || isTRUE(all.equal(diff(x@z), 0)) ){
      xdata <- x@data
    }else{
      nx = round(diff(range(x@z)) / median(diff(x@z))) + 1
      ny = round(diff(range(x@x)) / median(diff(x@x))) + 1
      xdata <- interpRegRaster(vx = x@z, vy = x@x, z = x@data, nx = nx, ny = ny)
      # xdata <- interp::bilinear.grid(x@z, x@x, x@data, 
      #                                xlim = range(x@z),
      #                                ylim = range(x@x),
      #                                dx = median(diff(x@z)),
      #                                dy = median(diff(x@x)))
    }
    # FIXME: use Window
    # FIXME: if too large, reduce!
    rasterImage(palCol(xdata, col = col, sym = sym, clim = clim), 
                xleft       = min(x@x), 
                xright      = max(x@x), 
                ytop        = ifelse(elev, max(x@z), min(x@z)),
                ybottom     = ifelse(elev, min(x@z), max(x@z)),
                interpolate = interpolate)
  }else if( type == "wiggles"){
    
    .plotWiggles(xvalues = x@x, yvalues = x@z, z = x@data,  
                 col = wiggles[["col"]],
                 wsize = wiggles[["size"]], wside = wiggles[["side"]], 
                 lwd = wiggles[["lwd"]], ylim = defaults$ylim)
  }else if(type == "contour"){
    if(isTRUE(add)){
      do.call(plot3D::contour2D, list(x = x@x, y = x@z, z = t(x@data), 
                                   col = col, add = TRUE,
                                    ...))
    }else{
      do.call(plot3D::contour2D, list(x = x@x, y = x@z, z = t(x@data), 
                                   col = col, add = FALSE,
                                   xaxs = "i", yaxs = "i",
                                   xaxt = "n", yaxt = "n", xlim = defaults$xlim, 
                                   ylim = ifelse(c(elev, elev), defaults$ylim, rev(defaults$ylim)), 
                                   xlab = defaults$xlab, ylab = defaults$ylab, bty= "n",
                                   # mgp = defaults$mpg, 
                                   asp = asp, ...))      
    }
  } 
  
  if(isFALSE(add)){
    title(defaults$main,  line = 2)
    if(length(x@path) > 0){
      mtext(x@path, side = 1, line = 3.5, adj = 0.5, cex = 0.65)
    }
    
    if(!is.null(markers)){
      do.call(.addMarkers, c(list(mrks = x@markers, xpos = x@x), markers))
    }
    
    if(!is.null(ann)){
      do.call(.addAnn, c(list(ann = x@ann, xpos = x@x), ann))
    }
    
    if(!is.null(z0) && length(x@z0) > 0){
      do.call(lines, c(list(x = x@x, y = x@z0), z0))
    }
    grid()
    axis(1, tck = 0.01, mgp = c(2, 0.5, 0), lwd = -1, lwd.ticks = 1)
    axis(2, tck = 0.01, mgp = c(2, 0.5, 0), lwd = -1, lwd.ticks = 1)
    if(isTRUE(secaxis)){
      if(isZTime(x) && isCommonOffset(x)){
        vmean <- mean(.getVel(x, type = "vrms", strict = FALSE))
        xat <- sort(par()$usr[3:4])
        .secaxis(x, antsep = x@antsep, z = x@z, v = vmean, z0 = mean(x@z0), xat = xat, side = 4)
      }else{
        axis(4, tck = 0.01, mgp = c(2, 0.5, 0), lwd = -1, lwd.ticks = 1)
      }
    }
    
    box()
    
    if(!is.null(cbar)){
      if(isTRUE(sym)){
        if(!is.null(clim)){
          clim <- max(abs(clim), na.rm = TRUE) * c(-1, 1)
        }else{
          clim <- max(abs(x), na.rm = TRUE) * c(-1, 1)
        }
      }else{
        if(!is.null(clim)){
          clim <- range(clim, na.rm = TRUE)
        }else{
          clim <- range(x, na.rm = TRUE)
        }
      }
      # col <- defaults[["col"]]
      # dim(col) <- c(length(col), 1)
      if(is.null(cbar[["clab"]])) cbar[["clab"]] <- x@dunit
      # do.call(colorbar, c(list(col = col, clim = range(x, na.rm = TRUE), zlim = defaults$ylim),  cbar))
      do.call(.colorbar, c(list(col = col, clim = clim),  cbar))
    }
  }
  if(isTRUE(pleaseExport)) dev.off()
  par(mar = parMar0)
}

.plotWiggles <- function(xvalues, yvalues, z, col,
                         wsize = 0.2, wside = 1, lwd = 0.5, ylim){
  
  if(wside > 0){
    for(i in rev(seq_along(xvalues))){
      y2   <- yvalues #+ topo[i]
      wig  <- cbind(wsize * z[,i] + xvalues[i], y2)
      wig1 <- rbind(c(xvalues[i], y2[1]), wig, c(xvalues[i], tail(y2,1) ))
      polygon(wig1, col = col, border = NA)
      rect(min(wig1[,1]), ylim[1], xvalues[i], ylim[2], 
           col = "white", border = NA)
    }
  }else{
    for(i in (seq_along(xvalues))){
      y2   <- yvalues #+ topo[i]
      wig  <- cbind(wsize * z[,i] + xvalues[i], y2)
      wig1 <- rbind(c(xvalues[i], y2[1]), wig, c(xvalues[i], tail(y2,1) ))
      polygon(wig1, col = col, border = NA)
      rect(max(wig1[,1]), ylim[1], xvalues[i], ylim[2], 
           col = "white", border = NA)
    }
  }
  for(i in (seq_along(xvalues))){
    y2 <- yvalues #+ topo[i]
    lines(xvalues[i] + wsize * z[, i], y2, lwd = lwd, col = col)  
  }
  box(bty = "o")
}


# mrks <- x@markers
# xpos <- x@x
.addMarkers <- function(mrks, xpos, lineSymbols = 0.35, pch = 25, 
                        colSymbols = "red", 
                        bgSymbols = "yellow", 
                        cexSymbols = 1,
                        lineText = 0.9, cexText = 0.6, colText = "red"){
  test <- mrks != ""
  if(any(test)){
    par(xpd=TRUE)
    
    points(xpos[test], rep(line2user(line = lineSymbols, side = 3), sum(test)), pch = pch,
           col = colSymbols, bg = bgSymbols, cex = cexSymbols)
    text(xpos[test], rep(line2user(line = lineText, side = 3), sum(test)), 
         mrks[test], cex = cexText, col = colText)
    #,pos=3,offset =0)
    par(xpd=FALSE)
  }
}

.addAnn <- function(ann, xpos, lineText = 1.7, colLine = "red", colText = "red", cexText = 0.9, lwd = 1){
  if(length(ann) > 0){
    testann <- (ann != "")
    if(sum(testann) > 0){
      # posann <- x
      ann <- gsub("#", "\n", ann)
      abline(v = xpos[testann], col = colLine, lwd = lwd)
      mtext(ann[testann], side = 3, line = lineText, at = xpos[testann],
            col = colText, cex = cexText)
    }
    return(TRUE)
  }else{
    return(FALSE)
  }
}

.colorbar <- function(col = palGPR(), clim, # zlim, <- replaced by par()$usr[3:4]
                      clab = "", 
                      w = 1,         # colorbar width in lines
                      pos = 1, 
                      hst = 0.5,  # space between colorbar and text in lines
                      fticks = 0.5,  # length factor for the ticks.
                      # 1 <-> space between colorbar and text
                      vclab = 0.5){  # vertical space to clab
  
  zlim <- par()$usr[3:4]
  par(xpd=TRUE)
  
  cbarPos <- c(line2user(pos, 4), line2user(pos+w, 4), line2user(0, 3), line2user(0, 1))
  col <- rev(col)
  dim(col) <- c(length(col), 1)
  rasterImage(col, 
              xleft = cbarPos[1], 
              xright = cbarPos[2], 
              ytop = cbarPos[3],
              ybottom = cbarPos[4])
  
  rect(cbarPos[1], cbarPos[4], cbarPos[2], cbarPos[3])
  cval <- pretty(clim, eps.correct = 0)
  cval <- cval[cval >= clim[1] & cval <= clim[2]]
  ylim <- range(zlim)
  # cpos <- (cval - clim[1])  * diff(ylim)/diff(clim)
  cpos <- (cval - clim[1])  * diff(ylim)/diff(clim) + ylim[1]
  
  text(x = line2user(pos + w + hst, 4), 
       y = cpos, 
       labels = rev(cval) , 
       adj = 0)
  segments(x0 = line2user(pos + w, 4),
           y0 = cpos,
           x1 = line2user(pos + w + fticks * hst, 4),
           y1 = cpos)
  
  if(clab != ""){
    clabx <- line2user(pos + w/2, 4)
    claby <- line2user(vclab, 3)
    text(x = clabx,
         y = claby,
         labels = clab,
         pos = 3,
         adj = 0.5)
  }
  
  par(xpd=FALSE)
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
    axis(side = side, at = t0 + depthat, labels = depth, tck = +0.01, 
         mgp = c(1,0.5,0), lwd = -1, lwd.ticks = 1)
    labels_0To1 <- FALSE
  }else{
    labels_0To1 <- d
  }
  axis(side = side, at = t0 + depthat2, labels = labels_0To1, tck = +0.007, 
       lwd = -1, lwd.ticks = 1)
  mtext(paste0("depth (", x@xunit, "),  v = ", round(v, 4), " ", 
               x@xunit, "/", x@zunit), side = side, line = 1.5)
}



#' Add a GPR trace on a plot
#' @param x (`GPR class`)
#' @param relTime0 (`logical[1]`) If `TRUE`, shift `x` to time-
#'                                      zero. 
#' @param ... Additional parameters to be passed to [lines()].
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
    if(isTRUE(dots$horiz) || is.null(dots$horiz)){
      dots[["y"]] <- x@data
      dots[["x"]] <- z
    }else{
      dots[["y"]] <- z
      dots[["x"]] <- x@data
    }
    dots$horiz <- NULL
    invisible( do.call(lines, dots) )
    #lines(z, x@data,...)
  }else{
    stop("x must a vector!")
  }
}



#' Add a GPR trace points on a plot
#'
#' @param x (`GPR class`)
#' @param relTime0 (`logical[1]`) If `TRUE`, shift `x` to time-
#'                                      zero. 
#' @param ... Additional parameters to be passed to [points()].
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
    if(isTRUE(dots$horiz) || is.null(dots$horiz)){
      dots[["y"]] <- x@data
      dots[["x"]] <- z
    }else{
      dots[["y"]] <- z
      dots[["x"]] <- x@data
    }
    dots$horiz <- NULL
    invisible( do.call(points, dots) )
    #lines(z, x@data,...)
  }else{
    stop("x must a vector!")
  }
}

#' Contours
#' 
#' `contour` extends `plot3D::contour2D` and creates a contour plot.
#' 
#' @method contour GPR
#' @rdname plot
#' @name contour
#' @export
# options: type=c(raster,wiggles), addTopo, clip, normalize
contour.GPR <- function(x, 
                        col = NULL,
                        # type = NULL,
                        horiz = TRUE,
                        interpolate = TRUE,
                        sym = TRUE,
                        clim = NULL,
                        add = FALSE,
                        asp = NA,
                        secaxis = TRUE,
                        elev = FALSE,
                        export = NULL,
                        fac = 1,
                        wiggles = list(side = 1, size = 0.2, col = "black", lwd = 0.5),
                        markers = list(lineSymbols = 0.35, pch = 25, 
                                       colSymbols = "red", 
                                       bgSymbols = "yellow", 
                                       cexSymbols = 1,
                                       lineText = 0.9, cexText = 0.6, colText = "red"),
                        ann = list(lineText = 1., colLine = "red", colText = "red", cexText = 0.75, lwd = 1),
                        z0 = list(lwd = 1, col = "green", lty = 1),
                        cbar = list(w = 1,
                                    pos = 1,
                                    hst = 0.5,
                                    fticks = 0.5,
                                    vclab = 0.5,
                                    clab = NULL),
                        ...){
  
  plot.GPR(x,
           col = col,
           type = "contour",
           horiz = horiz,
           interpolate = interpolate,
           sym = sym,
           clim = clim,
           add = add,
           asp = asp,
           secaxis = secaxis,
           elev = elev,
           export = export,
           fac = fac,
           wiggles = wiggles,
           markers = markers,
           ann = ann,
           z0 = z0,
           cbar = cbar,
           ...)
}
