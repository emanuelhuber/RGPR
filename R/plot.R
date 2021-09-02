#' Plot the GPR object.
#'
#' \code{plot}: If the GPR object consists of a single trace, wiggle plot 
#' is shown. For CMP, the position of the traces on the x-axis is defined
#' by the antenna separation (\code{antsep(x)}).
#' 
#' Additional arguments
#' \itemize{
#'   \item \code{type}: Possible values for 1D plot: see argument \code{type} in
#'             \code{\link[graphics]{plot}}.
#'              For 2D plot: \code{"raster"} (default) or \code{"wiggles"}.
#'   \item \code{col}: Color. Default 1D: "black". 2D: \code{palGPR(n = 101)}
#' }
#' 
#' @param x Object of class \code{GPR}
#' @param add logical. If \code{TRUE}, add to current plot
#' @param relTime0 logical. If \code{TRUE}, adjust vertical axis to time-zero.
#'                 If time-zero varies from trace to trace, the vertical axis
#'                 is adjusted to the mean time-zero. Apply first the function
#'                 \code{time0Cor()} to shift the traces to their time-zero.
#' @param addFid logical. Add fiducial marks
#' @param addAnn logical. Add GPR annotations (line intersections)
#' @param addTime0 logical. Add time-zero line
#' @param addDepth0 logical. Add depth-zero line
#' @param addAmpl0 logical. Add an horizontal line on 1D plot
#' @param addTopo logical. For 2D plot, add topography (if the data are sampled
#'                         in time unit, the data are migrated with a static
#'                         migration)
#' @param add2ndYAxis logical. Should be a second (i.e., right) y-axis added?
#' @param elev logical. If \code{TRUE} the elevation instead of depth is
#'                      displayed on y-axis (and therefore, time-to-depth
#'                      conversion is applied).
#' @param clip numeric. If length-one numeric vector, clip the amplitudes 
#'                      larger than \code{clip} and smaller than \code{-clip}.
#'                      If length-two numeric vector, clip the amplitudes
#'                      smaller than \code{clip[1]} and larger than 
#'                      \code{clip[2]}. Per default, values below the 
#'                      0.01-quantile and above the 0.99-quantile are clipped.
#'                      If \code{clip = FALSE} the data are not clipped.
#' @param ratio logical.
#' @param barscale logical. Add a colorbar scale
#' @param wsize length-one numeric. Size of the wiggles (default = \code{1}).
#' @param wside length-one numeric. If positive the right part of the wavelet
#'              is colored. If negative, the left part of the wavelet is
#'              colored.
#' @param pdfName length-one character. Name/path of the PDF to export 
#'                without extension
#' @param pngName length-one character. Name/path of the PDF to export 
#'                without extension
#' @param NAcol lengthe-one vector: color to be used.
#' @param fast logical: if \code{TRUE} plots only a subset of the data (max. 
#'            1000 traces) to speed up plotting.               
#' @param ... additional arguments passed to the plotting methods 
#'            \code{\link[graphics]{plot}} for 1D plot and 
#'            \code{\link[plot3D]{Image}} for 2D plot. See also  \code{details}.
#' @method plot GPR 
#' @name plot
#' @rdname plot
#' @export
#Default 1D: "black". 2D: \code{palGPR(n = 101)}
# ##' @param y \code{NULL,} not used
# options: type=c(raster,wiggles), addTopo, clip, normalize
plot.GPR <- function(x, 
                     #y = NULL, 
                     add = FALSE, 
                     relTime0 = FALSE,
                     note = NULL, 
                     addFid = TRUE,
                     addAnn = TRUE,
                     addTime0 = TRUE,
                     addDepth0 = TRUE,
                     addAmpl0 = TRUE,
                     addTopo = FALSE,
                     add2ndYAxis = TRUE,
                     elev    = FALSE,
                     clip = NULL,
                     # ratio = 1,
                     barscale = TRUE, 
                     wsize = 1,   # wiggles
                     wside = 1,   # wiggles
                     pdfName = NULL,
                     pngName = NULL,
                     NAcol = "white",
                     fast = FALSE,
                     ...){
  # print(list(...))
  if(isTRUE(fast) && ncol(x) > 1000){
    # FIXME: decimate vector to specified length without repetition
    x <- x[, round(seq(from = 1, to = ncol(x), length.out = 1000))]
  }
  if(length(x@vel)>0){  
    v <- x@vel[[1]]
  }else{
    v <- 0
  }
  dots <- list(...)
  if(length(x@coord) == 0 ){
    elev <- FALSE
  }
  # if(grepl("[s]$", x@depthunit)){
  #   elev <- FALSE
  # }
  if(isTRUE(elev)){
    y_lab <- "elevation"
    addTopo <- TRUE
  }else{
    y_lab <- "depth"
  }
  #------------------------ trace plot (1D) -----------------------------------#
  # Plot 1D ------------------------
  if(any(dim(x) == 1)){
    if(isTRUE(add)){
      lines(x, ...)
    }else{
      par(mar = c(5, 4, 3, 2) + 0.1, oma = c(0, 0, 3, 0), mgp = c(2, 0.5, 0))
      z <- x@depth
      t0 <- x@time0
      if(isTRUE(relTime0)){
        z <- x@depth - x@time0
        t0 <- 0
      }
      if(isTRUE(elev)){
      z <- max(x@coord[,3]) - z
        if(is.null(dots$xlim)){
          dots$xlim <- rev(range(z))
        }
        if(dots$xlim[1] < dots$xlim[2]) dots$xlim <- rev(dots$xlim)
      }
      
      if(is.null(dots$xlab)){
        if(grepl("[m]$", x@depthunit)){
          dots$xlab <- paste0(y_lab, " (",x@depthunit,")")
        }else if(grepl("[s]$", x@depthunit)){
          dots$xlab <- paste0("two-way travel time (",x@depthunit,")")
        }else{
          dots$xlab <- x@depthunit
        }
      }
      if(is.null(dots$type)) dots$type <- "l"
      if(is.null(dots$col)) dots$col <- "black"
      if(is.null(dots$ylab)) dots$ylab <- "amplitude (mV)"
      dotsxaxt <- dots$xaxt 
      if(is.null(dots$xaxt)) dots$xaxt <- "n"
      if(is.null(dots$main)){
        myMain <- paste0(x@name, ": trace #", x@traces," @", round(x@pos,2), 
                         x@posunit)
      }else{
        myMain <- dots$main
        dots$main <- NULL
      } 
      if(!is.null(dots[["log"]]) && dots[["log"]] == "y"){
        dots[["log"]] <- ""
        x@data <- log(x@data)
      }
      
      
      do.call(plot, c(list(x = z, y = x@data), dots))
      
      if(is.null(dots$ann) || dots$ann != FALSE){
        if(is.null(dotsxaxt) || dotsxaxt != "n"){
          x_axis <- pretty(z, 10)
          xat <- axis(side = 1,  tck = +0.02)
          if(grepl("[m]$", x@depthunit)){
            # axis(side = 3, at = x_axis, labels = x_axis, tck = +0.02)
            axis(side = 3, tck = +0.02)
            #FIXME: use fx .depthAxis()
          }else if(grepl("[s]$", x@depthunit)){
            if( length(x@antsep) > 0 && is.null(dim(x@vel[[1]])) && length(v) == 1 && v > 0){
              if( x@antsep > 0){
                depth_0 <- t0 + depth0(0, v, antsep = x@antsep)
                depth2  <- seq(0.1, by = 0.1, 0.9)
                depthat0 <- depthToTime(0, 0, v, antsep = x@antsep)
                if(max(z)*v/2 > 1.3){
                  # depth <- pretty(seq(1.1, by = 0.1, max(z)*v/2), 10)
                  depth <- pretty(xat * v / 2, 10)
                  depthat <- depthToTime(depth, 0, v, antsep = x@antsep)
                  axis(side = 3, at = t0 + depthat, labels = depth, tck = +0.02)
                  #print(t0)
                }
                depthat2 <- depthToTime(depth2, 0, v, antsep = x@antsep)
                axis(side =3, at = t0 + depthat2, labels = FALSE, tck =+0.01)
                if(isTRUE(addDepth0)) abline(v = depth_0, col = "grey", lty = 3)
                mtext(paste0("depth (m),   v=", v, "m/ns"), side = 3, line = 2)
              }else{
                depth <- pretty(xat * v / 2, 10)
                depthat <- depthToTime(depth, 0, v, antsep = x@antsep)
                axis(side = 3, at = t0 + depthat, labels = depth, tck = +0.02)
              }
            }else{
              axis(side = 3, tck = +0.02)
            }
          }
        }
      }
      
      title(myMain, outer = TRUE)
      
      if(isTRUE(addAmpl0))  abline(h = 0, lty = 3, col = "grey")
      if(isTRUE(addTime0))  abline(v = t0, col = "red")
    }
  #------------------------ radargram plot (2D) -------------------------------#
  # Plot 2D ------  
  }else{
    ##    Pre-processing   ----------------
    if(grepl("[s]$", x@depthunit) && isTRUE(addTopo)){
      x <- migrate(x)
    }
    if(!is.null(clip) && is.numeric(clip)){
      if(length(clip) > 1){
        x@data <- .clip(x@data, clip[2], clip[1])
      }else if(length(clip) == 1){
        x@data <- .clip(x@data, clip[1])
      }
    }else if(is.null(clip)){
      # clip below the 0.01-quantile and above the 0.99-quantile
      x@data <- .clip(x@data, quantile(as.vector(x@data), 0.99, na.rm = TRUE),
                      quantile(as.vector(x@data), 0.01, na.rm = TRUE))
    }
   
    
    if(is.null(note)) note <- x@filepath
    xvalues <- x@pos
    yvalues <- x@depth
    myxlab <- paste0("length (", x@posunit, ")")
    if(grepl("[m]$", x@depthunit)) myxlab <- x@posunit
    myclab <- "mV"
    mymain <- x@name
    if(isCMP(x)){
      if(length(x@antsep) == ncol(x)){
        xvalues <- x@antsep
        myxlab <- paste0("antenna separation (", x@posunit, ")")
      }else{
        stop("length(antsep(x)) != ncol(x). You must correctly define ",
             "the antenna separation distance with 'antsep(x) <- ...'")
      }
    }else if(toupper(x@surveymode) == "CMPANALYSIS"){
      myclab <- ""
      myxlab <- paste0("velocity (", x@posunit, "/", x@depthunit, ")")
    }else if( length(x@coord) > 0 ){
      # xvalues <- posLine(x@coord)
      x <- spRmDuplicates(x, verbose = FALSE)
      xvalues <- relTrPos(x)
    }
    
    time_0 <- x@time0    
    t0 <- median(x@time0)
    z <- t( as.matrix(x@data) )
    
    if(addFid == FALSE){
      x@fid <- character(length(x@fid))
    }
    
    if( grepl("[s]$", x@depthunit) ){
      myylab <- paste0("two-way travel time (", x@depthunit, ")")
    }else if(grepl("[m]$", x@depthunit)){
      myylab <- paste0(y_lab, " (", x@depthunit, ")")
    }else{
      myylab <- x@depthunit
    }
    
    if(is.null(dots$xlab)) dots$xlab <- myxlab
    if(is.null(dots$ylab)) dots$ylab <- myylab
    if(!is.null(dots$main)) mymain <- dots$main
    dots$main <- NULL
    if(is.null(dots$type)) dots$type <- "raster"
    dots$type <- match.arg(dots$type, c("raster","wiggles", "contour"))
    
    if(dots$type == "contour"){
      if(is.null(dots[["col"]]))      dots[["col"]] <- "black"
      if(length(dots[["col"]]) == 1)  barscale <- FALSE
      if(add == TRUE){
        addDepth0 <- FALSE
        addAmpl0 <- FALSE
        addTime0 <- FALSE
        addFid <- FALSE
        addAnn <- FALSE
        note <- ""
      }
    }
    
    
    # if(is.null(xlim)){
    #   xlim <- range(xvalues)
    # }
    if(isTRUE(relTime0)){
      yvalues <- yvalues - t0
      time_0 <- x@time0 - t0
      t0 <- 0
    }
    if(isTRUE(elev)){
      yvalues <- max(x@coord[,3]) - yvalues
      if(is.null(dots$ylim)){
        dots$ylim <- range(yvalues)
      }
    }else{
      if(is.null(dots$ylim)){
        dots$ylim <- rev(range(yvalues))
      }
      if(dots$ylim[1] < dots$ylim[2]) dots$ylim <- rev(dots$ylim)
    }
    
    if(is.null(dots$xlim)) dots$xlim <- range(xvalues)
    
    op <- par(no.readonly=TRUE)
    
    colkeyDist <- 0.05
    if(barscale == FALSE){
      mai <- c(1.1, 1.02, 1.02, 1.02)
    }else{
      mai <- c(1.1, 1.02, 1.02, 1.4)
      if(grepl("[s]$", x@depthunit) && !isCMP(x) && 
         toupper(x@surveymode) != "CMPANALYSIS"){
        if(length(x@antsep) > 0 ){
          mai <- c(1.1, 1.02, 1.02, 2)
          colkeyDist <- 0.05  # 0.09
        }
      }
    }
    # print(colkeyDist)
    # omi <- c(0, 0, 0.6, 0)
    mai <- mai + c(0, 0, 0, 0)
    mgp <- c(2.5, 0.75, 0)
    fac <- 0.2
    omi <- par()$omi
    
    if(!is.null(pdfName)){
      # if the depthunit are "meters"
      xlim <- dots$xlim
      ylim <- dots$ylim
      if(is.null(dots$asp)){
        asp <- 1
      }else{
        asp <- dots$asp
      }
      if(grepl("[m]$", x@depthunit)){
        heightPDF <- fac * abs(diff(ylim)) * asp + sum(omi[c(1,3)] + mai[c(1,3)])
        widthPDF <- fac * abs(diff(xlim)) +  
          sum(omi[c(2,4)] + mai[c(2,4)])
      }else{
        heightPDF <- fac * abs(diff(ylim)) * asp * v/2 + 
          sum(omi[c(1,3)] + mai[c(1,3)])
        widthPDF <- fac * abs(diff(xlim))  + 
          sum(omi[c(2,4)] + mai[c(2,4)])
      }
      Cairo::CairoPDF(file = paste0(pdfName, ".pdf"),
                      width = widthPDF,
                      height = heightPDF,
                      bg = "white",
                      pointsize=10,
                      title = pdfName)
    }
    if(!is.null(pngName)){
      # if the depthunit are "meters"
      xlim <- dots$xlim
      ylim <- dots$ylim
      if(is.null(dots$asp)){
        asp <- 1
      }else{
        asp <- dots$asp
      }
      fac <- 50
      if(grepl("[m]$", x@depthunit)){
        heightPDF <- fac * abs(diff(ylim)) * asp + sum(omi[c(1,3)] + mai[c(1,3)])
        widthPDF <- fac * abs(diff(xlim)) +  
          sum(omi[c(2,4)] + mai[c(2,4)])
      }else{
        heightPDF <- fac * abs(diff(ylim)) * asp * v/2 + 
          sum(omi[c(1,3)] + mai[c(1,3)])
        widthPDF <- fac * abs(diff(xlim))  + 
          sum(omi[c(2,4)] + mai[c(2,4)])
      }
      widthPDF <- round(widthPDF)
      heightPDF <- round(heightPDF)
      print(paste(widthPDF, "x", heightPDF))
      Cairo::CairoPNG(file = paste0(pngName, ".png"),
                      width = widthPDF,
                      height = heightPDF,
                      bg = "white",
                      pointsize=10,
                      title = pngName)
    }
    #------------------------------ RASTER ------------------------------------#
    
    # Plot raster ======
    
    if(dots$type %in% c("raster", "contour")){
      dots$NAcol <- NAcol
      if(is.null(dots$clab)) dots$clab <- myclab
      if( (!is.null(dots$rasterImage) && isTRUE(rasterImage)) || is.null(dots$rasterImage)){
        # dy <- diff(yvalues)
        # dx <- diff(yvalues)
        # test1 <- abs(max(dx) - min(dx)) > sqrt(.Machine$double.eps)
        # test2 <- abs(max(dy) - min(dy)) > sqrt(.Machine$double.eps)
        testx <- abs(range(diff(xvalues))) > sqrt(.Machine$double.eps)
        testy <- abs(range(diff(yvalues))) > sqrt(.Machine$double.eps)
        # all not equal
        if(testx || testy){
          dots$rasterImage <- FALSE
        }else{
          dots$rasterImage <- TRUE
        }
      }
      # print(dots$rasterImage)
      if(!all(is.na(z)) && is.null(dots$zlim)){
        if( min(z, na.rm = TRUE) >= 0 ){
          # to plot amplitudes for example...
          dots$zlim <- c(0, max(z, na.rm = TRUE))
          # if I use 'dots$col', R returns 'dots$colkey'!!!!
          if(is.null(dots[["col"]]) && diff(range(z, na.rm = TRUE)) != 0){
            dots[["col"]] <- palGPR("slice", n = 101)
          } 
        }else if(!is.null(x@surveymode) && 
                 tolower(x@surveymode) %in% c("cmp", "reflection")){
          dots$zlim <- c(-1, 1) * max(abs(z), na.rm = TRUE)
        }else{
          dots$zlim <- range(z[is.finite(z)], na.rm = TRUE)
        }
      }
      clim <- dots$zlim
      # if I use 'dots$col', R returns 'dots$colkey'!!!!
      if(is.null(dots[["col"]])) dots[["col"]] <- palGPR(n = 101)
      
      
      if(!all(is.na(z)) && diff(range(z, na.rm = TRUE)) == 0){
        dots$zlim <- rep(0, 2)
        clim <- rep(z[1], 2)
        z[!is.na(z)] <- 0
      }
      
      if(is.null(dots$xaxs)) dots$xaxs <- "i"
      if(is.null(dots$yaxs)) dots$yaxs <- "i"
      if(is.null(dots$yaxt)) dots$yaxt <- "n"
      if(is.null(dots[["colkey"]])) dots[["colkey"]] <- FALSE
      
      if(add == TRUE){ 
        #par(new = TRUE)
        dots$add <- TRUE
        barscale <- FALSE
        dots[["colkey"]] <- FALSE
        dots[["main"]] <- ""
        addDepth0 <- FALSE
        addAmpl0 <- FALSE
        addTime0 <- FALSE
        addFid <- FALSE
        addAnn <- FALSE
        mymain <- ""
        note <- ""
        dots[["ann"]] <- FALSE
        dots[["axes"]] <- FALSE
        
      }else{
        # par( mai = mai, omi = omi, mgp = mgp)
        # par( mai = mai, mgp = mgp)
        par( mai = mai)
      }
      
      # print(clim)  
      # print(dots)  
      
      if(dots$type == "contour"){
        # if(is.null(dots[["colkey"]]) && isTRUE(dots[["add"]])){
        #   dots[["colkey"]] <- list(plot = FALSE)
        # } 
        #if(is.null(dots[["col"]])) dots[["col"]] <- "black"
        dots$type <- NULL
        do.call(plot3D::contour2D, c(list(x = xvalues, y = yvalues, z = z), 
                                     dots))
      }else if(dots$type == "raster"){
        #if(is.null(dots$bty)) dots$bty <- "n"
        dots$type <- NULL
        do.call(plot3D::image2D, c(list(x = xvalues, y = yvalues, z = z), dots))
      }
    #------------------------------ WIGGLES -----------------------------------#
    # Plot wiggles ================
    }else if(dots$type == "wiggles"){
      dots$type <- NULL
      barscale <- FALSE
      
      op <- par(no.readonly = TRUE) 
      dx <- mean(diff(xvalues)) # estimated x-step
      z <- x@data
      z[is.na(z)] = 0
      z <- z/max(abs(z)) * dx
      nr <- nrow(z)
      nc <- ncol(z)
      y0 <- 0
      topo <- rep(0L,nc)
      
      xlim <- dots$xlim + c(-1,1)*dx
      dots$xlim <- NULL
      
      ylim <- dots$ylim
      dots$ylim <- NULL
      
      if(is.null(dots$side)) side <- 1
      yaxt <- "s"
      bty <- "o"
      
      # col and lwd have no influence on plot
      col <- dots$col
      if(is.null(dots$col)) col <- "black"
      lwd  <- dots$lwd
      if(is.null(dots$lwd)) lwd <- 0.5
      
      par(mai = mai, omi = omi, mgp = mgp)
      
      do.call(plot, c( list( x = 0, type = "n", xaxs = "i", yaxs = "i", 
                             yaxt = "n", xlim = xlim, ylim = ylim, bty = "n"),
                       dots))
      
      if(wside > 0){
        for(i in rev(seq_along(xvalues))){
          y2 <- yvalues + topo[i]
          wig = cbind(wsize*z[,i] + xvalues[i], y2)
          wig1 = rbind(c(xvalues[i], y2[1]), wig, c(xvalues[i], tail(y2,1) ))
          polygon(wig1, col = col, border = NA)
          rect(min(wig1[,1]), ylim[1], xvalues[i], ylim[2], col = "white",
               border = NA)
          # lines(x[i]+wside*z[,i],y2,lwd=lwd)
        }
      }else{
        for(i in (seq_along(xvalues))){
          y2 <- yvalues + topo[i]
          wig = cbind(wsize*z[,i] + xvalues[i], y2)
          wig1 = rbind(c(xvalues[i], y2[1]), wig, c(xvalues[i], tail(y2,1) ))
          polygon(wig1, col = col, border = NA)
          rect(max(wig1[,1]), ylim[1], xvalues[i], ylim[2], col = "white", 
               border = NA)
        }
      }
      for(i in (seq_along(xvalues))){
        y2 <- yvalues + topo[i]
        lines(xvalues[i]+wsize*z[,i],y2,lwd=lwd)  
      }
      box(bty = bty)
    }
    #--------------------------------------------------------------------------#
    if(is.null(dots$ann) || dots$ann != FALSE){
      yat <- axis(side = 2)
      #xat <- axis(side = 1,  tck = +0.02)
      if(isTRUE(add2ndYAxis)){
        if(grepl("[m]$", x@depthunit) || grepl("CMP", toupper(x@surveymode))){
          axis(side = 4)
          #FIXME: use fx .depthAxis()
        }else if(grepl("[s]$", x@depthunit)){
          if(length(x@antsep) == 0) x@antsep <- 0
          # if(length(x@antsep) > 0 && is.null(dim(x@vel[[1]])) ){
          if(is.null(dim(x@vel[[1]])) && length(v) == 1 && v > 0 ){
            if(x@antsep > 0){
              depth_0 <- t0 + depth0(0, v, antsep = x@antsep)
              depth2  <- seq(0.1, by = 0.1, 0.9)
              depthat0 <- depthToTime(0, 0, v, antsep = x@antsep)
              if(max(yvalues) * v / 2 > 1.3){
                # depth <- pretty(seq(1.1, by = 0.1, max(z)*v/2), 10)
                depth <- pretty(yat * v / 2, 10)
                depthat <- depthToTime(depth, 0, v, antsep = x@antsep)
                axis(side = 4, at = t0 + depthat, labels = depth, tck = -0.02)
              }
              depthat2 <- depthToTime(depth2, 0, v, antsep = x@antsep)
              axis(side = 4, at = t0 + depthat2, labels = FALSE, tck = -0.01)
              axis(side = 4, at = depth_0, labels = "0", tick = FALSE)
              if(isTRUE(addDepth0)) abline(h = depth_0, col = "grey", lty = 3)
              # mtext(paste0("depth (m),   v=", v, "m/ns"), side = 4, line = 2)
              mtext(paste0("depth (", x@posunit, "),   v = ", round(v, 3), " ", x@posunit, 
                           "/",  x@depthunit), side = 4, line = 2.5)
            }else{
              depth_0 <- t0 + depth0(0, v, antsep = x@antsep)
              depth <- pretty(yat * v / 2, 10)
              depthat <- depthToTime(depth, 0, v, antsep = x@antsep)
              axis(side = 4, at = t0 + depthat, labels = depth, tck = -0.02)
              if(isTRUE(addDepth0)) abline(h = depth_0, col = "grey", lty = 3)
              mtext(paste0("depth (", x@posunit, "),   v = ", round(v, 3), " ", x@posunit, 
                           "/",  x@depthunit), side = 4, line = 2.5)
            }
          }else{
            axis(side = 4)
          }
        }
      }
    }
    
    if(isTRUE(addTime0) && !grepl("CMPANALYSIS", toupper(x@surveymode))){
      dx <- diff(xvalues)/2
      xt0 <- c(xvalues[1] - dx[1],  xvalues + c(dx,  tail(dx, 1)))
      lines(xt0, c(time_0, tail(time_0, 1)), type = "s", col = "chartreuse",
            lwd = 2)
    }
    
    # plot note
    if(!is.null(note) && length(note) > 0){
      mtext(note, side = 1, line = 4, cex=0.6)
    }
    
    
    test <- ( xvalues >= dots$xlim[1] & xvalues <= dots$xlim[2] )
    # plot fiducial markers
    if(isTRUE(addFid) && !is.null(x@fid) && length(x@fid) > 0){
      .plotFid(x@fid[test], xvalues[test])
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
      # op2 <- par(no.readonly=TRUE)
      # plot3D::colkey (col = dots$col, clim = clim, clab = dots$clab, clog = FALSE, 
      #                 add = TRUE, cex.clab = 0.75, dist = colkeyDist)
      if(is.null(clim)){
        clim <- c(-0.1, 0.1)
      }else if(clim[1] == clim[2]){
        clim <- clim + c(-0.1, 0.1)
      } 
      fields::image.plot(zlim = clim, 
                         legend.only = TRUE, 
                         col = dots$col, 
                         legend.shrink = 1)
      # .barScale(clim = clim, y = yvalues, col = dots$col, 
      # clab = dots$clab, clabcex = 0.8)
      # par(op2)
    }
    # par(op)
    
    if(!is.null(pdfName)){
      dev.off()
    }
    if(!is.null(pngName)){
      dev.off()
    }
  }
}

#' \code{contour} extends \code{plot3D::contour2D} and creates a contour plot.
#' @method contour GPR 
#' @name contour
#' @rdname plot
#' @export
# options: type=c(raster,wiggles), addTopo, clip, normalize
contour.GPR <- function(x, 
                        relTime0 = FALSE,
                        add = FALSE, 
                        note = NULL, 
                        addFid = TRUE,
                        addAnn = TRUE,
                        addTime0 = TRUE,
                        addDepth0 = TRUE,
                        addAmpl0 = TRUE,
                        addTopo = FALSE,
                        clip = NULL,
                        asp = 1,
                        barscale = TRUE, 
                        pdfName = NULL,
                        ...){
  
  plot.GPR(x, 
           add = add, 
           relTime0 = relTime0,
           note = note, 
           addFid = addFid,
           addAnn = addAnn,
           addTime0 = addTime0,
           addDepth0 = addDepth0,
           addAmpl0 = addAmpl0,
           addTopo = addTopo,
           clip = clip,
           ratio = ratio,
           barscale = barscale, 
           pdfName = pdfName,
           type = "contour",
           ...)
}


#---------------------------------- CLASS GPRsurvey ---------------------------#

# parameter add=TRUE/FALSE
#       addArrows = TRUE/FALSE
#' Plot the GPRsurvey object.
#'
#' Plot GPR suvey lines
#' @method plot GPRsurvey 
#' @name plot
#' @rdname plot
#' @export
plot.GPRsurvey <- function(x, y, ...){
  if(length(x@coords) > 0){
    #isNotNull <- which(!sapply(x@coords, is.null))
    #x <- x[isNotNull]
    add <- FALSE
    add_shp_files <- FALSE
    parArrows <- list(col = "red", length = 0.1)
    parIntersect <- list(pch = 1, cex = 0.8)
    parFid <- list(pch = 21, col = "black", bg = "red", cex = 0.7)
    xlab <- "x"
    ylab <- "y"
    main <- ""
    asp <- 1
    lwd <- 1
    col <- 1
    # print(list(...))
    dots <- list()
    if( length(list(...)) > 0 ){
      dots <- list(...)
      uN <- table(names(dots))
      if(any(uN > 1)){
        idx <- which(uN > 1)
        stop("Arguments '", names(uN[idx]), "' is not unique!")  
      }
      if( !is.null(dots$add) && isTRUE(dots$add) ){
        add <- TRUE
        dots$add <- NULL
      }
      if(!is.null(dots$main)){
        main <- dots$main
        dots$main <- NULL
      }
      if(!is.null(dots$xlab)){
        xlab <- dots$xlab
        dots$xlab <- NULL
      }
      if(!is.null(dots$ylab)){
        ylab <- dots$ylab
        dots$ylab <- NULL
      }
      if(!is.null(dots$asp)){
        asp <- dots$asp
        dots$asp <- NULL
      }
      if(!is.null(dots$col)){
        col <- dots$col
      }
      if("parArrows" %in% names(dots)){
        #if(!is.null(dots$lwd)){
        parArrows <- dots$parArrows
        dots$parArrows <- NULL
      }
      if("parIntersect" %in% names(dots)){
        #if(!is.null(dots$parIntersect)){
        parIntersect <- dots$parIntersect
        dots$parIntersect <- NULL
      }
      if("parFid" %in% names(dots)){
        parFid <- dots$parFid
        dots$parFid <- NULL
      }
      if(!is.null(dots$addFid)){
        stop(paste0("'addFid' no more used! Use instead 'parFid'",
                    " with a vector of arguments for the points",
                    "function.\n"))
      }
      # dots$addFid <- NULL
      # dots$add <- NULL
      if(!is.null(dots$shp_files)){
        add_shp_files <- TRUE
        shp_files <- dots$shp_files
      }
      dots$shp_files <- NULL
    }
    #dots <- c(dots, list(type = "n",
    #                     xlab = xlab,
    #                     ylab = ylab))
    # print(dots)
    if(!add){
      xlim <- c(min(sapply(x@coords, function(y) min(y[,1]))),
                max(sapply(x@coords, function(y) max(y[,1]))))
      ylim <- c(min(sapply(x@coords, function(y) min(y[,2]))),
                max(sapply(x@coords, function(y) max(y[,2]))))
      #do.call("plot", c(list((do.call(rbind, x@coords))[,1:2]), dots))
      plot(0,0, type = "n", xlim = xlim, ylim = ylim, xlab = xlab,
           ylab = ylab, main = main, asp = asp)
      # print(paste("ASP", asp))
    }
    if(add_shp_files){
      if(length(shp_files) > 0){
        sel <- seq(from=1,length.out=length(shp_files),by=2)
        BASEName <- unlist(strsplit(basename(shp_files),'[.]'), 
                           use.names = FALSE)[sel]
        DIRName <- dirname(shp_files)
        for(i in seq_along(shp_files)){
          shp <- rgdal::readOGR(DIRName[i], BASEName[i])
          message(DIRName[i], BASEName[i])
          plot(shp, add = TRUE, pch = 13, col = "darkblue")
        }
      }
    }
    for(i in 1:length(x)){
      if(is.null(x@coords[[x@names[i]]])){
        message(x@names[i], ": coordinates missing.")
      }else{
        xyz <- unname(x@coords[[x@names[i]]])
        dots$x <- xyz[,1]
        dots$y <- xyz[,2]
        do.call(graphics::lines, dots)
        if(!is.null(parArrows)){
          # https://www.javaer101.com/en/article/24422200.html
          # Warning messages:
          # 1: In (function (x0, y0, x1 = x0, y1 = y0, length = 0.25, angle = 30,  :
          #                    zero-length arrow is of indeterminate angle and so skipped
          # get each arrow's length by converting x and y coords to inches
          units <- par(c('usr', 'pin'))
          x_to_inches <- with(units, pin[1L]/diff(usr[1:2]))
          y_to_inches <- with(units, pin[2L]/diff(usr[3:4]))
          
          it <- 1
          x0 = xyz[nrow(xyz)-it,1]
          y0 = xyz[nrow(xyz)-it,2] 
          x1 = xyz[nrow(xyz),1]
          y1 = xyz[nrow(xyz),2]
          dists <- sqrt((x_to_inches * (x0 - x1))**2 + 
                          (y_to_inches * (y0 - y1))**2)
          distsmax <- sqrt((x_to_inches * diff(range(xyz[,1])))^2 + 
                             (y_to_inches *  diff(range(xyz[,2])))^2)
          while(dists < 0.001 && distsmax > 0.001){
            it <- it + 1
            x0 = xyz[nrow(xyz)-it,1]
            y0 = xyz[nrow(xyz)-it,2] 
            x1 = xyz[nrow(xyz),1]
            y1 = xyz[nrow(xyz),2]
            
            dists <- sqrt((x_to_inches * (x0 - x1))^2 + 
                            (y_to_inches * (y0 - y1))^2)
            
          }
          
          # do.call(arrows, c(x0 = xyz[nrow(xyz)-1,1], 
          #                   y0 = xyz[nrow(xyz)-1,2], 
          #                   x1 = xyz[nrow(xyz),1],
          #                   y1 = xyz[nrow(xyz),2], 
          #                   parArrows))
          do.call(arrows, c(x0 = x0, 
                            y0 = y0, 
                            x1 = x1,
                            y1 = y1, 
                            parArrows))
        }
        if(!is.null(parFid) && length(x@fids) > 0){
          fidxyz <- x@coords[[x@names[i]]][trimStr(x@fids[[i]]) != "", , 
                                           drop = FALSE]
          if(length(fidxyz)>0){
            do.call( graphics::points, c(list(x = fidxyz[, 1:2]), parFid))
          }
        }
      }
    }
    #niet <- lapply(x@coords, .plotLine, lwd = lwd, col = col )
    # if(!is.null(parArrows)){
    #   for(i in 1:length(x)){
    #     xyz <- unname(x@coords[[i]])
    #     do.call(arrows, c(xyz[nrow(xyz)-1,1], xyz[nrow(xyz)-1,2], 
    #                       x1 = xyz[nrow(xyz),1],   y1 = xyz[nrow(xyz),2], 
    #                       parArrows))
    #   }
    #   #niet <- lapply(x@coords, .plotArrows, parArrows)
    # }
    # if(!is.null(parFid)){
    #   for(i in 1:length(x)){
    #     fidxyz <- x@coords[[i]][trimStr(x@fids[[i]]) != "", , 
    #                                 drop = FALSE]
    #     if(length(fidxyz)>0){
    #       do.call( points, c(list(x = fidxyz[, 1:2]), parFid))
    #     }
    #   }
    # }
    if(!is.null(parIntersect) && length(x@intersections) > 0){ 
      for(i in 1:length(x@intersections)){
        if(!is.null(x@intersections[[i]])){
          do.call(points , c(list(x=x@intersections[[i]]$coord), 
                             parIntersect))
        }
      }
    }
  }else{
    stop("no coordinates")
  }
}

#' Plot the GPR survey as lines
#'
#' Plot the GPR survey as lines
#' @method lines GPRsurvey 
#' @name lines
#' @rdname lines
#' @export
lines.GPRsurvey <- function(x, ...){
  dots <- list(...)
  for(i in 1:length(x)){
    xy <- unname(x@coords[[i]][,1:2])
    dots$x <- xy[,1]
    dots$y <- xy[,2]
    do.call(lines, dots)
  }
}



.plotAnn <- function(ann, x, line=1.7){
  if(length(ann)>0){
    testann <- (ann != "")
    if(sum(testann)>0){
      posann <- x
      ann <- gsub("#","\n",ann)
      abline(v=posann[testann],col="red",lwd=1)
      mtext(ann[testann], side = 3, line = line, at=posann[testann],
            col="red", cex=0.9)
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


.barScale <- function(clim, y, col, clab = "mV", clabcex = 0.8){
  # clim <- sort(clim)
  usr <- par()$usr
  pin <- par()$pin  # inch
  mai <- par()$mai
  dxin <- diff(usr[1:2])/(pin[1])
  dylim <- diff(usr[3:4])
  fin <- par()$fin
  mai2 <- c(par("mai")[1], par("mai")[1] + pin[1] + 1, par("mai")[3], 0.6)
  par(mai=mai2)
  fin2 <- par()$fin
  wstrip <- dxin*(fin2[1] - mai2[2] - mai2[4])/2
  xpos <- usr[1] + dxin*(mai2[2] - mai[2])
  xstrip <- c( xpos - 20*wstrip,  xpos + 20*wstrip)#*c(0.9, 1.1)
  # ystrip <- seq(min(y), max(y), length.out = length(col))
  ystrip <- rev(seq(usr[3], usr[4], length.out = length(col)))
  ystrip <- sort(ystrip)
  dclim <- clim[2] - clim[1] 
  if(dclim == 0){
    axis(side = 4, las = 2, at = (usr[3] - usr[4])/2, labels = clim[1])
    image(x = xstrip, y = c(usr[4], usr[3]), 
          z = matrix(rep(clim[1], 2), nrow = 1),
          add = TRUE, col = col,
          axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
  }else{
    zstrip <- matrix(seq(clim[1], clim[2], length.out = length(col)), nrow = 1)
    pretty_z <- pretty(as.vector(zstrip))
    pretty_at <- usr[3] - dylim * (clim[1] - pretty_z)/dclim
    axis(side = 4, las = 2, at = pretty_at, labels = pretty_z)
    image(x = xstrip, y = ystrip, z = zstrip,
          add = TRUE, col = rev(col),
          axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
  }
  #  print(par("usr"))
  # axis(side=4, las=2)
  title(main=clab, line =1, cex.main = clabcex)
  box()
}

# we use the Sensors & Software method to plot the depth axis
# when the data are in time domain: because of the offset between
# transmitter and receiver, there is an offset between time zero and depth,
# the depth axes is squished.
.depthAxis <- function(y, pretty_y, time_0, v, antsep, depthunit, posunit ){
  if(grepl("[s]$",depthunit)){
    maxDepth <- v * max( abs(y - time_0) ) / 2
    #print(maxDepth)
    depthAll <- pretty(c(0, maxDepth), 10)
    depthAllPos <- depthToTime(depthAll, 0, v, antsep)
    axis(side = 4, at = -depthAllPos - time_0, labels = depthAll, tck = -0.02)
    depth2  <- seq(0.1, by = 0.1, 0.9)
    depthat2 <- depthToTime(depth2, 0, v, antsep = antsep)
    axis(side = 4, at = - depthat2 - time_0, labels = FALSE, tck = -0.01)
    mtext(paste0("depth (", posunit, "),   v = ",v, " ", posunit, "/", 
                 depthunit), side = 4, line = 2.5)
  }else{
    axis(side = 4, at = pretty_y, labels = -pretty_y)
    mtext(paste0("depth (", depthunit, ")") ,side = 4, line = 3)
  }
}

