#' @name plot
#' @method plot GPRslice
#' @export
plot.GPRslice <- function(x, 
                          type = "raster",
                          col = NULL, 
                          interpolate = TRUE,
                          sym = TRUE,
                          clim = NULL,
                          add = FALSE,
                          asp = 1,
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
  
  defaults <- list(xlab = .xlab(x),
                   ylab = paste0(x@ylab, " (", x@xunit, ")"),
                   xlim = x@center[1] + c(0, x@dx * (nrow(x) - 1)),
                   ylim = x@center[2] + c(0, x@dy * (ncol(x) - 1)),
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
  
  if(is.null(col)) col <- palGPR()
  
  
    
  plot(0, 
       type = "n", 
       xaxs = "i", yaxs = "i",
       xaxt = "n", yaxt = "n", 
       xlim = defaults$xlim, 
       ylim =  defaults$ylim, 
       xlab = defaults$xlab, 
       ylab = defaults$ylab, 
       bty  = "n",
       mgp  = defaults$mpg, 
       asp  = asp)
  rasterImage(palCol(t(x@data[,ncol(x):1]), col = col, sym = sym, clim = clim), 
              xleft       = min(defaults$xlim), 
              xright      = max(defaults$xlim), 
              ytop        = max(defaults$ylim),
              ybottom     = min(defaults$ylim),
              interpolate = interpolate)
  
  grid()
  axis(1, tck = 0.01, mgp = c(2, 0.5, 0), lwd = -1, lwd.ticks = 1)
  axis(2, tck = 0.01, mgp = c(2, 0.5, 0), lwd = -1, lwd.ticks = 1)
  box()
}
#                           
#                           main = NULL, 
#                           xlab = NULL,
#                           ylab = NULL,
#                           col = NULL,
#                           # clim = NULL,
#                           asp = 1,
#                           # xclipData = NULL,
#                           clim = NULL,
#                           add = FALSE,
#                           ...){
#   if(is.null(main)){
#     # z_lab <- x@zlab
#     # if(z_lab == ""){
#     #   z_lab <- ifelse(isZTime(x), "time", "depth")
#     # }
#     main <- paste0("Slice : ", x@z, " ", x@zunit)
#   }
#   x_unit <- ifelse(x@xunit ==  "degree", "\u00B0", x@xunit)
#   if(is.null(xlab)){
#     xlab <- paste0( ifelse(x@xlab ==  "", "x", x@xlab), " (", x@xunit, ")")
#   }
#   if(is.null(ylab)){
#     ylab <- paste0( ifelse(x@ylab ==  "", "y", x@ylab), " (", x@xunit, ")")
#   }
#   # if(is.null(xclipData)){
#   #   xclipData <- c(quantile(as.vector(x@data), 0.99, na.rm = TRUE),
#   #              quantile(as.vector(x@data), 0.01, na.rm = TRUE))
#   # }
#   # x <- clipData(x, xclipData = xclipData, track = FALSE)
#   
#   if( min(x@data, na.rm = TRUE) >= 0 ){
#     # to plot amplitudes for example...
#     # if(is.null(clim)) clim <- c(0, max(x@data, na.rm = TRUE))
#     if(is.null(col))  col <- palGPR("slice")
#     if(is.null(clim)) clim <- c(0, 1) * max(abs(x@data), na.rm = TRUE)
#   }else{
#     # if(is.null(clim)) clim <- c(-1, 1) * max(abs(x@data), na.rm = TRUE)
#     if(is.null(col))  col <-  palGPR()
#     if(is.null(clim)) clim <- c(-1, 1) * max(abs(x@data), na.rm = TRUE)
#   }
#   
#   vx <- x@center[1] + seq(0, by = x@dx, length.out = dim(x@data)[1])
#   vy <- x@center[2] + seq(0, by = x@dy, length.out = dim(x@data)[2])
#   
#   
#   if(isTRUE(all.equal(x@rot[3], 0))){
#     plot3D::image2D(x = vx, y = vy, z = x@data,
#                     main = main, xlab = xlab, ylab = ylab, 
#                     # clim = clim, 
#                     col = col, asp = asp, clim = clim, add = add, ...)
#   }else{
#     xy_corners <- rbind(c(vx[1], vy[1]),
#                         c(vx[1], tail(vy, 1)),
#                         c(tail(vx, 1), vy[1]),
#                         c(tail(vx, 1), tail(vy, 1)))
#     xy_cornersnew <- .georef(xy_corners,
#                           alpha = -x@rot[3],
#                           cloc  = x@rot[1:2],
#                           creg  = x@rot[1:2])
#     # vy_new <- .georef(cbind(vx[1], vy),
#     #                       alpha = -zr@transf[5],
#     #                       cloc  = zr@transf[3:4],
#     #                       creg  = zr@transf[1:2])
#     if(isFALSE(add)){
#       plot(0, type = "n", asp = asp, xlim = range(xy_cornersnew[,1]), 
#            ylim = range(xy_cornersnew[,2]),
#            xaxs = "i", yaxs = "i")
#     }
#     center_rot <- .georef(matrix(x@center[1:2], nrow = 1, ncol = 2),
#                           alpha = -x@rot[3],
#                           cloc  = x@rot[1:2],
#                           creg  = x@rot[1:2])
#     x_nrm <- (x@data + clim[2])/(clim[2] - clim[1])
#     x_nrm[x_nrm <= 0] <- 0
#     x_nrm[x_nrm >= 1] <- 1
#     uu <- as.raster(t(x_nrm))
#     # uu[] <- (palCol(t(x_nrm[,ncol(x_nrm):1])))
#     uu[] <- col[ t(x_nrm[,ncol(x_nrm):1]) * (length(col) - 1) + 1 ]
#     
#     # return(col[ CCY*(length(col) - 1) + 1 ] 
#     # x_norm <- (x@data - min(x@data, na.rm = TRUE)) /
#     #             (max(x@data, na.rm = TRUE) - min(x@data, na.rm = TRUE))
#     # uu <- as.raster(t(x_norm))
#     # uu[] <- palCol(t(x_norm[,ncol(x_norm):1]))
#   
#     # rasterImage(uu,
#     #             xleft = xy_cornersnew[1, 1],
#     #             ybottom = xy_cornersnew[1, 2],
#     #             xright = xy_cornersnew[2, 1],
#     #             ytop = center_rot[2]  + (tail(vy, 1)  - vy[1]),
#     #             angle = x@rot[3]/pi * 180)
#     rasterImage(uu,
#                 xleft = center_rot[1],
#                 ybottom = center_rot[2],
#                 xright = center_rot[1] + (tail(vx, 1) - vx[1]),
#                 ytop = center_rot[2]  + (tail(vy, 1)  - vy[1]),
#                 angle = x@rot[3]/pi * 180)
# 
# 
#   }
# }
