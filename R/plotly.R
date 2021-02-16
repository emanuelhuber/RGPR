#' Interactive plot of GPR objects with plotly.js
#'
#' A wrapper for the function \code{\link[plotly]{plot_ly}}. Still experimental.
#'
#' 
#' @param x Object of class \code{GPR}
#' @param relTime0 logical. If \code{TRUE}, adjust vertical axis to time-zero.
#'                 If time-zero varies from trace to trace, the vertical axis
#'                 is adjusted to the mean time-zero. Apply first the function
#'                 \code{time0Cor()} to shift the traces to their time-zero.
#' @param addTopo logical. For 2D plot, add topography (if the data are sampled
#'                         in time unit, the data are migrated with a static
#'                         migration)
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
#' @param ... additional arguments passed to the plot_ly function, see 
#'            \code{\link[plotly]{plot_ly}}.
#' @name plotly
#' @rdname plotly
#' @export
plotly <- function(x, col = palGPR(),add = FALSE, 
                      relTime0 = FALSE,
                      main = NULL,
                      addTopo = FALSE,
                      elev    = FALSE,
                      clip = NULL,
                      ...){
  
  ##    Pre-processing    ================
  if(grepl("[s]$", x@depthunit) && isTRUE(addTopo)){
    x <- migrate(x)
  }
  
  if(isTRUE(relTime0)){
    time0Cor(x)
  }
  if(!is.null(clip) && is.numeric(clip)){
    if(length(clip) > 1){
      x@data <- RGPR:::.clip(x@data, clip[2], clip[1])
    }else if(length(clip) == 1){
      x@data <- RGPR:::.clip(x@data, clip[1])
    }
  }else if(is.null(clip)){
    # clip below the 0.01-quantile and above the 0.99-quantile
    x@data <- RGPR:::.clip(x@data, quantile(as.vector(x@data), 0.99, na.rm = TRUE),
                           quantile(as.vector(x@data), 0.01, na.rm = TRUE))
  }
  
  # title ===============
  if(is.null(main)) main <- x@name
  
  
  # x-values  ================
  xvalues <- x@pos
  if(isCMP(x)){
    if(length(x@antsep) == ncol(x)){
      xvalues <- x@antsep
    }else{
      stop("length(antsep(x)) != ncol(x). You must correctly define ",
           "the antenna separation distance with 'antsep(x) <- ...'")
    }
  }else if(toupper(x@surveymode) == "CMPANALYSIS"){
    # myclab <- ""
    # myxlab <- paste0("velocity (", x@posunit, "/", x@depthunit, ")")
    xvalues <- x@pos
  }else if( length(x@coord) > 0 ){
    # xvalues <- posLine(x@coord)
    x <- spRmDuplicates(x, verbose = FALSE)
    xvalues <- relTrPos(x)
  }
  # if(is.null(dots$xlim)) dots$xlim <- range(xvalues)
  
  # z-values ==================
  z <- t( as.matrix(x@data) )
  z[!is.finite(z)] <- 0
  
  
  # y-values ==================
  yvalues <- -x@depth
  
  if(isTRUE(elev)){
    yvalues <- max(x@coord[,3]) - yvalues
    #   if(is.null(dots$ylim)){
    #     dots$ylim <- range(yvalues)
    #   }
    # }else{
    #   if(is.null(dots$ylim)){
    #     dots$ylim <- rev(range(yvalues))
    #   }
    #   if(dots$ylim[1] < dots$ylim[2]) dots$ylim <- rev(dots$ylim)
    # 
  }
  fig <- plotly::plot_ly(x = xvalues, y = yvalues, 
                         z = x@data, 
                         type = "heatmap",
                         colors = col,
                         name = main,
                         ...)
  fig
}
