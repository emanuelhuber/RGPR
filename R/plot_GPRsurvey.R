#' @name plot
#' @method plot GPRsurvey
#' @export
plot.GPRsurvey <- function(x, 
                           markers = list(pch = 21, col = "black", bg = "red", cex = 0.7),
                           ann = list(pch = 1, cex = 0.8, col = "black"),
                           dirArrows = list(col = "red", length = 0.1),
                           asp = 1, 
                           add = FALSE,
                           # xlim,
                           # ylim,
                           ...){
  if(length(x@coords) > 0 && any(sapply(x@coords, length) > 0)){
    #isNotNull <- which(!sapply(x@coords, is.null))
    #x <- x[isNotNull]
    # add <- FALSE
    # add_shp_files <- FALSE
    # parLines <- list(col = "black")
    # parArrows <- list(col = "red", length = 0.1)
    # parIntersect <- list(pch = 1, cex = 0.8)
    # parMarkers <- list(pch = 21, col = "black", bg = "red", cex = 0.7)
    
    # main <- ""
    # 
    # lwd <- 1
    print(list(...))
    dots <- list(...)
    if( length(dots) > 0 ){
      uN <- table(names(dots))
      if(any(uN > 1)){
        idx <- which(uN > 1)
        stop("Arguments '", names(uN[idx]), "' is not unique!")  
      }    }
    
    if(!add){
      FUN <- function(x, i, f){
        if(length(x) > 0) f(x[, i])
      }
      defaults <- list(xlim = c(min(unlist(sapply(x@coords, FUN, 1, min))),
                                max(unlist(sapply(x@coords, FUN, 1, max)))),
                       ylim = c(min(unlist(sapply(x@coords, FUN, 2, min))),
                                max(unlist(sapply(x@coords, FUN, 2, max)))),
                       main = "",
                       xlab = "x",
                       ylab = "y")
      # if(missing(xlim)){
      #   xlim <- c(min(unlist(sapply(x@coords, FUN, 1, min))),
      #             max(unlist(sapply(x@coords, FUN, 1, max))))
      # }
      # if(missing(ylim)){
      #   ylim <- c(min(unlist(sapply(x@coords, FUN, 2, min))),
      #             max(unlist(sapply(x@coords, FUN, 2, max))))
      #   
      # }
      defaults <- setDots(dots, defaults)
      plot(0, 0, type = "n", xlim = defaults$xlim, ylim = defaults$ylim, xlab = defaults$xlab,
           ylab = defaults$ylab, main = defaults$main, asp = asp)
    }
    for(i in 1:length(x)){
      if(length(x@coords[[i]]) == 0){
        message(x@names[i], ": coordinates missing.")
      }else{
        xyz <- unname(x@coords[[i]])
        # if(!is.null(parLines)){
        #   parLines$x <- xyz[,1]
        #   parLines$y <- xyz[,2]
        # print(parLines)
        do.call(graphics::lines, c(list(x = xyz[,1], y = xyz[,2]), dots))
        # }
        if(!is.null(dirArrows)){
          # FIXME: test for distance between n and n-1 points 
          # (must be) large enough
          do.call(arrows, c(xyz[nrow(xyz)-1,1], xyz[nrow(xyz)-1,2], 
                            x1 = xyz[nrow(xyz),1],   y1 = xyz[nrow(xyz),2], 
                            dirArrows))
        }
        if(!is.null(markers) && length(x@markers) > 0){
          fidxyz <- x@coords[[i]][trimStr(x@markers[[i]]) != "", , 
                                  drop = FALSE]
          if(length(fidxyz)>0){
            do.call( graphics::points, c(list(x = fidxyz[, 1:2]), markers))
          }
        }
        if(!is.null(ann) && length(x@intersections) > 0 &&
           !is.null(x@intersections[[i]])){
          do.call(points , c(list(x=x@intersections[[i]][, 1:2]), 
                             ann))
        }
      }
    }
    
  }else{
    stop("no coordinates")
  }
}


#' @method lines GPRsurvey 
#' @name lines
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
