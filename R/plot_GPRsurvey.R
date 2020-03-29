 

# FIXME DOCUMENTATION in plot_GPR.R
#' @method plot GPRsurvey 
#' @name plot
#' @export
plot.GPRsurvey <- function(x, y, ...){
  if(length(x@coords) > 0 && any(sapply(x@coords, length) > 0)){
    #isNotNull <- which(!sapply(x@coords, is.null))
    #x <- x[isNotNull]
    add <- FALSE
    add_shp_files <- FALSE
    parLines <- list(col = "black")
    parArrows <- list(col = "red", length = 0.1)
    parIntersect <- list(pch = 1, cex = 0.8)
    parMarkers <- list(pch = 21, col = "black", bg = "red", cex = 0.7)
    xlab <- "x"
    ylab <- "y"
    main <- ""
    asp <- 1
    lwd <- 1
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
      if("parLines" %in% names(dots)){
        parLines <- dots$parLines
        dots$parLines <- NULL
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
      if("parMarkers" %in% names(dots)){
        parMarkers <- dots$parMarkers
        dots$parMarkers <- NULL
      }
    }
    #dots <- c(dots, list(type = "n",
    #                     xlab = xlab,
    #                     ylab = ylab))
    # print(dots)
    if(!add){
      FUN <- function(x, i, f){
        if(length(x) > 0) f(x[, i])
      }
      xlim <- c(min(unlist(sapply(x@coords, FUN, 1, min))),
                max(unlist(sapply(x@coords, FUN, 1, max))))
      ylim <- c(min(unlist(sapply(x@coords, FUN, 2, min))),
                max(unlist(sapply(x@coords, FUN, 2, max))))
      #do.call("plot", c(list((do.call(rbind, x@coords))[,1:2]), dots))
      plot(0, 0, type = "n", xlim = xlim, ylim = ylim, xlab = xlab,
           ylab = ylab, main = main, asp = asp)
    }
    for(i in 1:length(x)){
      if(length(x@coords[[i]]) == 0){
        message(x@names[i], ": coordinates missing.")
      }else{
        xyz <- unname(x@coords[[i]])
        if(!is.null(parLines)){
          parLines$x <- xyz[,1]
          parLines$y <- xyz[,2]
          # print(parLines)
          do.call(graphics::lines, parLines)
        }
        if(!is.null(parArrows)){
          # FIXME: test for distance between n and n-1 points 
          # (must be) large enough
          do.call(arrows, c(xyz[nrow(xyz)-1,1], xyz[nrow(xyz)-1,2], 
                            x1 = xyz[nrow(xyz),1],   y1 = xyz[nrow(xyz),2], 
                            parArrows))
        }
        if(!is.null(parMarkers) && length(x@markers) > 0){
          fidxyz <- x@coords[[i]][trimStr(x@markers[[i]]) != "", , 
                                  drop = FALSE]
          if(length(fidxyz)>0){
            do.call( graphics::points, c(list(x = fidxyz[, 1:2]), parMarkers))
          }
        }
        if(!is.null(parIntersect) && length(x@intersections) > 0 &&
           !is.null(x@intersections[[i]])){
          do.call(points , c(list(x=x@intersections[[i]][, 1:2]), 
                             parIntersect))
        }
      }
    }
    # if(!is.null(parIntersect) && length(x@intersections) > 0){ 
    #   for(i in 1:length(x@intersections)){
    #     if(!is.null(x@intersections[[i]])){
    #       do.call(points , c(list(x=x@intersections[[i]]$coord), 
    #                          parIntersect))
    #     }
    #   }
    # }
  }else{
    stop("no coordinates")
  }
}

# FIXME: documentation in file plot_GPR.R
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


# x@intersections
# 
# plot(x, parLines = NULL, parMarkers = NULL, parArrows = NULL, parIntersect = NULL)
# plot(x, parLines = NULL, parMarkers = NULL, parIntersect = NULL)  # arrows
# plot(x, parMarkers = NULL, parArrows = NULL, parIntersect = NULL) # lines
# plot(x, parLines = NULL, parArrows = NULL, parIntersect = NULL)   # markers
# plot(x, parLines = NULL, parArrows = NULL, parMarkers = NULL)   # intersections
# plot(x, parLines = NULL, parArrows = NULL, parMarkers = list(pch = 3), parIntersect = NULL)   # markers
# plot(x, parMarkers = NULL)
