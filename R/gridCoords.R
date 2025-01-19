# value = x, y, dx, dy


#' @name gridCoords
#' @rdname gridCoords
#' @export
#' @concept spatial computing
setGeneric("gridCoords",function(x,value){standardGeneric("gridCoords")})

#' @name gridCoords<-
#' @rdname gridCoords
#' @export
setGeneric("gridCoords<-",function(x,value){standardGeneric("gridCoords<-")})


#' Set grid coordinates the trace position.
#'
#' Set grid coordinates to a survey
#' @param x An object of the class GPRsurvey
#' @param value A list with following elements: \code{xlines} (number or id of 
#'              the GPR data along the x-coordinates), \code{ylines} (number or 
#'              id of the GPR data along the y-coordinates), \code{x} 
#'              (position of the x-GPR data on the x-axis),
#'              \code{x} (position of the y-GPR data on the y-axis)
#' @rdname gridCoords
#' @export
# gridCoords(SU) <- list(xlines = 1:10,
# x   = seq(0,
#              by = 2,
#              length.out = 10),
# ylines = 15 + (1:10),
# y   = c(0, 1, 2, 4, 6))
setReplaceMethod(
  f = "gridCoords",
  signature = "GPRsurvey",
  definition = function(x, value){
    # --- start checking --- #
    value$xlines <- unique(value$xlines)
    value$ylines <- unique(value$ylines)
    if( any(value$xlines %in% value$ylines) ){
      stop("No duplicates between 'x' and 'y' allowed!")
    }
    if(!is.null(value$xlines)){
      if(length(value$xlines) != length(value$x)){
        stop("length(xlines) must be equal to length(x)")
      }
      if(is.null(value$xstart)){
        value$xstart <- rep(0, length(value$xlines))
      }else if(length(value$xlines) != length(value$xstart)){
        stop("length(xlines) must be equal to length(xstart)")
      }
      if(is.null(value$xreverse)){
        value$xreverse <- rep(FALSE, length(value$xlines))
      }else if(length(value$xlines) != length(value$xreverse)){
        stop("length(xlines) must be equal to length(xreverse)")
      }
    # --- end checking --- #
      xNames <- .getSurveyXYNames(value$xlines, x, "xlines")
      if(!is.null(value$xlength)){
        if(length(value$xlines) != length(value$xlength)){
          stop("length(xlines) must be equal to length(xlength)")
        }
        for(i in seq_along(xNames)){
          id <- which(xNames[[i]] == x@names)
          ntr <- x@ntraces[id]
          x@coords[[id]] <- matrix(0, nrow = ntr, ncol = 3)
          x@coords[[id]][,1] <- value$x[i]
          if(isTRUE(value$xreverse[i])){
            x@coords[[id]][,2] <- seq(to       = value$xstart[i], 
                                             from         = value$xlength[i], 
                                             length.out = ntr)
          }else{
            x@coords[[id]][,2] <- seq(from       = value$xstart[i], 
                                             to         = value$xlength[i], 
                                             length.out = ntr)
          }
        }
      }else{
        for(i in seq_along(xNames)){
          id <- which(xNames[[i]] == x@names)
          y <- verboseF( getGPR(x, id), verbose = FALSE )
          ntr <- ncol(y)
          x@coords[[id]] <- matrix(0, nrow = ntr, ncol = 3)
          x@coords[[id]][,1] <- value$x[i]
          if(isTRUE(value$xreverse[i])){
            x@coords[[id]][,2] <- rev(y@x) + value$xstart[i]
          }else{
            x@coords[[id]][,2] <- y@x + value$xstart[i]
          }
        }
      }
    }
    if(!is.null(value$ylines)){
      if(length(value$ylines) != length(value$y)){
        stop("length(ylines) must be equal to length(y)")
      }
      if(is.null(value$ystart)){
        value$ystart <- rep(0, length(value$ylines))
      }else if(length(value$ylines) != length(value$ystart)){
        stop("length(ylines) must be equal to length(ystart)")
      }
      if(is.null(value$yreverse)){
        value$yreverse <- rep(FALSE, length(value$ylines))
      }else if(length(value$ylines) != length(value$yreverse)){
        stop("length(ylines) must be equal to length(xreverse)")
      }
      yNames <- .getSurveyXYNames(value$ylines, x, "ylines")
      if(!is.null(value$ylength)){
        if(length(value$ylines) != length(value$ylength)){
          stop("length(ylines) must be equal to length(ylength)")
        }
        for(i in seq_along(yNames)){
          id <- which(yNames[[i]] == x@names)
          ntr <- x@ntraces[id]
          x@coords[[id]] <- matrix(0, nrow = ntr, ncol = 3)
          if(isTRUE(value$yreverse[i])){
            x@coords[[id]][,1] <- seq(to         = value$ystart[i], 
                                             from       = value$ylength[i], 
                                             length.out = ntr)
          }else{
            x@coords[[id]][,1] <- seq(from       = value$ystart[i], 
                                             to         = value$ylength[i], 
                                             length.out = ntr)
          }
          x@coords[[id]][,2] <- value$y[i]
        }
      }else{
        for(i in seq_along(yNames)){
          id <- which(yNames[[i]] == x@names)
          y <- verboseF( getGPR(x, id), verbose = FALSE)
          ntr <- ncol(y)
          x@coords[[id]] <- matrix(0, nrow = ntr, ncol = 3)
          if(isTRUE(value$yreverse[i])){
            x@coords[[id]][,1] <- rev(y@x) + value$ystart[i]
          }else{
            x@coords[[id]][,1] <- y@x + value$ystart[i]
          }
          x@coords[[id]][,2] <- value$y[i] 
        }
      }
    }
    return(x)
  }
)

.getSurveyXYNames <- function(xylines, x, tag){
  if(is.numeric(xylines)){
    if(max(xylines) > length(x) || 
       min(xylines) < 1){
      stop("Length of '", tag, "' must be between 1 and ", length(x))
    }
    xNames <- x@names[xylines]
  }else if(is.character(xylines)){
    if(!all(xylines %in% x@names) ){
      stop("These names do not exist in the GPRsurvey object:\n",
           xylines[! (xylines %in% x@names) ])
    }
    xNames <- xylines
  }
  return(xNames)
}
