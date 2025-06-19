#' Resample GPR data (2D) to a regular grid
#' 
#' `resampleRegGrid` remove the low-frequency component (the so-called 'wow') of 
#' every traces.
#' 
#' The low-frequency component is computed by different methods:
#'   * `runmed` running median based on [stats::runmed]
#'   * `runmean` running mean based on [stats::filter]
#'   * `MAD` DEPRECATED - Median Absolute Deviation filter
#'   * `Gaussian` Gaussian smoothing applied to the trace samples
#'         after time-zero based on [mmand::gaussianSmooth]
#' 
#' Modified slots:
#'   * `data`: trace dewowed.
#'   * `proc`: updated with function name and arguments.
#' 
#' @param x    (`GPR`) An object of the class GPR.
#' @param type (`character[1]`) Dewow method,
#'             one of `runmed` (running median),
#'             `runmean` (running mean),
#'             `Gaussian` (Gaussian smoothing).
#' @param w    (`numeric[1]`) If `type` = `runmed`, 
#'             `MAD` or `runmean`, window length of the filter in
#'             trace unit;
#'             If `type` = `Gaussian`, standard deviation in trace
#'             unit.
#'             If `w = NULL`, `w` is estimated as five times the 
#'             wavelength corresponding to the maximum frequency of x 
#'             (estimated with [spec])
#' @param track (`logical[1]`) Should the processing step be tracked?
#' @return (`GPR`) An object of the class GPR whose traces are dewowed.
#' @name resampleRegGrid
#' @rdname resampleRegGrid
#' @export
#' @concept processing
setGeneric("resampleRegGrid", 
           function(x, dx = NULL, dz = NULL,
                    method = c("linear", "nearest", "pchip", "cubic", "spline"),
                    track = TRUE)
             standardGeneric("resampleRegGrid"))

#' @rdname resampleRegGrid
#' @export
setMethod("resampleRegGrid", 
          "GPR", 
          function(x, dx = NULL, dz = NULL,
                   method = c("linear", "nearest", "pchip", "cubic", "spline"),
                   track = TRUE){
  if(is.null(dx)){
    dx <- mean(diff(x@x))
  }
  if(is.null(dz)){
    dz <- mean(diff(x@z))
  }
  nx <- round(diff(range(x@x)) / dx) + 1
  nz <- round(diff(range(x@z)) / dz) + 1
  xp <- x
  a <- x@data
  if(!isFALSE(dx)){
    x_new <- seq(min(x@x), max(x@x), length.out = nx)  # Regular x-axis
    a <- apply(xp@data, 1, 
               function(row, x, x_new, method = method) 
                 signal::interp1(x, row, x_new, method = method), 
               x@x, x_new, method = method)
    xp@data <- t(a)
    xp@x <- x_new
    xp@z0 <- signal::interp1(x@x, x@z0, x_new, method = method)
    if(length(x@time) == ncol(x)){
      xp@time <- signal::interp1(x@x, x@time, x_new, method = method)
    }
    marks <- signal::interp1(x@x, seq_along(x@markers), x_new, method = method)
    if(length(x@markers) == ncol(x)){
      xp@markers <- x@markers[round(marks)]
    }
    if(length(x@ann) == ncol(x)){
      xp@ann <- x@ann[round(marks)]
    }
    if(length(x@antsep) == ncol(x)){
      xp@antsep <- signal::interp1(x@x, x@antsep, x_new, method = method)
    }
    if(nrow(x@coord) == ncol(x)){
      cumdist <- pathRelPos(x@coord[, 1:2], lonlat = isCRSGeographic(x))
      newcumdist <- seq(0, max(cumdist), by = dx)
      
      xp@coord <- matrix(nrow = ncol(xp), ncol = 3)
      # Interpolate x and y at new arc length positions
      xp@coord[, 1] <- signal::interp1(cumdist, x@coord[, 1], xi = newcumdist)
      xp@coord[, 2] <- signal::interp1(cumdist, x@coord[, 2], xi = newcumdist)
      xp@coord[, 3] <- signal::interp1(cumdist, x@coord[, 3], xi = newcumdist)
      
      if(nrow(x@angles) == ncol(x)){
        xp@angles <- matrix(nrow = ncol(xp), ncol = 2)
        # Interpolate x and y at new arc length positions
        xp@angles[, 1] <- signal::interp1(cumdist, x@angles[, 1], xi = newcumdist)
        xp@angles[, 2] <- signal::interp1(cumdist, x@angles[, 2], xi = newcumdist)
      }
    }
    if(nrow(x@rec) == ncol(x)){
      stop("regular resampling of @rec no yet implemented.\n",
           "Please contact me: emanuel.huber@pm.me")
    }
    if(nrow(x@trans) == ncol(x)){
      stop("regular resampling of @rec no yet implemented.\n",
           "Please contact me: emanuel.huber@pm.me")
    }

  }
  
  # Transpose and interpolate along columns (y-direction)
  if(!isFALSE(dz)){
    z_new <- seq(min(x@z), max(x@z), length.out = nz)  # Regular y-axis
    a <- apply(xp@data, 2, 
                     function(col, y, z_new, method = method) 
                       signal::interp1(y, col, z_new, method = method), 
                     x@z, z_new, method = method)
    xp@z <- z_new

    xp@data <- a
  }
  
  # if(isTRUE(trsp))   xp@data <- t(a)
  
  if(isTRUE(track)) proc(xp) <- getArgs()
  return(xp)
  
})