#' Resample a GPR profile to a regular grid
#'
#' Resamples a \code{\linkS4class{GPR}} object onto a regular spatial
#' (\eqn{x}) and/or temporal/depth (\eqn{z}) grid using interpolation.
#'
#' This function is useful for correcting irregular trace spacing,
#' standardizing sampling intervals, and preparing data for imaging,
#' migration, filtering, or comparison between profiles.
#'
#' @param obj A \code{\linkS4class{GPR}} object.
#' @param dx Numeric. Desired trace spacing in the horizontal direction.
#' If \code{NULL}, the average spacing of \code{obj@x} is used. If
#' \code{FALSE}, no resampling is performed along the horizontal axis.
#' @param dz Numeric. Desired sample spacing in the vertical direction.
#' If \code{NULL}, the average spacing of \code{obj@z} is used. If
#' \code{FALSE}, no resampling is performed along the vertical axis.
#' @param method Character string specifying the interpolation method passed
#' to \code{\link[signal]{interp1}}. One of:
#' \describe{
#' \item{\code{"linear"}}{Linear interpolation.}
#' \item{\code{"nearest"}}{Nearest-neighbour interpolation.}
#' \item{\code{"pchip"}}{Shape-preserving cubic interpolation.}
#' \item{\code{"cubic"}}{Cubic interpolation.}
#' \item{\code{"spline"}}{Cubic spline interpolation.}
#' }
#' @param track Logical. If \code{TRUE}, the processing step is added to the
#' processing history stored in the object.
#'
#' @return
#' A \code{\linkS4class{GPR}} object resampled onto a regular grid.
#'
#' @details
#' The radargram amplitudes are interpolated independently along the
#' horizontal (\eqn{x}) and vertical (\eqn{z}) dimensions.
#'
#' When resampling along the profile direction (\code{dx}), the function also
#' interpolates associated trace attributes where available:
#' \itemize{
#' \item trace positions (\code{@x}),
#' \item first-break positions (\code{@z0}),
#' \item acquisition times (\code{@time}),
#' \item antenna separations (\code{@antsep}),
#' \item marker information (\code{@markers}),
#' \item annotations (\code{@ann}),
#' \item spatial coordinates (\code{@coord}),
#' \item antenna orientation angles (\code{@angles}).
#' }
#'
#' Spatial coordinates are resampled along cumulative profile distance using
#' \code{\link{pathRelPos}}. This preserves the geometry of curved survey
#' lines.
#'
#' Resampling of receiver coordinates (\code{@rec}) and transmitter
#' coordinates (\code{@trans}) is currently not implemented and will generate
#' an error if present.
#'
#' @examples
#' \dontrun{
#' data(frenkeLine00)
#'
#' # Resample to a regular trace spacing of 0.05 m
#' x <- resampleRegGrid(frenkeLine00, dx = 0.05)
#'
#' # Resample both horizontal and vertical axes
#' x <- resampleRegGrid(frenkeLine00,
#' dx = 0.05,
#' dz = 0.2)
#'
#' # Use shape-preserving interpolation
#' x <- resampleRegGrid(frenkeLine00,
#' method = "pchip")
#' }
#'
#' @name resampleRegGrid
#' @rdname resampleRegGrid
#' @export
#' @concept processing
setGeneric("resampleRegGrid", 
           function(obj, dx = NULL, dz = NULL,
                    method = c("linear", "nearest", "pchip", "cubic", "spline"),
                    track = TRUE)
             standardGeneric("resampleRegGrid"))

#' @rdname resampleRegGrid
#' @export
setMethod("resampleRegGrid", 
          "GPR", 
          function(obj, dx = NULL, dz = NULL,
                   method = c("linear", "nearest", "pchip", "cubic", "spline"),
                   track = TRUE){
  if(is.null(dx)){
    dx <- mean(diff(obj@x))
  }
  if(is.null(dz)){
    dz <- mean(diff(obj@z))
  }
  nx <- round(diff(range(obj@x)) / dx) + 1
  nz <- round(diff(range(obj@z)) / dz) + 1
  xp <- obj
  a <- obj@data
  if(!isFALSE(dx)){
    x_new <- seq(min(obj@x), max(obj@x), length.out = nx)  # Regular x-axis
    a <- apply(xp@data, 1, 
               function(row, obj, x_new, method = method) 
                 signal::interp1(obj, row, x_new, method = method), 
               obj@x, x_new, method = method)
    xp@data <- t(a)
    xp@x <- x_new
    xp@z0 <- signal::interp1(obj@x, obj@z0, x_new, method = method)
    if(length(obj@time) == ncol(obj)){
      xp@time <- signal::interp1(obj@x, obj@time, x_new, method = method)
    }
    marks <- signal::interp1(obj@x, seq_along(obj@markers), x_new, method = method)
    if(length(obj@markers) == ncol(obj)){
      xp@markers <- obj@markers[round(marks)]
    }
    if(length(obj@ann) == ncol(obj)){
      xp@ann <- obj@ann[round(marks)]
    }
    if(length(obj@antsep) == ncol(obj)){
      xp@antsep <- signal::interp1(obj@x, obj@antsep, x_new, method = method)
    }
    if(nrow(obj@coord) == ncol(obj)){
      cumdist <- pathRelPos(obj@coord[, 1:2], lonlat = isCRSGeographic(obj))
      newcumdist <- seq(0, max(cumdist), by = dx)
      
      xp@coord <- matrix(nrow = ncol(xp), ncol = 3)
      # Interpolate x and y at new arc length positions
      xp@coord[, 1] <- signal::interp1(cumdist, obj@coord[, 1], xi = newcumdist)
      xp@coord[, 2] <- signal::interp1(cumdist, obj@coord[, 2], xi = newcumdist)
      xp@coord[, 3] <- signal::interp1(cumdist, obj@coord[, 3], xi = newcumdist)
      
      if(nrow(obj@angles) == ncol(obj)){
        xp@angles <- matrix(nrow = ncol(xp), ncol = 2)
        # Interpolate x and y at new arc length positions
        xp@angles[, 1] <- signal::interp1(cumdist, obj@angles[, 1], xi = newcumdist)
        xp@angles[, 2] <- signal::interp1(cumdist, obj@angles[, 2], xi = newcumdist)
      }
    }
    if(nrow(obj@rec) == ncol(obj)){
      stop("regular resampling of @rec no yet implemented.\n",
           "Please contact me: emanuel.huber@pm.me")
    }
    if(nrow(obj@trans) == ncol(obj)){
      stop("regular resampling of @rec no yet implemented.\n",
           "Please contact me: emanuel.huber@pm.me")
    }

  }
  
  # Transpose and interpolate along columns (y-direction)
  if(!isFALSE(dz)){
    z_new <- seq(min(obj@z), max(obj@z), length.out = nz)  # Regular y-axis
    a <- apply(xp@data, 2, 
                     function(col, y, z_new, method = method) 
                       signal::interp1(y, col, z_new, method = method), 
                     obj@z, z_new, method = method)
    xp@z <- z_new

    xp@data <- a
  }
  
  # if(isTRUE(trsp))   xp@data <- t(a)
  
  if(isTRUE(track)) proc(xp) <- getArgs()
  return(xp)
  
})