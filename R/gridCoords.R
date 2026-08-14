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
  definition = function(x, value) {
    
    if (isTRUE(x@view)) {
      message(
        "This GPRsurvey object is a view of an existing HDF5 file. ",
        "Coordinates were not modified. Use materialize(x, dsn = ...) first ",
        "if you want an independent, writable survey."
      )
      return(x)
    }
    
    changed_ids <- integer(0)
    
    # --- start checking --- #
    value$xlines <- unique(value$xlines)
    value$ylines <- unique(value$ylines)
    
    if (any(value$xlines %in% value$ylines)) {
      stop("No duplicates between 'xlines' and 'ylines' allowed!", call. = FALSE)
    }
    
    # ------------------ XLINES --------------- #
    if (!is.null(value$xlines)) {
      if (length(value$xlines) != length(value$x)) {
        stop("length(xlines) must be equal to length(x)", call. = FALSE)
      }
      
      if (is.null(value$xstart)) {
        value$xstart <- rep(0, length(value$xlines))
      } else if (length(value$xlines) != length(value$xstart)) {
        stop("length(xlines) must be equal to length(xstart)", call. = FALSE)
      }
      
      if (is.null(value$xreverse)) {
        value$xreverse <- rep(FALSE, length(value$xlines))
      } else if (length(value$xlines) != length(value$xreverse)) {
        stop("length(xlines) must be equal to length(xreverse)", call. = FALSE)
      }
      
      # xNames <- .getSurveyXYNames(value$xlines, x, "xlines")
      
      if (!is.null(value$xlength)) {
        if (length(value$xlines) != length(value$xlength)) {
          stop("length(xlines) must be equal to length(xlength)", call. = FALSE)
        }
        
        for (i in value$xlines){
          # id <- which(xNames[[i]] == x@names)
          ntr <- x@nx[i] #.gprsurvey_ntraces(x, id)
          
          x@coords[[i]] <- matrix(0, nrow = ntr, ncol = 3)
          colnames(x@coords[[i]]) <- c("x", "y", "z")
          
          x@coords[[i]][, 1] <- value$x[i]
          
          if (isTRUE(value$xreverse[i])) {
            x@coords[[i]][, 2] <- seq(
              to         = value$xstart[i],
              from       = value$xlength[i],
              length.out = ntr
            )
          } else {
            x@coords[[i]][, 2] <- seq(
              from       = value$xstart[i],
              to         = value$xlength[i],
              length.out = ntr
            )
          }
          
          # changed_ids <- c(changed_ids, id)
        }
        
      } else {
        h5 <- hdf5r::H5File$new(x@path, mode = "r")
        on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)
        nms <- names(h5[["lines"]])
        for (i in value$xlines) {
          # id <- which(xNames[[i]] == x@names)
          # y <- verboseF(getGPR(x, id), verbose = FALSE)
          # ntr <- ncol(y)
          ntr <- x@nx[i]
          
          grp <- h5[["lines"]][[nms[i]]]
          xpos <- grp[["x"]]$read()
          
          x@coords[[i]] <- matrix(0, nrow = ntr, ncol = 3)
          colnames(x@coords[[i]]) <- c("x", "y", "z")
          
          x@coords[[i]][, 1] <- value$x[i]
          
          if (isTRUE(value$xreverse[i])) {
            x@coords[[i]][, 2] <- rev(xpos) + value$xstart[i]
          } else {
            x@coords[[i]][, 2] <- xpos + value$xstart[i]
          }
          
          # changed_ids <- c(changed_ids, id)
        }
      }
    }
    
    # ------------------ YLINES --------------- #
    if (!is.null(value$ylines)) {
      if (length(value$ylines) != length(value$y)) {
        stop("length(ylines) must be equal to length(y)", call. = FALSE)
      }
      
      if (is.null(value$ystart)) {
        value$ystart <- rep(0, length(value$ylines))
      } else if (length(value$ylines) != length(value$ystart)) {
        stop("length(ylines) must be equal to length(ystart)", call. = FALSE)
      }
      
      if (is.null(value$yreverse)) {
        value$yreverse <- rep(FALSE, length(value$ylines))
      } else if (length(value$ylines) != length(value$yreverse)) {
        stop("length(ylines) must be equal to length(yreverse)", call. = FALSE)
      }
      
      # yNames <- .getSurveyXYNames(value$ylines, x, "ylines")
      
      if (!is.null(value$ylength)) {
        if (length(value$ylines) != length(value$ylength)) {
          stop("length(ylines) must be equal to length(ylength)", call. = FALSE)
        }
        
        for (i in value$ylines) {
          # id <- which(yNames[[i]] == x@names)
          # ntr <- .gprsurvey_ntraces(x, id)
          ntr <- x@nx[i]
        
          
          x@coords[[i]] <- matrix(0, nrow = ntr, ncol = 3)
          colnames(x@coords[[i]]) <- c("x", "y", "z")
          
          if (isTRUE(value$yreverse[i])) {
            x@coords[[i]][, 1] <- seq(
              to         = value$ystart[i],
              from       = value$ylength[i],
              length.out = ntr
            )
          } else {
            x@coords[[i]][, 1] <- seq(
              from       = value$ystart[i],
              to         = value$ylength[i],
              length.out = ntr
            )
          }
          
          x@coords[[i]][, 2] <- value$y[i]
          
          # changed_ids <- c(changed_ids, id)
        }
        
      } else {
        h5 <- hdf5r::H5File$new(x@path, mode = "r")
        on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)
        nms <- names(h5[["lines"]])
        for (i in value$ylines) {
          # id <- which(yNames[[i]] == x@names)
          # y <- verboseF(getGPR(x, id), verbose = FALSE)
          # ntr <- ncol(y)
          ntr <- x@nx[i]

          grp <- h5[["lines"]][[nms[i]]]
          xpos <- grp[["x"]]$read()
          
          
          x@coords[[i]] <- matrix(0, nrow = ntr, ncol = 3)
          colnames(x@coords[[i]]) <- c("x", "y", "z")
          
          if (isTRUE(value$yreverse[i])) {
            x@coords[[i]][, 1] <- rev(xpos) + value$ystart[i]
          } else {
            x@coords[[i]][, 1] <- xpos + value$ystart[i]
          }
          
          x@coords[[i]][, 2] <- value$y[i]
          
          # changed_ids <- c(changed_ids, id)
        }
      }
    }
    
    x <- .finalize_gridCoords_GPRsurvey(x, changed_ids)
    
    return(x)
  }
)

#' Persist changed coordinates (and recomputed intersections) to HDF5
#'
#' Called once at the end of `gridCoords<-`, after *every* requested
#' coordinate assignment (both the `xlines` and `ylines` blocks above) has
#' already been applied to `x@coords` in memory. This function:
#'
#' 1. Recomputes `@intersections` a single time, now that all coordinates
#'    for this call are final (recomputing per-line, inside the loops
#'    above, would repeat the same O(n^2) intersection search once per
#'    changed line for no benefit).
#' 2. Persists both the updated coordinates and the recomputed
#'    intersections to the HDF5 backing file in a **single** update
#'    transaction via `.h5_update_survey()` (see `hdf5_update.R`): one
#'    lock, one temporary copy, one open file handle, one checksum
#'    verification pass, one atomic replace. The previous version of this
#'    function opened/closed the backing file separately for the
#'    coordinates and for the intersections, which was neither atomic
#'    (a crash between the two calls could leave them out of sync) nor as
#'    efficient.
#'
#' @param x (`GPRsurvey`) Survey with `@coords` already updated in memory.
#' @param changed_ids (`integer`) Indices (into `x@names`) of the lines
#'   whose coordinates changed during this call.
#' @return The (possibly updated) `GPRsurvey` object.
#' @keywords internal
.finalize_gridCoords_GPRsurvey <- function(x, changed_ids) {
  changed_ids <- unique(as.integer(changed_ids))
  changed_ids <- changed_ids[!is.na(changed_ids)]

  if (length(changed_ids) == 0L) {
    return(x)
  }

  if (isTRUE(x@view)) {
    message(
      "This GPRsurvey object is a view of an existing HDF5 file. ",
      "Coordinates were not modified. Use materialize(x, dsn = ...) first ",
      "if you want an independent, writable survey."
    )
    return(x)
  }

  # Recompute intersections once, now that every coordinate change
  # requested in this call has already been applied in memory.
  if (.hasSlot(x, "intersections")) {
    x@intersections <- list()
    x <- findIntersection(x)
  }

  # Single atomic transaction: write coordinates for the changed lines and
  # the freshly recomputed intersections under one open HDF5 handle, verify
  # checksums, then swap the file into place. x@path is left untouched
  # unless this call succeeds completely.
  .h5_update_survey(x@path, function(h5) {
    .write_GPRsurvey_coords_hdf5(h5, x, changed_ids)
    .write_intersections_hdf5(h5, x)
  })

  x
}
# 
# .getSurveyXYNames <- function(xylines, x, tag){
#   if(is.numeric(xylines)){
#     if(max(xylines) > length(x) || min(xylines) < 1){
#       stop("Length of '", tag, "' must be between 1 and ", length(x))
#     }
#     xNames <- x@names[xylines]
#   }else if(is.character(xylines)){
#     if(!all(xylines %in% x@names) ){
#       stop("These names do not exist in the GPRsurvey object:\n",
#            xylines[! (xylines %in% x@names) ])
#     }
#     xNames <- xylines
#   }
#   return(xNames)
# }
