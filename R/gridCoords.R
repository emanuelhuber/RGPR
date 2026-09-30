#' Get or set grid coordinates
#'
#' Get or set grid coordinates for the traces of a
#' \code{\linkS4class{GPRsurvey}} object.
#'
#' Grid lines are divided into two groups:
#'
#' \itemize{
#'   \item \code{xlines}: survey lines with a constant x-coordinate and
#'   varying y-coordinates.
#'   \item \code{ylines}: survey lines with varying x-coordinates and a
#'   constant y-coordinate.
#' }
#'
#' When \code{xlength} or \code{ylength} is supplied, trace coordinates are
#' generated as an evenly spaced sequence between the corresponding start
#' and end coordinates. Otherwise, the original trace-position vectors are
#' read from the HDF5 backing file and shifted by \code{xstart} or
#' \code{ystart}.
#'
#' Coordinates cannot be modified while the survey is an HDF5 view. Use
#' \code{materialize()} first to create an independent, writable survey.
#'
#' @param x A \code{\linkS4class{GPRsurvey}} object.
#'
#' @param value A named list describing the grid geometry. It may contain:
#' \describe{
#'   \item{\code{xlines}}{
#'     Integer indices of lines having a constant x-coordinate.
#'   }
#'   \item{\code{ylines}}{
#'     Integer indices of lines having a constant y-coordinate.
#'   }
#'   \item{\code{x}}{
#'     Constant x-coordinate for every line in \code{xlines}.
#'   }
#'   \item{\code{y}}{
#'     Constant y-coordinate for every line in \code{ylines}.
#'   }
#'   \item{\code{xstart}}{
#'     Optional starting y-coordinate for every line in \code{xlines}.
#'     Defaults to zero.
#'   }
#'   \item{\code{ystart}}{
#'     Optional starting x-coordinate for every line in \code{ylines}.
#'     Defaults to zero.
#'   }
#'   \item{\code{xlength}}{
#'     Optional ending y-coordinate for every line in \code{xlines}.
#'     If omitted, the original trace positions are read from HDF5.
#'   }
#'   \item{\code{ylength}}{
#'     Optional ending x-coordinate for every line in \code{ylines}.
#'     If omitted, the original trace positions are read from HDF5.
#'   }
#'   \item{\code{xreverse}}{
#'     Optional logical vector indicating whether the trace direction of
#'     each line in \code{xlines} should be reversed. Defaults to
#'     \code{FALSE}.
#'   }
#'   \item{\code{yreverse}}{
#'     Optional logical vector indicating whether the trace direction of
#'     each line in \code{ylines} should be reversed. Defaults to
#'     \code{FALSE}.
#'   }
#' }
#'
#' The vectors associated with \code{xlines} must have the same length as
#' \code{xlines}. Likewise, the vectors associated with \code{ylines} must
#' have the same length as \code{ylines}.
#'
#' Line indices must be unique, valid indices into the survey, and cannot
#' occur in both \code{xlines} and \code{ylines}.
#'
#' @return
#' \code{gridCoords(x)} returns the grid coordinates associated with
#' \code{x}. The replacement form returns the modified
#' \code{\linkS4class{GPRsurvey}} object.
#'
#' @examples
#' \dontrun{
#' gridCoords(SU) <- list(
#'   xlines  = 1:10,
#'   x       = seq(0, by = 2, length.out = 10),
#'   xstart  = rep(0, 10),
#'   xlength = rep(20, 10),
#'   ylines  = 11:15,
#'   y       = seq(0, by = 2, length.out = 5),
#'   ystart  = rep(0, 5),
#'   ylength = rep(18, 5)
#' )
#' }
#'
#' @name gridCoords
#' @rdname gridCoords
#' @concept spatial computing
#' @export
setGeneric(
  name = "gridCoords",
  def = function(x, value) {
    standardGeneric("gridCoords")
  }
)


#' @name gridCoords<-
#' @rdname gridCoords
#' @export
setGeneric(
  name = "gridCoords<-",
  def = function(x, value) {
    standardGeneric("gridCoords<-")
  }
)


#' Validate survey line indices
#'
#' @param ids Line indices to validate.
#' @param nlines Total number of lines in the survey.
#' @param tag Name used in error messages.
#'
#' @return An integer vector of validated line indices, or \code{NULL}.
#'
#' @keywords internal
#' @noRd
.validate_grid_line_ids <- function(ids, nlines, tag) {
  
  if (is.null(ids)) {
    return(NULL)
  }
  
  if (!is.numeric(ids)) {
    stop(
      "'", tag, "' must contain numeric line indices.",
      call. = FALSE
    )
  }
  
  if (length(ids) == 0L) {
    return(integer(0))
  }
  
  if (
    anyNA(ids) ||
    any(!is.finite(ids)) ||
    any(ids != floor(ids))
  ) {
    stop(
      "'", tag, "' must contain finite, non-missing, whole-number indices.",
      call. = FALSE
    )
  }
  
  ids <- as.integer(ids)
  
  if (any(ids < 1L | ids > nlines)) {
    invalid <- ids[ids < 1L | ids > nlines]
    
    stop(
      "Invalid line indices in '", tag, "': ",
      paste(unique(invalid), collapse = ", "),
      ". Valid indices are between 1 and ", nlines, ".",
      call. = FALSE
    )
  }
  
  if (anyDuplicated(ids)) {
    duplicated_ids <- unique(ids[duplicated(ids)])
    
    stop(
      "Duplicated line indices in '", tag, "': ",
      paste(duplicated_ids, collapse = ", "),
      ". Each line may be specified only once.",
      call. = FALSE
    )
  }
  
  ids
}


#' Normalize a grid-coordinate argument
#'
#' @param value Argument value, or \code{NULL}.
#' @param n Expected argument length.
#' @param default Default scalar value used when \code{value} is
#'   \code{NULL}.
#' @param name Argument name used in error messages.
#'
#' @return A vector of length \code{n}.
#'
#' @keywords internal
#' @noRd
.normalize_grid_argument <- function(value, n, default, name) {
  if (is.null(value)) {
    return(rep(default, n))
  }
  if (length(value) != n) {
    stop("length(", name, ") must be equal to ",
      ifelse(startsWith(name, "x"), "length(xlines)", "length(ylines)"),
      ".",
      call. = FALSE
    )
  }
  value
}


#' Validate a required grid-coordinate argument
#'
#' @param value Argument value.
#' @param n Expected argument length.
#' @param name Argument name used in error messages.
#' @param lines_name Name of the associated line-index argument.
#'
#' @return The validated argument.
#'
#' @keywords internal
#' @noRd
.validate_required_grid_argument <- function(value, n, name, lines_name) {
  if (is.null(value)) {
    stop(
      "'", name, "' must be supplied when '", lines_name,
      "' is specified.",
      call. = FALSE
    )
  }

  if (length(value) != n) {
    stop(
      "length(", name, ") must be equal to length(",
      lines_name, ").",
      call. = FALSE
    )
  }
  
  if (!is.numeric(value)) {
    stop(
      "'", name, "' must be numeric.",
      call. = FALSE
    )
  }
  
  if (anyNA(value) || any(!is.finite(value))) {
    stop(
      "'", name, "' must contain finite, non-missing values.",
      call. = FALSE
    )
  }
  
  value
}


#' Validate a coordinate endpoint argument
#'
#' @param value Argument value, or \code{NULL}.
#' @param n Expected argument length.
#' @param name Argument name used in error messages.
#' @param lines_name Name of the associated line-index argument.
#'
#' @return The validated argument, or \code{NULL}.
#'
#' @keywords internal
#' @noRd
.validate_optional_grid_argument <- function(
    value,
    n,
    name,
    lines_name
) {
  
  if (is.null(value)) {
    return(NULL)
  }
  
  if (length(value) != n) {
    stop(
      "length(", name, ") must be equal to length(",
      lines_name, ").",
      call. = FALSE
    )
  }
  
  if (!is.numeric(value)) {
    stop(
      "'", name, "' must be numeric.",
      call. = FALSE
    )
  }
  
  if (anyNA(value) || any(!is.finite(value))) {
    stop(
      "'", name, "' must contain finite, non-missing values.",
      call. = FALSE
    )
  }
  
  value
}


#' Validate reversal flags
#'
#' @param value Logical reversal flags.
#' @param n Expected argument length.
#' @param name Argument name used in error messages.
#'
#' @return A logical vector of length \code{n}.
#'
#' @keywords internal
#' @noRd
.normalize_grid_reverse <- function(value, n, name) {
  
  value <- .normalize_grid_argument(
    value   = value,
    n       = n,
    default = FALSE,
    name    = name
  )
  
  if (!is.logical(value) || anyNA(value)) {
    stop(
      "'", name, "' must contain non-missing logical values.",
      call. = FALSE
    )
  }
  
  value
}


#' Construct coordinates for one grid line
#'
#' @param ntr Number of traces in the line.
#' @param fixed_coordinate Constant grid coordinate.
#' @param start Starting coordinate along the line.
#' @param reverse Whether to reverse the trace direction.
#' @param orientation Line orientation, either \code{"x"} for a line with
#'   constant x or \code{"y"} for a line with constant y.
#' @param end Optional ending coordinate. If supplied, positions are generated
#'   with \code{seq()}.
#' @param trace_positions Optional original trace-position vector.
#'
#' @return A numeric matrix with columns \code{x}, \code{y}, and \code{z}.
#'
#' @keywords internal
#' @noRd
.make_grid_line_coords <- function(
    ntr,
    fixed_coordinate,
    start,
    reverse,
    orientation = c("x", "y"),
    end = NULL,
    trace_positions = NULL
) {
  
  orientation <- match.arg(orientation)
  
  if (
    length(ntr) != 1L ||
    is.na(ntr) ||
    !is.finite(ntr) ||
    ntr < 0 ||
    ntr != floor(ntr)
  ) {
    stop(
      "'ntr' must be a single non-negative integer.",
      call. = FALSE
    )
  }
  
  ntr <- as.integer(ntr)
  
  if (!is.null(end)) {
    positions <- seq(
      from       = if (isTRUE(reverse)) end else start,
      to         = if (isTRUE(reverse)) start else end,
      length.out = ntr
    )
  } else {
    if (is.null(trace_positions)) {
      stop(
        "'trace_positions' must be supplied when no line endpoint is given.",
        call. = FALSE
      )
    }
    
    if (length(trace_positions) != ntr) {
      stop(
        "The number of stored trace positions (",
        length(trace_positions),
        ") does not match the number of traces (",
        ntr,
        ").",
        call. = FALSE
      )
    }
    
    positions <- if (isTRUE(reverse)) {
      rev(trace_positions)
    } else {
      trace_positions
    }
    
    positions <- positions + start
  }
  
  coords <- matrix(
    0,
    nrow = ntr,
    ncol = 3L,
    dimnames = list(NULL, c("x", "y", "z"))
  )
  
  if (identical(orientation, "x")) {
    coords[, "x"] <- fixed_coordinate
    coords[, "y"] <- positions
  } else {
    coords[, "x"] <- positions
    coords[, "y"] <- fixed_coordinate
  }
  
  coords
}


#' Set grid coordinates
#'
#' @rdname gridCoords
#' @export
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
    
    if (!is.list(value)) {
      stop(
        "'value' must be a named list.",
        call. = FALSE
      )
    }
    
    if (is.null(names(value)) || any(names(value) == "")) {
      stop(
        "'value' must be a named list.",
        call. = FALSE
      )
    }
    
    nlines <- length(x@nx)
    
    xlines <- .validate_grid_line_ids(
      ids    = value[["xlines"]],
      nlines = nlines,
      tag    = "xlines"
    )
    
    ylines <- .validate_grid_line_ids(
      ids    = value[["ylines"]],
      nlines = nlines,
      tag    = "ylines"
    )
    
    if (any(xlines %in% ylines)) {
      duplicated_ids <- intersect(xlines, ylines)
      
      stop(
        "Lines cannot occur in both 'xlines' and 'ylines': ",
        paste(duplicated_ids, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    
    nxlines <- length(xlines)
    nylines <- length(ylines)
    
    if (nxlines == 0L && nylines == 0L) {
      return(x)
    }
    
    # Normalize and validate x-line arguments.
    if (nxlines > 0L) {
      x_fixed <- .validate_required_grid_argument(
        value      = value[["x"]],
        n          = nxlines,
        name       = "x",
        lines_name = "xlines"
      )
      
      x_start <- .normalize_grid_argument(
        value   = value[["xstart"]],
        n       = nxlines,
        default = 0,
        name    = "xstart"
      )
      
      if (
        !is.numeric(x_start) ||
        anyNA(x_start) ||
        any(!is.finite(x_start))
      ) {
        stop(
          "'xstart' must contain finite, non-missing numeric values.",
          call. = FALSE
        )
      }
      
      x_reverse <- .normalize_grid_reverse(
        value = value[["xreverse"]],
        n     = nxlines,
        name  = "xreverse"
      )
      
      x_end <- .validate_optional_grid_argument(
        value      = value[["xlength"]],
        n          = nxlines,
        name       = "xlength",
        lines_name = "xlines"
      )
    }
    
    # Normalize and validate y-line arguments.
    if (nylines > 0L) {
      y_fixed <- .validate_required_grid_argument(
        value      = value[["y"]],
        n          = nylines,
        name       = "y",
        lines_name = "ylines"
      )
      
      y_start <- .normalize_grid_argument(
        value   = value[["ystart"]],
        n       = nylines,
        default = 0,
        name    = "ystart"
      )
      
      if (
        !is.numeric(y_start) ||
        anyNA(y_start) ||
        any(!is.finite(y_start))
      ) {
        stop(
          "'ystart' must contain finite, non-missing numeric values.",
          call. = FALSE
        )
      }
      
      y_reverse <- .normalize_grid_reverse(
        value = value[["yreverse"]],
        n     = nylines,
        name  = "yreverse"
      )
      
      y_end <- .validate_optional_grid_argument(
        value      = value[["ylength"]],
        n          = nylines,
        name       = "ylength",
        lines_name = "ylines"
      )
    }
    
    # Read the original trace positions at most once. They are only needed
    # when xlength or ylength is not supplied.
    needs_trace_positions <- (
      nxlines > 0L && is.null(x_end)
    ) || (
      nylines > 0L && is.null(y_end)
    )
    
    trace_positions <- NULL
    
    if (needs_trace_positions) {
      trace_positions <- .h5_line_read_all(x, "x")
    }
    
    # Lines with constant x and varying y.
    if (nxlines > 0L) {
      for (k in seq_along(xlines)) {
        line_id <- xlines[k]
        ntr <- x@nx[line_id]
        
        line_positions <- if (is.null(x_end)) {
          trace_positions[[line_id]]
        } else {
          NULL
        }
        
        line_end <- if (is.null(x_end)) {
          NULL
        } else {
          x_end[k]
        }
        
        x@coords[[line_id]] <- .make_grid_line_coords(
          ntr              = ntr,
          fixed_coordinate = x_fixed[k],
          start            = x_start[k],
          reverse          = x_reverse[k],
          orientation      = "x",
          end              = line_end,
          trace_positions  = line_positions
        )
      }
    }
    
    # Lines with varying x and constant y.
    if (nylines > 0L) {
      for (k in seq_along(ylines)) {
        line_id <- ylines[k]
        ntr <- x@nx[line_id]
        
        line_positions <- if (is.null(y_end)) {
          trace_positions[[line_id]]
        } else {
          NULL
        }
        
        line_end <- if (is.null(y_end)) {
          NULL
        } else {
          y_end[k]
        }
        
        x@coords[[line_id]] <- .make_grid_line_coords(
          ntr              = ntr,
          fixed_coordinate = y_fixed[k],
          start            = y_start[k],
          reverse          = y_reverse[k],
          orientation      = "y",
          end              = line_end,
          trace_positions  = line_positions
        )
      }
    }
    
    message("Grid computed, write to hdf5 file!")
    # Both xlines and ylines must be included. This corrects the previous
    # ylines-block bug where value$xlines was appended a second time.
    changed_ids <- c(xlines, ylines)
    
    .finalize_gridCoords_GPRsurvey(x = x, changed_ids = changed_ids)
  }
)


#' Persist changed grid coordinates
#'
#' Finalize a grid-coordinate update after all requested coordinate
#' assignments have been applied to the in-memory survey object.
#'
#' The function performs the following operations:
#'
#' \enumerate{
#'   \item Removes duplicated, missing, and invalid changed-line indices.
#'   \item Recomputes survey intersections once, after all coordinate changes
#'   are complete.
#'   \item Persists the changed coordinates and recomputed intersections in a
#'   single HDF5 update transaction.
#' }
#'
#' Updating coordinates and intersections together prevents the two datasets
#' from becoming inconsistent if an error occurs during persistence.
#'
#' @param x A \code{\linkS4class{GPRsurvey}} object whose in-memory
#'   coordinates have already been updated.
#' @param changed_ids Integer indices into \code{x@names} identifying the
#'   lines whose coordinates changed.
#'
#' @return The updated \code{\linkS4class{GPRsurvey}} object.
#'
#' @keywords internal
#' @noRd
.finalize_gridCoords_GPRsurvey <- function(x, changed_ids) {
  
  changed_ids <- unique(as.integer(changed_ids))
  changed_ids <- changed_ids[!is.na(changed_ids)]
  
  if (length(changed_ids) == 0L) {
    return(x)
  }
  
  if (isTRUE(x@view)) {
    message(
      "This GPRsurvey object is a view of an existing HDF5 file. ",
      "Coordinates were not persisted. Use materialize(x, dsn = ...) first ",
      "if you want an independent, writable survey."
    )
    
    return(x)
  }
  
  nlines <- length(x@nx)
  
  if (any(changed_ids < 1L | changed_ids > nlines)) {
    stop(
      "'changed_ids' contains invalid survey line indices.",
      call. = FALSE
    )
  }
  
  # Recompute intersections once, after every requested coordinate change
  # has been applied in memory.
  if (.hasSlot(x, "intersections")) {
    x@intersections <- list()
    x <- findIntersection(x)
  }
  
  # Write coordinates and intersections using one atomic HDF5 transaction.
  .h5_update_survey(x@path, function(h5) {
      .write_GPRsurvey_coords_hdf5(h5 = h5, obj = x, ids = changed_ids)
      .write_intersections_hdf5(h5 = h5, obj  = x)
    }
  )
  
  x
}