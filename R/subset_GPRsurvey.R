
# ============================================================================ #
# GPRsurvey extract/replace methods for HDF5-backed surveys
# ============================================================================ #
#
# Supports:
#   SU[[2]]  <- gpr
#   SU[2]    <- gpr
#   SU[1:3]  <- list(gpr1, gpr2, gpr3)
#   SU[1:10] <- SU2
#
# If x@view is TRUE, replacement is ignored with a message and x is returned
# unchanged. If value is a GPRsurvey, line groups are copied directly between
# HDF5 files using hdf5r's copy_to() method, without reading all GPR data
# into R.
#
# WRITE WORKFLOW (updated)
# -------------------------------------------------------------------------- #
# Every replace method below performs exactly ONE `.h5_update_survey()` (or,
# when copying lines in from a *different* backing file,
# `.h5_update_survey_with_source()`) transaction per call, regardless of how
# many lines are being replaced: a single lock, a single temporary copy of
# the backing file, a single open `hdf5r` handle under which every line is
# rewritten, one checksum-verification pass, and one atomic swap into place
# at the very end. Survey-level metadata and `@intersections` are
# recomputed and written exactly once, after all lines for this call have
# been written -- not once per line. The previous version of this file (and
# of `papply.R`, which shares these helpers) opened and closed the backing
# file separately for every single line replacement, which was both slower
# and not atomic (a crash partway through a multi-line replacement could
# leave the file with some lines updated and some not).
# ============================================================================ #


#----- extract [ ] ---- #

#' Extract and replace parts of a GPRsurvey object
#'
#' Subsetting a GPRsurvey returns a view. A view does not create a new HDF5
#' file. It remains backed by the original HDF5 file and is not modified in
#' place.
#'
#' Extract parts of a GPRsurvey object
#' @param x (`GPRsurvey`)
#' @param i (`integer`) Indices specifying elements to extract or replace.
#' @param j (`integer`) Not used.
#' @param ... Not used.
#' @param drop Not used.
#' @param exact Not used.
#' @return (`GPRsurvey`)
#' @aliases [,GPRsurvey-method
#' @rdname subset-GPRsurvey
#' @export
setMethod("[", signature(x = "GPRsurvey", i = "ANY", j = "ANY"),
          function(x, i, j, ..., drop = TRUE){
            if(missing(i)) i <- j
            y <- x
            y@paths          <- x@paths[i]
            y@names          <- x@names[i]
            y@descs          <- x@descs[i]
            y@modes          <- x@modes[i]
            
            y@dates          <- x@dates[i]
            y@freqs          <- x@freqs[i]
            y@antseps        <- x@antseps[i]
            
            if(length(x@spunit) == length(x@names)){
              y@spunit         <- x@spunit[i]
            }
            # FIXME
            if(length(x@crs) == 1){
              y@crs <- x@crs
            }else{
              y@crs <- x@crs[i]
            }
            y@coords         <- x@coords[i]
            y@markers        <- x@markers[i]
            y@intersections  <- list()
            y@nz             <- x@nz[i]
            y@zlengths       <- x@zlengths[i]
            y@zunits         <- x@zunits[i]
            y@nx             <- x@nx[i]
            y@xlengths       <- x@xlengths[i]
            y@transf         <- x@transf
            
            y@view           <- TRUE
            
            y <- findIntersection(y)
            
            return(y)
          })

#----- extract [[ ]] ---- #

#' @aliases [[,GPRsurvey-method
#' @rdname subset-GPRsurvey
#' @export
setMethod("[[", signature(x = "GPRsurvey", i = "ANY", j = "ANY"),
          function(x, i, j, ..., exact = TRUE){
            nm <- if (is.numeric(i)) x@names[[i]] else i
            if (!nm %in% x@names) {
              stop("Line '", nm, "' not found in this GPRsurvey.", call. = FALSE)
            }
            .read_GPR_line_hdf5(x@path, nm)
            
          })


#----- replace [ ] ---- #

#' @aliases [<-,GPR-method
#' @rdname subset-GPR
#' @export
setReplaceMethod(
  f = "[",
  signature = signature(x = "GPRsurvey", i = "ANY", j = "ANY", value = "ANY"),
  definition = function(x, i, j, ..., value) {
    if (isTRUE(x@view)) {
      message(
        "This GPRsurvey object is a view of an existing HDF5 file. ",
        "The replacement was not written to the HDF5 file, and the object ",
        "is returned unchanged. Use writeGPRsurvey(x, dsn = ...) first if ",
        "you want an independent, writable survey."
      )
      return(x)
    }
    
    ii <- .gprsurvey_index(x, i)
    
    # ------------------------------------------------------------------------ #
    # Case 1: replacement by another GPRsurvey
    # ------------------------------------------------------------------------ #
    if (inherits(value, "GPRsurvey")) {
      n_value <- length(value@names)
      
      if (n_value != length(ii)) {
        stop(
          "When 'value' is a GPRsurvey object, its length must match ",
          "the number of replacement indices.\n",
          "Length of 'i': ", length(ii), "\n",
          "Length of 'value': ", n_value,
          call. = FALSE
        )
      }
      
      same_file <- identical(
        normalizePath(x@path, mustWork = TRUE),
        normalizePath(value@path, mustWork = TRUE)
      )
      
      if (same_file) {
        x <- .h5_update_survey(x@path, function(h5) {
          .replace_lines_same_file_hdf5(h5, x, ii, value)
        })
      } else {
        x <- .h5_update_survey_with_source(x@path, value@path, function(h5, src_h5) {
          .replace_lines_cross_file_hdf5(h5, src_h5, x, ii, value)
        })
      }
      
      validObject(x)
      return(x)
    }
    
    # ------------------------------------------------------------------------ #
    # Case 2: replacement by one GPR object or a list of GPR objects
    # ------------------------------------------------------------------------ #
    if (inherits(value, "GPR")) {
      if (length(ii) != 1L) {
        stop(
          "A single GPR object can replace only one survey line. ",
          "For several lines, provide a list of GPR objects or a GPRsurvey ",
          "object of the same length as 'i'.",
          call. = FALSE
        )
      }
      value <- list(value)
    } else if (!is.list(value)) {
      stop(
        "'value' must be a GPR object, a list of GPR objects, ",
        "or a GPRsurvey object.",
        call. = FALSE
      )
    }
    
    ok <- vapply(value, inherits, logical(1L), what = "GPR")
    
    if (!all(ok)) {
      stop("All replacement elements must be of class 'GPR'.", call. = FALSE)
    }
    
    if (length(value) != length(ii)) {
      stop(
        "Number of replacement GPR objects does not match number of indices.\n",
        "Length of 'i': ", length(ii), "\n",
        "Length of 'value': ", length(value),
        call. = FALSE
      )
    }
    
    # Single transaction for the whole batch: one lock, one temp copy, one
    # open handle for every line, one survey-metadata/intersections write,
    # one checksum verification, one atomic swap.
    x <- .h5_update_survey(x@path, function(h5) {
      local_x <- x
      for (k in seq_along(ii)) {
        local_x <- .replace_one_GPRsurvey_line_hdf5(h5, local_x, ii[k], value[[k]])
      }
      .finalize_replace_write_hdf5(h5, local_x)
    })
    
    validObject(x)
    x
  }
)


#----- replace [[ ]] ---- #

#' @aliases [[<-,GPRsurvey-method
#' @rdname subset-GPRsurvey
#' @export
setReplaceMethod(
  f = "[[",
  signature = signature(x = "GPRsurvey", i = "ANY", j = "ANY", value = "GPR"),
  definition = function(x, i, j, ..., value) {
    if (isTRUE(x@view)) {
      message(
        "This GPRsurvey object is a view of an existing HDF5 file. ",
        "The replacement was not written to the HDF5 file, and the object ",
        "is returned unchanged. Use writeGPRsurvey(x, dsn = ...) first if ",
        "you want an independent, writable survey."
      )
      return(x)
    }
    
    ii <- .gprsurvey_index(x, i)
    
    if (length(ii) != 1L) {
      stop("[[<- can replace only one GPR line.", call. = FALSE)
    }
    
    x <- .h5_update_survey(x@path, function(h5) {
      local_x <- .replace_one_GPRsurvey_line_hdf5(h5, x, ii, value)
      .finalize_replace_write_hdf5(h5, local_x)
    })
    
    validObject(x)
    x
  }
)


# ------------------------------------------------------------------------- #
# Helpers
# ------------------------------------------------------------------------- #

#' Finish a line-replacement transaction: recompute intersections once and
#' write survey metadata + intersections once (internal)
#'
#' Called exactly once per `[<-`/`[[<-` call, after every line for that call
#' has already been (re)written under the same open `h5` handle -- mirrors
#' the "intersections computed at the end" pattern used in
#' `.finalize_gridCoords_GPRsurvey()` (see `gridCoords.R`).
#'
#' @param h5 Open, writable [hdf5r::H5File] handle (the temporary copy
#'   managed by `.h5_update_survey()`/`.h5_update_survey_with_source()`).
#' @param x (`GPRsurvey`) Survey with all in-memory line metadata for this
#'   call already applied.
#' @return The (possibly updated) `GPRsurvey` object.
#' @keywords internal
#' @noRd
.finalize_replace_write_hdf5 <- function(h5, x) {
  x@intersections <- list()
  x <- findIntersection(x)
  
  .write_survey_group_hdf5(h5, x)
  .write_intersections_hdf5(h5, x)
  
  x
}

#' Replace one survey line's data and in-memory metadata, given an already
#' open HDF5 handle (internal)
#'
#' Low-level building block used by both `[<-` (looped over indices) and
#' `[[<-` (a single call). Unlike the previous `.replace_one_GPRsurvey_line()`
#' (removed -- see `papply.R` for its other former caller, now updated to
#' use this function too), this does **not** open/close the HDF5 file
#' itself and does **not** recompute intersections or write survey
#' metadata; callers are expected to wrap one or more calls to this
#' function in a single `.h5_update_survey()` transaction and call
#' `.finalize_replace_write_hdf5()` once at the end.
#'
#' @param h5 Open, writable [hdf5r::H5File] handle.
#' @param x (`GPRsurvey`) Survey to update.
#' @param i (`integer(1)`) Index (into `x@names`) of the line to replace.
#' @param value (`GPR`) Replacement line.
#' @param compress (`integer(1)`) gzip level for the rewritten line.
#' @return The updated `GPRsurvey` object (HDF5 line already written).
#' @keywords internal
#' @noRd
.replace_one_GPRsurvey_line_hdf5 <- function(h5, x, i, value, compress = 5L) {
  if (!inherits(value, "GPR")) {
    stop("'value' must be of class 'GPR'.", call. = FALSE)
  }
  
  old_name <- x@names[i]
  
  new_name <- value@name[1L]
  if (length(new_name) == 0L || !nzchar(new_name)) {
    new_name <- "default_name"
  }
  new_name <- safeName(x = new_name, y = x@names[-i])
  value@name <- new_name
  
  meta <- .gprsurvey_line_metadata(value)
  
  # ---- update in-memory metadata --------------------------------------------
  x@names[i]    <- new_name
  x@descs[i]    <- meta$desc
  x@modes[i]    <- meta$mode
  x@dates[i]    <- meta$date
  x@freqs[i]    <- meta$freq
  x@antseps[i]  <- meta$antsep
  x@nz[i]       <- meta$nz
  x@nx[i]       <- meta$nx
  x@zlengths[i] <- meta$zlength
  x@xlengths[i] <- meta$xlength
  x@zunits[i]   <- meta$zunit
  
  if (length(x@spunit) == length(x@names)) {
    x@spunit[i] <- meta$spunit
  }
  
  if (length(x@crs) == length(x@names)) {
    x@crs[i] <- meta$crs
  } else if (
    length(x@crs) == 0L ||
    is.na(x@crs[1L]) ||
    !nzchar(x@crs[1L])
  ) {
    x@crs <- meta$crs
  }
  
  if (length(meta$coord) > 0L) {
    if (nrow(meta$coord) != ncol(value) || ncol(meta$coord) != 3L) {
      stop("Coordinates are not correct.", call. = FALSE)
    }
    
    if (length(x@coords) == 0L) {
      x@coords <- vector("list", length(x@names))
      names(x@coords) <- x@names
    }
    
    x@coords[[i]] <- meta$coord
    names(x@coords)[i] <- new_name
    
  } else if (length(x@coords) > 0L) {
    x@coords[[i]] <- list()
    names(x@coords)[i] <- new_name
    
    if (all(vapply(x@coords, length, integer(1L)) == 0L)) {
      x@coords <- list()
    }
  }
  
  if (length(x@markers) > 0L) {
    names(x@markers)[i] <- new_name
  }
  
  # ---- update HDF5 line, under the caller's open handle ----------------------
  if (!"lines" %in% names(h5)) h5$create_group("lines")
  lg <- h5[["lines"]]
  
  .delete_h5_link_if_exists(lg, old_name)
  .write_GPR_line_hdf5(lg, name = new_name, gpr = value, compress = compress)
  
  x
}

#' Replace several survey lines with lines copied from a *different* survey
#' backed by the SAME HDF5 file (internal)
#'
#' Only used from `[<-` when `x@path` and `value@path` point at the same
#' file. Line groups are copied to temporary names first (protecting
#' against self-overlapping replacements such as `SU[1:2] <- SU[2:1]`,
#' where the destination of one replacement is the source of another),
#' then moved into their final names, then the temporary names are removed.
#' Everything happens under the single `h5` handle supplied by
#' `.h5_update_survey()`.
#'
#' @param h5 Open, writable [hdf5r::H5File] handle for the (shared)
#'   temporary copy of the backing file.
#' @param x (`GPRsurvey`) Destination survey.
#' @param ii (`integer`) Destination indices (into `x@names`).
#' @param value (`GPRsurvey`) Source survey (same backing file as `x`).
#' @return The updated `GPRsurvey` object.
#' @keywords internal
#' @noRd
.replace_lines_same_file_hdf5 <- function(h5, x, ii, value) {
  tmp_names <- paste0(
    ".RGPR_tmp_replace_",
    format(Sys.time(), "%Y%m%d%H%M%OS6"),
    "_",
    seq_along(ii)
  )
  
  for (k in seq_along(ii)) {
    .copy_h5_line_group(
      src_h5 = h5, dst_h5 = h5,
      src_name = value@names[k], dst_name = tmp_names[k]
    )
  }
  
  for (k in seq_along(ii)) {
    old_name <- x@names[ii[k]]
    
    new_name <- value@names[k]
    if (length(new_name) == 0L || !nzchar(new_name)) {
      new_name <- "default_name"
    }
    new_name <- safeName(x = new_name, y = x@names[-ii[k]])
    
    if ("lines" %in% names(h5)) {
      .delete_h5_link_if_exists(h5[["lines"]], old_name)
    }
    
    .copy_h5_line_group(
      src_h5 = h5, dst_h5 = h5,
      src_name = tmp_names[k], dst_name = new_name
    )
    
    x <- .replace_one_GPRsurvey_line_from_survey(
      x = x, i = ii[k], value = value, k = k, dst_name = new_name
    )
  }
  
  lg <- h5[["lines"]]
  for (nm in tmp_names) {
    .delete_h5_link_if_exists(lg, nm)
  }
  
  .finalize_replace_write_hdf5(h5, x)
}

#' Replace several survey lines with lines copied from a *different* survey
#' backed by a DIFFERENT HDF5 file (internal)
#'
#' Used from `[<-` when `x@path` and `value@path` point at different files.
#' No self-overlap protection is needed here (source and destination are
#' different files), so lines are copied directly to their final names.
#'
#' @param h5 Open, writable handle for the temporary copy of `x@path`.
#' @param src_h5 Open, read-only handle for `value@path`.
#' @param x (`GPRsurvey`) Destination survey.
#' @param ii (`integer`) Destination indices (into `x@names`).
#' @param value (`GPRsurvey`) Source survey (different backing file).
#' @return The updated `GPRsurvey` object.
#' @keywords internal
#' @noRd
.replace_lines_cross_file_hdf5 <- function(h5, src_h5, x, ii, value) {
  for (k in seq_along(ii)) {
    old_name <- x@names[ii[k]]
    
    new_name <- value@names[k]
    if (length(new_name) == 0L || !nzchar(new_name)) {
      new_name <- "default_name"
    }
    new_name <- safeName(x = new_name, y = x@names[-ii[k]])
    
    if ("lines" %in% names(h5)) {
      .delete_h5_link_if_exists(h5[["lines"]], old_name)
    }
    
    .copy_h5_line_group(
      src_h5 = src_h5, dst_h5 = h5,
      src_name = value@names[k], dst_name = new_name
    )
    
    x <- .replace_one_GPRsurvey_line_from_survey(
      x = x, i = ii[k], value = value, k = k, dst_name = new_name
    )
  }
  
  .finalize_replace_write_hdf5(h5, x)
}


# Update one destination line's in-memory metadata from a source GPRsurvey line.
# This does not read the GPR data matrix and performs no HDF5 I/O; the HDF5
# line group is copied by the caller (.replace_lines_same_file_hdf5() /
# .replace_lines_cross_file_hdf5()).
.replace_one_GPRsurvey_line_from_survey <- function(x, i, value, k, dst_name) {
  new_name <- dst_name
  
  x@names[i]    <- new_name
  x@descs[i]    <- value@descs[k]
  x@modes[i]    <- value@modes[k]
  x@dates[i]    <- value@dates[k]
  x@freqs[i]    <- value@freqs[k]
  x@antseps[i]  <- value@antseps[k]
  x@nz[i]       <- value@nz[k]
  x@nx[i]       <- value@nx[k]
  x@zlengths[i] <- value@zlengths[k]
  x@xlengths[i] <- value@xlengths[k]
  x@zunits[i]   <- value@zunits[k]
  
  if (length(x@spunit) == length(x@names)) {
    if (length(value@spunit) == length(value@names)) {
      x@spunit[i] <- value@spunit[k]
    } else if (length(value@spunit) > 0L) {
      message("took the first spatial unit found!")
      x@spunit[i] <- value@spunit[1L]
    }
  }
  
  if (length(x@crs) == length(x@names)) {
    if (length(value@crs) == length(value@names)) {
      x@crs[i] <- value@crs[k]
    } else if (length(value@crs) > 0L) {
      message("took the first CRS found!")
      x@crs[i] <- value@crs[1L]
    }
  }
  
    
  if (length(value@coords) > 0L) {
    if (length(x@coords) == 0L) {
      x@coords <- vector("list", length(x@names))
      names(x@coords) <- x@names
    }
    
    x@coords[[i]] <- value@coords[[k]]
    names(x@coords)[i] <- new_name
  } else if (length(x@coords) > 0L) {
    x@coords[[i]] <- list()
    names(x@coords)[i] <- new_name
  }
  
  if (length(value@markers) > 0L) {
    if (length(x@markers) == 0L) {
      x@markers <- vector("list", length(x@names))
      names(x@markers) <- x@names
    }
    
    x@markers[[i]] <- value@markers[[k]]
    names(x@markers)[i] <- new_name
  } else if (length(x@markers) > 0L) {
    names(x@markers)[i] <- new_name
  }
  
  x
}


# Resolve replacement indices
.gprsurvey_index <- function(x, i) {
  if (missing(i)) {
    stop("Missing index.", call. = FALSE)
  }
  if (is.character(i)) {
    ii <- match(i, x@names)
    if (anyNA(ii)) {
      stop(
        "Line(s) not found in this GPRsurvey: ",
        paste(i[is.na(ii)], collapse = ", "),
        call. = FALSE
      )
    }
    return(ii)
  }
  ii <- as.integer(i)
  if (anyNA(ii) || any(ii < 1L) || any(ii > length(x@names))) {
    stop("Index out of bounds.", call. = FALSE)
  }
  ii
}

.gprsurvey_line_metadata <- function(gpr) {
  zlength <- if (length(gpr@z) > 1L) {
    abs(diff(range(gpr@z, na.rm = TRUE)))
  } else {
    0
  }
  
  xlength <- if (length(gpr@x) > 1L) {
    abs(diff(range(gpr@x, na.rm = TRUE)))
  } else {
    0
  }
  
  list(
    desc    = gpr@desc,
    mode    = gpr@mode,
    date    = .setSlotDefault(gpr, "date",   Sys.Date(), verbose = FALSE),
    freq    = .setSlotDefault(gpr, "freq",   0,          verbose = FALSE),
    antsep  = .setSlotDefault(gpr, "antsep", 0,          verbose = FALSE),
    spunit  = gpr@xunit,
    crs     = gpr@crs,
    coord   = gpr@coord,
    nz      = nrow(gpr),
    nx      = ncol(gpr),
    zlength = zlength,
    xlength = xlength,
    zunit   = gpr@zunit
  )
}
