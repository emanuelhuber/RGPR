# =============================================================================
# readGPRsurvey()  -  reconstruct a GPRsurvey from an existing HDF5 file
# =============================================================================

#' Read a GPRsurvey object from an HDF5 file
#'
#' Reconstructs the `GPRsurvey` index from the `/survey` group of an RGPR
#' HDF5 file. The big per-line radar-data arrays are **not** loaded until a
#' line is accessed (via `x[[name]]` / `getGPR()`); only the small,
#' per-line metadata (coordinates, markers) is read eagerly here, since
#' those are needed for e.g. `gridCoords()`/`findIntersection()` to work
#' correctly on a freshly-read survey.
#'
#' Every slot that `GPRsurvey()` sets is restored here, so
#' `readGPRsurvey(dsn)` after `GPRsurvey(x, dsn = dsn)` yields an object
#' equivalent to what `GPRsurvey()` returned directly (modulo `@view`,
#' which is always `FALSE` for a survey read directly from disk -- see
#' `.write_survey_group_hdf5()` for why `@view` is not itself persisted).
#'
#' @param file (`character(1)`) Path to the `.h5` file previously written by
#'             [RGPR::GPRsurvey()] or `writeGPR(..., format = "h5")`.
#'
#' @return Object of class `GPRsurvey`.
#'
#' @seealso [RGPR::GPRsurvey()], [RGPR::writeGPR()]
#' @export
readGPRsurvey <- function(file) {

  if (!requireNamespace("hdf5r", quietly = TRUE)) {
    stop("Package 'hdf5r' is required to read a GPRsurvey HDF5 file.\n",
         "Install it with: install.packages('hdf5r')",
         call. = FALSE)
  }

  file <- normalizePath(file, mustWork = TRUE)
  h5   <- hdf5r::H5File$new(file, mode = "r")
  on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)

  s <- .read_survey_group_hdf5(h5)
  n <- length(s$names)

  # ---- per-line coordinates and markers --------------------------------------
  # Read directly from each line's (small) /lines/<name>/coords and
  # /lines/<name>/markers datasets -- NOT from the big "data" array, which
  # stays untouched until the line is actually requested via `[[`/getGPR().
  coords_  <- vector("list", n)
  markers_ <- vector("list", n)
  names(coords_)  <- s$names
  names(markers_) <- s$names

  if ("lines" %in% names(h5)) {
    lg <- h5[["lines"]]
    for (i in seq_len(n)) {
      nm <- s$names[i]
      if (!nm %in% names(lg)) next
      grp <- lg[[nm]]

      if ("coords" %in% names(grp) && "xyz" %in% names(grp[["coords"]])) {
        coord <- grp[["coords"]][["xyz"]][]
        if (is.matrix(coord) && ncol(coord) == 3L) {
          colnames(coord) <- c("x", "y", "z")
        }
        coords_[[i]] <- coord
      }
      if ("markers" %in% names(grp)) {
        markers_[[i]] <- grp[["markers"]][]
      }
    }
  }

  new("GPRsurvey",
      version       = s$version,
      name          = tryCatch(h5$attr_open("name")$read(), error = function(e) ""),
      desc          = tryCatch(h5$attr_open("desc")$read(), error = function(e) ""),
      path          = file,
      paths         = s$paths,
      names         = s$names,
      descs         = s$descs,
      modes         = s$modes,
      dates         = s$dates,
      freqs         = s$freqs,
      antseps       = s$antseps,
      spunit        = s$spunit,
      crs           = s$crs,
      coords        = coords_,
      intersections = .read_intersections_hdf5(h5),
      markers       = markers_,
      nz            = s$nz,
      nx            = s$nx,
      zlengths      = s$zlengths,
      xlengths      = s$xlengths,
      zunits        = s$zunits,
      transf        = s$transf,
      view          = FALSE   # a survey read directly from disk always fully
                               # owns (and is independently writable to)
                               # this backing file -- it is never a "view"
  )
}


# =============================================================================
# Internal helpers
# =============================================================================

#' Read a single GPR line from an HDF5 file
#'
#' @param dsn (`character(1)`) Path to the HDF5 file.
#' @param name (`character(1)`) Line name (sub-group under `/lines/`).
#'
#' @return Object of class `GPR`.
#' @keywords internal
.read_GPR_line_hdf5 <- function(dsn, name) {

  h5 <- hdf5r::H5File$new(dsn, mode = "r")
  on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)

  if (!h5$exists(file.path("lines", name))) {
    stop("Line '", name, "' not found in '", dsn, "'.", call. = FALSE)
  }

  grp <- h5[["lines"]][[name]]
  nz  <- grp[["data"]]$dims[1L]
  nx  <- grp[["data"]]$dims[2L]

  # -- helper: read attribute, return default if absent -----------------------
  .attr <- function(grp, key, default = "") {
    tryCatch(grp$attr_open(key)$read(), error = function(e) default)
  }

  crs_str <- .attr(grp, "crs", "")
  crs_val <- if (nzchar(crs_str)) crs_str else NA_character_

  # -- coordinates (optional groups) -------------------------------------------
  coord <- matrix(numeric(0), nrow = 0L, ncol = 3L)
  rec   <- coord
  trans <- coord
  if (grp$exists("coords")) {
    cg <- grp[["coords"]]
    if (cg$exists("xyz"))   coord <- cg[["xyz"]]$read()
    if (cg$exists("rec"))   rec   <- cg[["rec"]]$read()
    if (cg$exists("trans")) trans <- cg[["trans"]]$read()
  }

  # -- velocity model -----------------------------------------------------------
  vel <- list(v = NULL)
  if (grp$exists("vel") && grp[["vel"]]$exists("v")) {
    vel$v <- grp[["vel"]][["v"]]$read()
  }

  # -- raw metadata (`@md`) -----------------------------------------------------
  # Prefer the lossless serialized copy (`metadata_raw`, see
  # `.h5_write_r_object()` in hdf5_update.R); fall back to the flattened,
  # scalars-only `metadata` group for files written before `metadata_raw`
  # existed.
  md <- list()
  if (grp$exists("metadata_raw")) {
    md <- tryCatch(.h5_read_r_object(grp, "metadata_raw"), error = function(e) list())
  }
  if (length(md) == 0L && grp$exists("metadata")) {
    mg   <- grp[["metadata"]]
    keys <- names(mg)
    for (key in keys) {
      val       <- mg[[key]][]
      md[[key]] <- if (identical(val, "NA")) NA else val
    }
  }

  new("GPR",
      version  = .attr(grp, "version", "0.3"),
      name     = .attr(grp, "name",    ""),
      path     = dsn,
      desc     = .attr(grp, "desc",    ""),
      mode     = .attr(grp, "mode",    "CO"),
      date     = as.Date(.attr(grp, "date", format(Sys.Date(), "%Y-%m-%d"))),
      freq     = as.numeric(.attr(grp, "freq",   0)),
      antsep   = as.numeric(.attr(grp, "antsep", 0)),
      crs      = crs_val,
      dunit    = .attr(grp, "dunit",  "mV"),
      xunit    = .attr(grp, "xunit",  "m"),
      zunit    = .attr(grp, "zunit",  "ns"),
      spunit   = .attr(grp, "spunit", ""),
      data     = grp[["data"]][1:nz, 1:nx],
      z        = grp[["z"]][],
      x        = grp[["x"]][],
      z0       = grp[["z0"]][],
      markers  = grp[["markers"]][],
      coord    = coord,
      rec      = rec,
      trans    = trans,
      vel      = vel,
      md       = md
  )
}
