# =============================================================================
# HDF5 write helper for writeGPR("GPRsurvey")   (internal)
# =============================================================================

#' HDF5 write helper for `writeGPR("GPRsurvey")`
#'
#' Three cases:
#'
#' 1. `dsn` is `NULL` or identical to `obj@path`: the file is already
#'    current -- no-op, return `obj`.
#' 2. `dsn` is a different path: copy the backing HDF5 file there and
#'    return an updated `obj` pointing at the new location.
#' 3. `obj@path` no longer exists: raise an informative error asking the
#'    user to re-create with `GPRsurvey()`.
#'
#' The copy in case 2 uses the same lock + temporary-file + checksum-verify
#' + atomic-replace pattern as `.h5_update_survey()` (see `hdf5_update.R`),
#' so an interrupted copy never leaves a partial/corrupt file at `dst`, and
#' a concurrent writer to the *source* file is guarded against with a lock.
#'
#' @param obj Object of class `GPRsurvey`.
#' @param dsn (`character(1)` or `NULL`) Destination `.h5` path.
#' @param overwrite (`logical(1)`) Overwrite an existing `dst`?
#' @param compress Unused here (compression is fixed by whatever the
#'   source file already contains) -- kept for a consistent call signature
#'   with the rest of the `writeGPR()` dispatch.
#' @keywords internal
.writeGPR_h5 <- function(obj, dsn, overwrite, compress) {

  src <- obj@path

  # ---- Case 1: nothing to do -------------------------------------------------
  if (is.null(dsn) || identical(normalizePath(dsn, mustWork = FALSE),
                                 normalizePath(src, mustWork = FALSE))) {
    if (!file.exists(src)) {
      stop("The backing HDF5 file no longer exists: '", src, "'.\n",
           "Re-create the GPRsurvey object with GPRsurvey().",
           call. = FALSE)
    }
    message("HDF5 file is already up to date: '", src, "'")
    return(invisible(obj))
  }

  # ---- Case 3: source missing -------------------------------------------------
  if (!file.exists(src)) {
    stop("The backing HDF5 file no longer exists: '", src, "'.\n",
         "Re-create the GPRsurvey object with GPRsurvey().",
         call. = FALSE)
  }

  # ---- Case 2: copy to a new location, safely --------------------------------
  dst <- normalizePath(dsn, mustWork = FALSE)
  if (file.exists(dst) && !overwrite) {
    stop("File already exists: '", dst, "'.\n",
         "Use overwrite = TRUE to replace it.",
         call. = FALSE)
  }

  # Guard the *source* file: we are about to read it wholesale, and don't
  # want another process rewriting it out from under us mid-copy.
  lock <- .h5_lock_acquire(src)
  on.exit(.h5_lock_release(lock), add = TRUE)

  tmp <- .h5_temp_path(dst)
  on.exit(unlink(tmp, force = TRUE), add = TRUE)

  if (!file.copy(src, tmp)) {
    stop("Failed to copy '", src, "' to a temporary file.", call. = FALSE)
  }
  .h5_verify_checksums(tmp)
  .h5_atomic_replace(tmp, dst)

  message("GPRsurvey HDF5 file copied to: '", dst, "'")
  obj@path <- dst
  invisible(obj)
}


# =============================================================================
# HDF5 write logic for a single GPR line  (internal)
# =============================================================================
#
# The generic building blocks (typed writers, checksums, temp-file/atomic
# replace, locking) live in `hdf5_update.R`. This file only contains the
# logic specific to serializing one `GPR` object into `/lines/<name>` of an
# already-open HDF5 file/group.
# =============================================================================


#' Write a single GPR line into an open HDF5 group
#'
#' Creates `parent_grp/<name>` and fills it with everything needed to fully
#' reconstruct the corresponding `GPR` object later with
#' `.read_GPR_line_hdf5()`:
#'
#' ```
#' <name>/
#'   (attrs: name, date, freq, antsep, mode, crs, dunit, xunit, zunit,
#'           version, desc, spunit)
#'   data            -- nz x nx, 64-bit float, chunked, checksummed,
#'                       optionally gzip-compressed
#'   z, x, z0        -- axes
#'   markers         -- character[nx], trimmed and padded/truncated to
#'                       exactly nx elements (see `.normalizeMarkers()`)
#'   coords/xyz      -- trace coordinates (if any)
#'   coords/rec      -- receiver coordinates (if any)
#'   coords/trans    -- transmitter coordinates (if any)
#'   vel/v           -- velocity model (if any)
#'   metadata/...    -- flattened, human-browsable copy of scalar @md entries
#'   metadata_raw    -- the *entire* @md list, losslessly serialized (see
#'                       `.h5_write_r_object()`); this is what is actually
#'                       used to restore @md on read
#' ```
#'
#' @param parent_grp An open `hdf5r` group object (the `/lines` group).
#' @param name (`character(1)`) Name for the new sub-group.
#' @param gpr Object of class `GPR`.
#' @param compress (`integer(1)`) gzip level 0-9 for the main data array
#'   (and, if large, the coordinate matrix). `0` disables compression.
#'
#' @return Invisibly returns the created group object.
#' @keywords internal
.write_GPR_line_hdf5 <- function(parent_grp, name, gpr, compress = 0L) {

  nz <- nrow(gpr)
  nx <- ncol(gpr)

  # Fail fast and clearly rather than creating a malformed/partial group for
  # an empty profile -- see `.h5_write_data_array()` for the same guard.
  if (nz == 0L || nx == 0L) {
    stop(
      "GPR line '", name, "' is empty (nz = ", nz, ", nx = ", nx, "); ",
      "cannot be written to HDF5.",
      call. = FALSE
    )
  }

  grp <- parent_grp$create_group(name)

  # ---- Scalar attributes (light metadata -- no dataset overhead) -----------
  grp$create_attr("name",    gpr@name)
  grp$create_attr("date",    format(gpr@date, "%Y-%m-%d"))
  grp$create_attr("freq",    gpr@freq[1L])
  grp$create_attr("antsep",  if (length(gpr@antsep) == 1L) gpr@antsep else gpr@antsep[1L])
  grp$create_attr("mode",    gpr@mode)
  grp$create_attr("crs",     if (is.na(gpr@crs)) "" else gpr@crs)
  grp$create_attr("dunit",   gpr@dunit)
  grp$create_attr("xunit",   gpr@xunit)
  grp$create_attr("zunit",   gpr@zunit)
  grp$create_attr("version", gpr@version)
  grp$create_attr("desc",    gpr@desc)
  grp$create_attr("spunit",  gpr@spunit)

  # ---- Data array (64-bit, chunked, checksummed, optionally compressed) ----
  .h5_write_data_array(grp, gpr, compress = compress)

  # ---- Axes -------------------------------------------------------------
  .h5_write_vector(grp, "z",  gpr@z)
  .h5_write_vector(grp, "x",  gpr@x)
  .h5_write_vector(grp, "z0", gpr@z0)

  # ---- Markers ------------------------------------------------------------
  # Always exactly `nx` elements, trimmed with trimStr(). Using the same
  # helper here as in GPRsurvey.R keeps /lines/<name>/markers and the
  # survey-level @markers list in agreement.
  .h5_write_vector(grp, "markers", .normalizeMarkers(gpr@markers, nx))

  # ---- Coordinates ----------------------------------------------------------
  cg <- grp$create_group("coords")
  if (!is.null(gpr@coord) && length(gpr@coord) > 0L && nrow(gpr@coord) > 0L) {
    .h5_write_matrix(cg, "xyz", gpr@coord, compress = compress)
  }
  if (length(gpr@rec)   > 0L) .h5_write_matrix(cg, "rec",   gpr@rec)
  if (length(gpr@trans) > 0L) .h5_write_matrix(cg, "trans", gpr@trans)

  # ---- Velocity model -------------------------------------------------------
  vg <- grp$create_group("vel")
  if (!is.null(gpr@vel$v)) .h5_write_vector(vg, "v", gpr@vel$v)

  # ---- Raw manufacturer metadata (`@md`) ------------------------------------
  # Stored twice, for two different purposes:
  #
  #  1) `metadata/<key>` -- one dataset per *scalar* entry of `@md`, purely
  #     for convenience: this is what shows up if you browse the file with
  #     an external HDF5 tool (h5dump, HDFView, h5py, ...). Non-scalar
  #     entries (vectors, lists, NULL, ...) are simply not represented here.
  #
  #  2) `metadata_raw` -- the entire `@md` list, serialized losslessly with
  #     `.h5_write_r_object()`. This is the one `.read_GPR_line_hdf5()`
  #     actually restores `@md` from, so nothing in `@md` is lost on a
  #     round trip, regardless of its structure.
  mg <- grp$create_group("metadata")
  for (key in names(gpr@md)) {
    val <- gpr@md[[key]]
    if (is.atomic(val) && length(val) == 1L) {
      mg[[key]] <- if (is.na(val)) "NA" else val
    }
  }
  if (length(gpr@md) > 0L) {
    .h5_write_r_object(grp, "metadata_raw", gpr@md)
  }

  invisible(grp)
}
