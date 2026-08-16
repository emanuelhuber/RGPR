# ============================================================================ #
# HDF5 backing writer for GPRcube (internal) -- interpSlices() Phase 2
# ============================================================================ #
#
# Streams a depth-interpolated cube straight to a chunked, checksummed HDF5
# file instead of accumulating it as an in-memory R array. Mirrors the
# conventions already used for GPRsurvey backing files (see hdf5_update.R):
#
#   - write to a temp file in the *same directory* as the destination,
#   - every dataset chunked + fletcher32-checksummed (gzip optional),
#   - verify checksums by reading the temp file back before it goes live,
#   - atomic file.rename() into place.
#
# Unlike GPRsurvey's ".h5_update_survey()" this always creates a brand-new
# file (there is no existing cube to merge into), so there's no need for
# the lock-then-copy-then-mutate dance used when *updating* an existing
# backing file -- we just build the temp file directly.
#
# File layout:
#   /cube/z    dataset [nx, ny, nz]  double, chunked (nx, ny, 1)
#   /cube/x    dataset [nx]          grid x-coordinates
#   /cube/y    dataset [ny]          grid y-coordinates
#   /cube/vz   dataset [nz]          depth/time vector
#   /cube/x0   dataset [n]           original observation x-coordinates
#   /cube/y0   dataset [n]           original observation y-coordinates
#   /cube/z0   dataset [nz, n]       resampled traces at all target depths
#   attributes on /cube: nx, ny, nz
# ============================================================================ #

#' Stream an interpolated depth cube to a new HDF5 file
#'
#' Computes depth slices in memory-bounded batches (same batching logic as
#' the in-memory path, see `.computeSlicesBatched()`), writing each batch
#' directly into the `/cube/z` dataset as soon as it's computed rather than
#' accumulating the whole cube in R. A single process performs all writes
#' (the parallel workers only ever return slice data; they never open the
#' HDF5 file themselves), so this is safe under any `future::plan()`
#' without needing HDF5's SWMR mode.
#'
#' @param xypos (`matrix[n,2]`) Observation coordinates
#' @param V (`matrix[nz,n]`) Resampled trace values at all target depths
#' @param vz Target depth/time vector
#' @param bbox_params,mba_params,clip_mask,h,gx,gy See `.sliceInterp()`
#' @param dsn (`character[1]|NULL`) Destination `.h5` path; a temp file if
#'   `NULL`.
#' @param compress (`integer[1]`) gzip level 0-9 for `/cube/z`; `0`
#'   disables compression. Coordinate/metadata datasets are always written
#'   uncompressed (too small to matter).
#' @param overwrite (`logical[1]`) Overwrite `dsn` if it exists?
#' @param batch_size (`integer[1]|NULL`) See `.computeSlicesBatched()`.
#' @param verbose (`logical[1]`)
#' @return (`character[1]`) The final path of the HDF5 file (`dsn`, or the
#'   generated temp path if `dsn = NULL`), invisibly.
#' @keywords internal
#' @noRd
.writeCubeHDF5 <- function(xypos, V, vz, bbox_params, mba_params, clip_mask,
                           gx, gy, h, dsn = NULL, compress = 5L,
                           overwrite = FALSE, batch_size = NULL,
                           verbose = TRUE) {

  if (!requireNamespace("hdf5r", quietly = TRUE)) {
    stop("Package 'hdf5r' is required to write an HDF5-backed GPRcube.\n",
         "Install it with: install.packages('hdf5r')", call. = FALSE)
  }

  compress <- as.integer(compress)
  if (length(compress) != 1L || is.na(compress) || compress < 0L || compress > 9L) {
    stop("'compress' must be an integer between 0 and 9.", call. = FALSE)
  }

  nx <- bbox_params$nx
  ny <- bbox_params$ny
  nz <- length(vz)

  if (is.null(dsn)) {
    dsn <- tempfile(pattern = "GPRcube_", fileext = ".h5")
    if (verbose) {
      message("No 'dsn' given: writing to a temporary file:\n  ", dsn,
              "\nUse writeGPR() to copy it somewhere permanent.")
    }
  } else {
    dsn <- path.expand(dsn)
    if (file.exists(dsn) && !overwrite) {
      stop("File already exists: '", dsn, "'.\n",
           "Use overwrite = TRUE to replace it.", call. = FALSE)
    }
  }
  dir.create(dirname(dsn), recursive = TRUE, showWarnings = FALSE)

  tmp <- .h5_temp_path(dsn)
  on.exit(unlink(tmp, force = TRUE), add = TRUE)

  h5 <- hdf5r::H5File$new(tmp, mode = "w")
  h5_open <- TRUE
  on.exit(if (h5_open) try(h5$close_all(), silent = TRUE), add = TRUE)

  grp <- h5$create_group("cube")

  # ---- main data dataset: chunked one-slice-per-chunk (matches both the
  # write pattern here -- one batch of whole slices at a time -- and the
  # typical read pattern of pulling out single horizontal slices later) ---
  dcpl <- hdf5r::H5P_DATASET_CREATE$new()
  dcpl$set_chunk(c(nx, ny, 1L))
  if (compress > 0L) {
    dcpl$set_shuffle()
    dcpl$set_deflate(compress)
  }
  dcpl$set_fletcher32()

  ds_z <- grp$create_dataset(
    name              = "z",
    dtype             = hdf5r::h5types$H5T_NATIVE_DOUBLE,
    space             = hdf5r::H5S$new(dims = c(nx, ny, nz)),
    dataset_create_pl = dcpl
  )

  # ---- small coordinate/metadata datasets (checksummed, uncompressed) ----
  .h5_write_vector(grp, "x",  gx)
  .h5_write_vector(grp, "y",  gy)
  .h5_write_vector(grp, "vz", vz)
  .h5_write_vector(grp, "x0", xypos[, 1])
  .h5_write_vector(grp, "y0", xypos[, 2])
  .h5_write_matrix(grp, "z0", V, compress = compress)

  hdf5r::h5attr(grp, "nx") <- nx
  hdf5r::h5attr(grp, "ny") <- ny
  hdf5r::h5attr(grp, "nz") <- nz

  # ---- stream depth slices in batches, writing each batch as it's ready --
  # Each batch's rows are extracted from V fresh, right before that batch's
  # future_lapply() call, and discarded right after -- unlike splitting all
  # of V into a persistent row-list up front, peak extra memory here is
  # bounded to one batch's rows, not a full second copy of V held for the
  # whole streaming loop. The extracted subset is passed directly as
  # future_lapply()'s X argument (not referenced by name from inside FUN),
  # so only that batch's rows are ever serialized to workers.
  if (is.null(batch_size)) {
    slice_mb   <- nx * ny * 8 / 1024^2
    batch_size <- max(1L, min(nz, floor(300 / max(slice_mb, 1e-6))))
  }
  batches <- split(seq_len(nz), ceiling(seq_len(nz) / batch_size))
  xy      <- xypos[, 1:2]   # see .computeSlicesBatched() for why this matters

  for (b in batches) {
    values_b <- lapply(b, function(j) V[j, , drop = TRUE])
    
    slices_b <- future.apply::future_lapply(
      values_b,   # this batch's rows only, passed as X
      function(values) {
        S <- .interpolateSlice(xy, values, bbox_params, h,
                               mba_params$m, mba_params$n)
        if (!is.null(clip_mask)) S$z[clip_mask] <- NA_real_
        S$z
      },
      future.packages = "MBA",
      future.seed = TRUE
    )
    ds_z[1:nx, 1:ny, b] <- simplify2array(slices_b)
    rm(values_b, slices_b)
    if (verbose) {
      message(sprintf("  slices %d-%d / %d written to HDF5", min(b), max(b), nz))
    }
  }

  h5$close_all()
  h5_open <- FALSE

  # Reading every dataset back forces HDF5 to validate the fletcher32
  # checksums written above, catching a corrupted write before it's ever
  # treated as "the" cube file.
  .h5_verify_checksums(tmp)
  .h5_atomic_replace(tmp, dsn)

  if (verbose) message("GPRcube written to: '", dsn, "'")
  invisible(dsn)
}
