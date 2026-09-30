# ============================================================================ #
# materialize() for GPRcube
# ============================================================================ #
#
# The `materialize` generic is already declared in writeGPR.R (for
# GPRsurvey); this file only adds the GPRcube method.
#
# Unlike GPRsurvey's current materialize() -- which, for format = "h5",
# just file.copy()'s the *entire* backing file regardless of which subset
# of lines the view actually selects (a known limitation: materializing a
# 3-line view out of a 100-line survey still clones all 100 lines) --
# GPRcube's materialize() extracts exactly the requested region:
#
#   - if obj is a VIEW (@view = TRUE): only obj@viewIdx's region is read
#     from the ORIGINAL backing file and written to the new file, in
#     memory-bounded batches along z (same batching spirit as
#     .writeCubeHDF5() / .computeSlicesBatched() in interpSlices.R) --
#     the source file's full extent is never loaded.
#   - if obj is a "whole" HDF5-backed cube (@view = FALSE, isH5Backed(obj)
#     TRUE): the entire dataset is copied across in the same batched way
#     (equivalent to view = "read everything").
#   - if obj has no backing file at all (a plain in-memory cube): there's
#     nothing to stream from, so @data is written directly in one shot.
#
# In all cases the *diagnostic* datasets .writeCubeHDF5() also writes
# (x0, y0, z0 -- the original per-trace observations) are NOT carried
# over: those are indexed by observation, not by the regular x/y grid, so
# they don't subset meaningfully along an arbitrary [i,j,k] view selection.
# Only the grid itself (/cube/z, /cube/x, /cube/y, /cube/vz) is written.
# ============================================================================ #

#' @rdname materialize
#' @export
setMethod(
  "materialize",
  "GPRcube",
  function(obj, dsn,
           overwrite  = FALSE,
           compress   = 5L,
           batch_size = NULL,
           verbose    = TRUE,
           ...) {
    
    if (missing(dsn) || is.null(dsn) || length(dsn) != 1L || !nzchar(dsn)) {
      stop(
        "Argument 'dsn' is required: provide the path to the output HDF5 ",
        "backing file, e.g. materialize(x, dsn = 'subset_cube.h5').",
        call. = FALSE
      )
    }
    
    compress <- as.integer(compress)
    if (length(compress) != 1L || is.na(compress) ||
        compress < 0L || compress > 9L) {
      stop("'compress' must be an integer between 0 and 9.", call. = FALSE)
    }
    
    d  <- dim(obj)
    nx <- d[1]; ny <- d[2]; nz <- d[3]
    
    gx <- obj@center[1] + seq(0, by = obj@dx, length.out = nx)
    gy <- obj@center[2] + seq(0, by = obj@dy, length.out = ny)
    vz <- obj@z   # depths/times of obj's own slices (see GPRcube-class)
    if (length(vz) != nz) {
      stop("Internal error: length(obj@z) (", length(vz), ") does not match ",
           "the number of z-slices (", nz, ").", call. = FALSE)
    }
    
    if (isH5Backed(obj)) {
      # There IS a backing HDF5 file (obj is a view, or a "whole"
      # HDF5-backed cube): stream the requested region straight from it,
      # in batches, without ever loading the whole thing into R.
      path <- .materializeCubeFromH5(
        obj, dsn = dsn, gx = gx, gy = gy, vz = vz,
        compress = compress, overwrite = overwrite,
        batch_size = batch_size, verbose = verbose
      )
    } else {
      # No backing file: obj@data is already fully in memory, so just
      # write it directly -- no batching needed on the read side.
      path <- .materializeCubeFromMemory(
        obj, dsn = dsn, gx = gx, gy = gy, vz = vz,
        compress = compress, overwrite = overwrite, verbose = verbose
      )
    }
    
    obj@data    <- array(dim = c(0L, 0L, 0L))
    obj@path    <- path
    obj@view    <- FALSE
    obj@viewIdx <- list()
    obj
  }
)


#' Create a new, empty chunked `/cube/z` dataset (+ axes) in a temp HDF5 file
#'
#' Shared setup used by both `.materializeCubeFromH5()` and
#' `.materializeCubeFromMemory()`: creates the temp file, `/cube` group,
#' chunked+checksummed(+optionally compressed) `z` dataset, and the small
#' `x`/`y`/`vz` axis datasets. Caller fills `z` and then calls
#' `.finalizeCubeH5File()`.
#'
#' @param dsn (`character[1]`) Final destination path.
#' @param nx,ny,nz (`integer[1]`)
#' @param gx,gy,vz (`numeric`) Grid axes, length `nx`/`ny`/`nz`.
#' @param compress (`integer[1]`) gzip level 0-9 for `z`.
#' @param overwrite (`logical[1]`)
#' @return `list(tmp=, dsn=, h5=, grp=, ds_z=)`
#' @keywords internal
#' @noRd
.newCubeH5File <- function(dsn, nx, ny, nz, gx, gy, vz, compress, overwrite) {
  if (!requireNamespace("hdf5r", quietly = TRUE)) {
    stop("Package 'hdf5r' is required to materialize a GPRcube.\n",
         "Install it with: install.packages('hdf5r')", call. = FALSE)
  }
  
  dsn <- path.expand(dsn)
  if (file.exists(dsn) && !overwrite) {
    stop("File already exists: '", dsn, "'.\n",
         "Use overwrite = TRUE to replace it.", call. = FALSE)
  }
  dir.create(dirname(dsn), recursive = TRUE, showWarnings = FALSE)
  
  tmp <- .h5_temp_path(dsn)
  
  h5  <- hdf5r::H5File$new(tmp, mode = "w")
  grp <- h5$create_group("cube")
  
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
  
  .h5_write_vector(grp, "x",  gx)
  .h5_write_vector(grp, "y",  gy)
  .h5_write_vector(grp, "vz", vz)
  hdf5r::h5attr(grp, "nx") <- nx
  hdf5r::h5attr(grp, "ny") <- ny
  hdf5r::h5attr(grp, "nz") <- nz
  
  list(tmp = tmp, dsn = dsn, h5 = h5, grp = grp, ds_z = ds_z)
}


#' Close, checksum-verify, and atomically install a temp cube HDF5 file
#' @param f The `list` returned by `.newCubeH5File()`.
#' @keywords internal
#' @noRd
.finalizeCubeH5File <- function(f) {
  f$h5$close_all()
  .h5_verify_checksums(f$tmp)
  .h5_atomic_replace(f$tmp, f$dsn)
  invisible(f$dsn)
}


#' Materialize by streaming from an HDF5-backed source (view or whole cube)
#' @keywords internal
#' @noRd
.materializeCubeFromH5 <- function(obj, dsn, gx, gy, vz, compress, overwrite,
                                   batch_size, verbose) {
  nx <- length(gx); ny <- length(gy); nz <- length(vz)
  
  f <- .newCubeH5File(dsn, nx, ny, nz, gx, gy, vz, compress, overwrite)
  h5_open <- TRUE
  on.exit(if (h5_open) try(f$h5$close_all(), silent = TRUE), add = TRUE)
  on.exit(unlink(f$tmp, force = TRUE), add = TRUE)
  
  reader <- .openCubeReader(obj)   # opens obj@path ONCE for the whole loop
  on.exit(reader$close(), add = TRUE)
  
  if (is.null(batch_size)) {
    slice_mb   <- nx * ny * 8 / 1024^2
    batch_size <- max(1L, min(nz, floor(300 / max(slice_mb, 1e-6))))
  }
  batches <- split(seq_len(nz), ceiling(seq_len(nz) / batch_size))
  
  for (b in batches) {
    batch_array <- reader$read(seq_len(nx), seq_len(ny), b, drop = FALSE)
    f$ds_z[1:nx, 1:ny, b] <- batch_array
    rm(batch_array)
    if (verbose) {
      message(sprintf("  slices %d-%d / %d materialized", min(b), max(b), nz))
    }
  }
  
  path <- .finalizeCubeH5File(f)
  h5_open <- FALSE
  if (verbose) message("GPRcube materialized to: '", path, "'")
  path
}


#' Materialize directly from an in-memory cube's @data (no source file)
#' @keywords internal
#' @noRd
.materializeCubeFromMemory <- function(obj, dsn, gx, gy, vz, compress, overwrite,
                                       verbose) {
  nx <- length(gx); ny <- length(gy); nz <- length(vz)
  
  f <- .newCubeH5File(dsn, nx, ny, nz, gx, gy, vz, compress, overwrite)
  h5_open <- TRUE
  on.exit(if (h5_open) try(f$h5$close_all(), silent = TRUE), add = TRUE)
  on.exit(unlink(f$tmp, force = TRUE), add = TRUE)
  
  f$ds_z[1:nx, 1:ny, 1:nz] <- obj@data
  
  path <- .finalizeCubeH5File(f)
  h5_open <- FALSE
  if (verbose) message("GPRcube materialized to: '", path, "'")
  path
}
