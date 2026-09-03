# ============================================================================ #
# Accessors for HDF5-backed / view GPRcube (interpSlices() Phase 2 + views)
# ============================================================================ #
#
# Two related but distinct things make a GPRcube's data NOT be a plain
# in-memory array:
#
#   1. HDF5-backed: interpSlices(..., hdf5 = "always"/"auto") streamed a
#      cube straight to an HDF5 file. @data = array(dim = c(0,0,0)),
#      @path points at that file, @view = FALSE (see .writeCubeHDF5()).
#
#   2. View: x[i, j, k] on an HDF5-backed (or already-view) cube returns a
#      *view* instead of eagerly reading -- @data stays empty, @path still
#      points at the ORIGINAL backing file (never a new one), @view = TRUE,
#      and @viewIdx = list(i=, j=, k=) records which indices into that
#      original file this view represents. Subsetting a view again
#      composes the index mapping rather than replacing it, so
#      x[1:10,,][ ,1:5, ] still resolves back to x's original file
#      correctly (see subset_GPRcube.R).
#
# isH5Backed(x) is TRUE for both cases (data empty + path set); @view
# distinguishes "the whole file" from "a subset of it".
#
# What's here:
#   - isH5Backed() / dim() -- introspection without reading data
#   - .openCubeReader() / .readCubeRegion_ds() -- read a [i,j,k] hyperslab,
#     resolving view-index composition once and (for .openCubeReader())
#     reusing a single open file handle across many reads -- used by
#     materialize_GPRcube.R's batched writer so it doesn't reopen the
#     source file once per batch
#   - .readCubeRegion() -- single-shot convenience wrapper around the
#     above, used by subset_GPRcube.R's "[" method and by loadCube()
#   - loadCube() -- reifies a view's (or a whole HDF5-backed cube's)
#     region into memory, clearing @view/@viewIdx since the result is no
#     longer tied to the source file's indexing
#
# Still out of scope: plot(), filter2D(), and arithmetic ops on GPRcube
# still assume an in-memory @data array. Those would need to either call
# loadCube() first or be reworked to go through .readCubeRegion() the same
# way "[" does -- a separate piece of work per method.
# ============================================================================ #

#' Is this GPRcube backed by an HDF5 file rather than an in-memory array?
#'
#' `TRUE` both for a "whole" HDF5-backed cube (`@view = FALSE`) and for an
#' unmaterialized view/subset of one (`@view = TRUE`) -- in both cases
#' `@data` is empty and the real data lives at `@path`. Use `x@view`
#' directly if you need to distinguish the two.
#' @param x (`GPRcube`)
#' @return (`logical[1]`)
#' @export
#' @rdname isH5Backed
setGeneric("isH5Backed", function(x) standardGeneric("isH5Backed"))

#' @rdname isH5Backed
#' @export
setMethod("isH5Backed", "GPRcube", function(x) {
  length(x@data) == 0L && length(x@path) == 1L && nzchar(x@path)
})


#' @rdname GPRcube-class
#' @export
setMethod("dim", "GPRcube", function(x) {
  if (isTRUE(x@view)) {
    # A view's shape is just the length of its index mapping -- no need to
    # touch the backing file at all for this.
    return(c(length(x@viewIdx$i), length(x@viewIdx$j), length(x@viewIdx$k)))
  }
  if (isH5Backed(x)) {
    if (!file.exists(x@path)) {
      stop("HDF5 backing file not found: '", x@path, "'.", call. = FALSE)
    }
    h5 <- hdf5r::H5File$new(x@path, mode = "r")
    on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)
    return(h5[["cube/z"]]$dims)
  }
  dim(x@data)
})


#' Read a `[i, j, k]` hyperslab from an already-open cube dataset
#'
#' Low-level reader shared by `.openCubeReader()` and (indirectly)
#' `.readCubeRegion()`. `hdf5r`'s dataset indexing is documented to mirror
#' base R array indexing (including fancy/vector indices and a `drop`
#' argument); if that assumption turns out wrong for some `hdf5r` version,
#' falls back to reading without `drop` and applying base R's `drop()`
#' manually, which has identical length-1-dimension-dropping semantics to
#' `[, drop = TRUE]`.
#'
#' @param ds An open `hdf5r::H5D` dataset object (i.e. `h5[["cube/z"]]`).
#' @param i,j,k (`integer`) Indices *already resolved to the dataset's own
#'   coordinate space* -- i.e. already composed through any view mapping.
#' @param drop (`logical[1]`)
#' @return (`array`) as `ds[i, j, k, drop = drop]` would return.
#' @keywords internal
#' @noRd
.readCubeRegion_ds <- function(ds, i, j, k, drop = TRUE) {
  i <- as.integer(i); j <- as.integer(j); k <- as.integer(k)
  tryCatch(
    ds[i, j, k, drop = drop],
    error = function(e) {
      out <- ds[i, j, k]
      if (isTRUE(drop)) drop(out) else out
    }
  )
}


#' Open a reusable reader for an HDF5-backed (or view) `GPRcube`
#'
#' Opens `x`'s backing file *once* and returns a small interface for
#' reading `[i, j, k]` regions in the cube's *own* coordinate space
#' (indices are automatically composed through `x@viewIdx` when
#' `x@view = TRUE`), plus a `close()` to release the handle when done.
#' Intended for callers that need several reads from the same cube --
#' e.g. `materialize_GPRcube.R`'s batched writer -- so the file is opened
#' once for the whole operation rather than once per batch.
#'
#' @param x (`GPRcube`) Must satisfy `isH5Backed(x)`.
#' @return `list(read = function(i, j, k, drop = TRUE), dim = integer[3],
#'   close = function())`.
#' @keywords internal
#' @noRd
.openCubeReader <- function(x) {
  if (!isH5Backed(x)) {
    stop(".openCubeReader() requires an HDF5-backed GPRcube.", call. = FALSE)
  }
  if (!file.exists(x@path)) {
    stop(
      "This GPRcube is HDF5-backed but its backing file is missing:\n  ",
      x@path, "\nWas it moved, renamed, or deleted after interpSlices() created it?",
      call. = FALSE
    )
  }
  
  h5 <- hdf5r::H5File$new(x@path, mode = "r")
  ds <- h5[["cube/z"]]
  is_view <- isTRUE(x@view)
  vidx <- x@viewIdx
  d <- dim(x)
  
  read <- function(i, j, k, drop = TRUE) {
    if (is_view) {
      i <- vidx$i[i]; j <- vidx$j[j]; k <- vidx$k[k]
    }
    .readCubeRegion_ds(ds, i, j, k, drop = drop)
  }
  
  list(
    read  = read,
    dim   = d,
    close = function() try(h5$close_all(), silent = TRUE)
  )
}


#' Read a `[i, j, k]` region of a `GPRcube`'s data
#'
#' Internal helper used by `subset_GPRcube.R`'s `"["` method and by
#' `loadCube()`. For an in-memory cube this is just
#' `x@data[i, j, k, drop = drop]`. For an HDF5-backed cube (see
#' `isH5Backed()`), only the requested hyperslab is read from disk -- the
#' full cube is never loaded just to extract a slice or sub-region. If `x`
#' is itself a view (`x@view = TRUE`), `i`/`j`/`k` (given in the view's
#' own coordinate space) are composed through `x@viewIdx` before reading,
#' so this always reads from `x@path`, which for a view is the *original*
#' backing file.
#'
#' This opens and closes the backing file for a single read; see
#' `.openCubeReader()` if you need several reads from the same cube (e.g.
#' a batched loop) without reopening the file each time.
#'
#' @param x (`GPRcube`)
#' @param i,j,k (`integer`) Indices along each dimension, in `x`'s own
#'   coordinate space (already resolved to concrete integer vectors by
#'   the caller -- this helper does not handle `missing()`/default-index
#'   logic).
#' @param drop (`logical[1]`) Passed through to the underlying `[`.
#' @return (`array`) or a lower-dimensional object if dimensions were
#'   dropped, exactly as `x@data[i, j, k, drop = drop]` would return.
#' @keywords internal
#' @noRd
.readCubeRegion <- function(x, i, j, k, drop = TRUE) {
  if (!isH5Backed(x)) {
    return(x@data[i, j, k, drop = drop])
  }
  reader <- .openCubeReader(x)
  on.exit(reader$close(), add = TRUE)
  reader$read(i, j, k, drop = drop)
}


#' Load an HDF5-backed (or view) `GPRcube`'s data into memory
#'
#' `interpSlices(..., hdf5 = "always")` (or `"auto"` for large cubes)
#' returns a `GPRcube` whose `@data` is empty and whose `@path` points at
#' the HDF5 file holding the actual array; subsetting such a cube (or a
#' view of one) with `[` returns another view rather than reading
#' anything (see `subset_GPRcube.R`). `loadCube()` reads a cube's *own*
#' region -- the whole file for a non-view cube, or just the view's
#' region for a view -- into `@data`, returning an ordinary in-memory
#' `GPRcube` with `@view` cleared. Note this defeats the purpose of HDF5
#' backing if the region doesn't actually fit in memory -- prefer
#' `x[i, j, k]` to read only what you need, or `materialize()` to write a
#' view out to its own independent HDF5 file without loading it into R at
#' all.
#'
#' @param x (`GPRcube`)
#' @param ... Currently unused.
#' @return (`GPRcube`) with `@data` populated and `@view = FALSE`. If `x`
#'   is already in-memory, returned unchanged.
#' @export
#' @rdname loadCube
setGeneric("loadCube", function(x, ...) standardGeneric("loadCube"))

#' @rdname loadCube
#' @export
setMethod("loadCube", "GPRcube", function(x, ...) {
  if (!isH5Backed(x)) return(x)
  
  d <- dim(x)
  x@data    <- .readCubeRegion(x, seq_len(d[1]), seq_len(d[2]), seq_len(d[3]), drop = FALSE)
  x@view    <- FALSE
  x@viewIdx <- list()
  x
})
