# ============================================================================ #
# Accessors for HDF5-backed GPRcube (interpSlices() Phase 2)
# ============================================================================ #
#
# When interpSlices(..., hdf5 = "always"/"auto") streams a cube straight to
# an HDF5 file, the returned GPRcube has data = array(dim = c(0,0,0)) and
# @path pointing at that file (see .writeCubeHDF5() / interpSlices,GPRsurvey).
#
# What's here:
#   - dim() falls back to reading the dataset's shape (cheap: no data read)
#   - isH5Backed() lets calling code check before trying @data
#   - loadCube() explicitly reifies the *full* array into memory on request
#   - .readCubeRegion() reads only a requested [i,j,k] hyperslab -- this is
#     what subset_GPRcube.R's "[" method uses, so x[i, j, k] on an
#     HDF5-backed cube reads only the requested region rather than
#     requiring the whole cube to be loaded first (see subset_GPRcube.R)
#
# Still out of scope: plot(), filter2D(), and arithmetic ops on GPRcube
# still assume an in-memory @data array. Those would need to either call
# loadCube() first or be reworked to go through .readCubeRegion() the same
# way "[" now does -- a separate piece of work per method.
# ============================================================================ #

#' Is this GPRcube backed by an HDF5 file rather than an in-memory array?
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


#' Read a `[i, j, k]` region of a `GPRcube`'s data
#'
#' Internal helper used by `subset_GPRcube.R`'s `"["` method. For an
#' in-memory cube this is just `x@data[i, j, k, drop = drop]`. For an
#' HDF5-backed cube (see `isH5Backed()`), only the requested hyperslab is
#' read from disk -- the full cube is never loaded just to extract a slice
#' or sub-region. `hdf5r`'s dataset indexing is documented to mirror base
#' R array indexing (including fancy/vector indices and a `drop`
#' argument); if that assumption turns out wrong for some `hdf5r` version,
#' this falls back to reading without `drop` and applying base R's
#' `drop()` manually, which has identical length-1-dimension-dropping
#' semantics to `[, drop = TRUE]`.
#'
#' @param x (`GPRcube`)
#' @param i,j,k (`integer`) Indices along each dimension (already resolved
#'   to concrete integer vectors by the caller -- this helper does not
#'   handle `missing()`/default-index logic).
#' @param drop (`logical[1]`) Passed through to the underlying `[`.
#' @return (`array`) or a lower-dimensional object if dimensions were
#'   dropped, exactly as `x@data[i, j, k, drop = drop]` would return.
#' @keywords internal
#' @noRd
.readCubeRegion <- function(x, i, j, k, drop = TRUE) {
  if (!isH5Backed(x)) {
    return(x@data[i, j, k, drop = drop])
  }
  if (!file.exists(x@path)) {
    stop(
      "This GPRcube is HDF5-backed but its backing file is missing:\n  ",
      x@path, "\nWas it moved, renamed, or deleted after interpSlices() created it?",
      call. = FALSE
    )
  }
  h5 <- hdf5r::H5File$new(x@path, mode = "r")
  on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)
  ds <- h5[["cube/z"]]
  i <- as.integer(i); j <- as.integer(j); k <- as.integer(k)
  tryCatch(
    ds[i, j, k, drop = drop],
    error = function(e) {
      out <- ds[i, j, k]
      if (isTRUE(drop)) drop(out) else out
    }
  )
}


#' Load an HDF5-backed `GPRcube`'s data into memory
#'
#' `interpSlices(..., hdf5 = "always")` (or `"auto"` for large cubes)
#' returns a `GPRcube` whose `@data` is empty and whose `@path` points at
#' the HDF5 file holding the actual array. `loadCube()` reads that array
#' back into `@data`, returning an ordinary in-memory `GPRcube`. Note this
#' defeats the purpose of HDF5 backing if the cube doesn't actually fit in
#' memory -- prefer `x[i, j, k]` (see `subset_GPRcube.R`) to read only the
#' region/slices you need for very large cubes.
#'
#' @param x (`GPRcube`)
#' @param ... Currently unused.
#' @return (`GPRcube`) with `@data` populated. If `x` is already
#'   in-memory, returned unchanged.
#' @export
#' @rdname loadCube
setGeneric("loadCube", function(x, ...) standardGeneric("loadCube"))

#' @rdname loadCube
#' @export
setMethod("loadCube", "GPRcube", function(x, ...) {
  if (!isH5Backed(x)) return(x)
  
  if (!file.exists(x@path)) {
    stop("HDF5 backing file not found: '", x@path, "'.", call. = FALSE)
  }
  
  h5 <- hdf5r::H5File$new(x@path, mode = "r")
  on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)
  
  x@data <- h5[["cube/z"]]$read()
  x
})
