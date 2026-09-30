
#' Class GPRcube
#' 
#' An S4 class to represent 3D ground-penetrating radar (GPR) data. 
#' Array of dimension \eqn{n \times m \times p}  (\eqn{n} samples, 
#' \eqn{m} traces or A-scans along \eqn{x}, 
#' and \eqn{p} traces or A-scans along \eqn{y}), with horizontal grid cell
#' sizes \eqn{dx} and \eqn{dy}, and slices at the depths/times given by
#' `z` (which need not be evenly spaced).
#' We assume that the unit along y is the same as the unit along x.
#' @slot dx     (`numeric[1]`) Grid cell size along x
#' @slot dy     (`numeric[1]`) Grid cell size along y
#' @slot z      (`numeric[p]`) Depth/time of each of the `p` slices (the
#'              cube's 3rd data dimension), in `@zunit`. Slices need not
#'              be evenly spaced, but `z` must be *strictly monotonic*
#'              from top to bottom: strictly increasing (small to large)
#'              when `@zunit` is a time unit (see `isZTime()`), strictly
#'              decreasing (large to small) when it's a depth/elevation
#'              unit -- enforced by this class's validity check (see
#'              `setValidity()` below). `length(z)` must match the size of
#'              the cube's 3rd dimension.
#' @slot ylab   (`character[1|p]`) Label of `y`.
#' @slot center (`numeric[3]`) Coordinates of the bottom left grid corner;
#'              the 3rd element is redundant with (and should equal)
#'              `z[1]`, kept for backward compatibility.
#' @slot rot    (`numeric[1]`) Rotation angle
#' @slot view   (`logical[1]`) `TRUE` if this object is an unmaterialized
#'              view: a subset of an HDF5-backed cube (see `isH5Backed()`)
#'              obtained via `[`. A view's `@data` is empty and `@path`
#'              still points at the *original* backing file -- nothing is
#'              read from disk until it's actually needed (see
#'              `ClassGPRcube_hdf5.R`). Call `materialize()` to write a
#'              view's data to its own independent HDF5 file, or
#'              `loadCube()` to pull just the view's region into memory.
#'              Always `FALSE` for an in-memory cube or a "whole",
#'              non-view HDF5-backed cube.
#' @slot viewIdx (`list`) Only meaningful when `view = TRUE`: 
#'              `list(i = integer, j = integer, k = integer)`, the indices
#'              into the *original* HDF5 dataset at `@path` that this view
#'              represents. Composed (not overwritten) when a view is
#'              subsetted again, so `x[1:10,,][ ,1:5, ]` still resolves
#'              back to `x`'s original backing file correctly. Empty list
#'              when `view = FALSE`.
#' @name GPRcube-class
#' @rdname GPRcube-class
#' @export
setClass(
  Class="GPRcube",
  contains = "GPRvirtual",  
  slots=c(
    dx     = "numeric",
    dy     = "numeric",
    z      = "numeric",    # depth/time of each slice (3rd dimension), top to bottom
    ylab   = "character",  # set names, length = 1|p
    
    center = "numeric",    # coordinates grid corner bottom left (0, 0, 0)
    rot    = "numeric",    # affine transformation
    
    view    = "logical",   # TRUE = unmaterialized view of an HDF5-backed cube
    viewIdx = "list"       # list(i=,j=,k=) indices into the ORIGINAL @path dataset
  ),
  prototype = prototype(
    view    = FALSE,
    viewIdx = list()
  )
)

#' GPRcube validity: `@z` must be strictly monotonic, top to bottom
#' 
#' Enforced automatically by `new("GPRcube", ...)` / `new("GPRslice", ...)`
#' (`GPRslice contains GPRcube` and inherits this slot/check) and by
#' explicit `validObject()` calls -- every construction site in the
#' package (`interpSlices.R`, `subset_GPRcube.R`, `coercion_GPRcube.R`,
#' `createCubeFromGrid.R`, ...) goes through `new()`, so this single check
#' covers all of them without needing a repeated manual check in each.
#' 
#' Direction depends on `@zunit` (via `isZTime()`, inherited from
#' `GPRvirtual`): strictly increasing for a time unit (small to large,
#' e.g. two-way travel time growing with depth), strictly decreasing for
#' a depth/elevation unit (large to small, e.g. elevation decreasing with
#' depth) -- matching the convention already used by
#' `.computeTargetDepths()` in `interpSlices.R`. If `@zunit` isn't set (or
#' isn't a single non-empty string), only "strictly monotonic in *some*
#' direction" is enforced, since direction can't be determined.
#' 
#' Deliberately does NOT cross-check `length(z)` against `dim(object)`:
#' for an HDF5-backed "whole" (non-view) cube, `dim()` requires opening
#' the backing file, and validity checks are conventionally expected to
#' be fast and I/O-free. Callers are responsible for keeping `z` and the
#' data dimensions consistent (see e.g. `subset_GPRcube.R`'s defensive
#' length check right where a `z` view is sliced).
#' @noRd
setValidity("GPRcube", function(object) {
  z <- object@z
  if (length(z) < 2L) return(TRUE)
  
  dz <- diff(z)
  if (any(dz == 0)) {
    return("@z must be strictly monotonic: it contains repeated depth/time values.")
  }
  increasing <- all(dz > 0)
  decreasing <- all(dz < 0)
  if (!increasing && !decreasing) {
    return("@z must be strictly monotonic from top to bottom (either entirely increasing or entirely decreasing).")
  }
  
  if (length(object@zunit) == 1L && nzchar(object@zunit)) {
    time_axis <- isTRUE(isZTime(object))
    if (time_axis && !increasing) {
      return(paste0(
        "@z must be strictly increasing from top to bottom (small to large) ",
        "since @zunit ('", object@zunit, "') is a time unit."
      ))
    }
    if (!time_axis && !decreasing) {
      return(paste0(
        "@z must be strictly decreasing from top to bottom (large to small) ",
        "since @zunit ('", object@zunit, "') is a depth/elevation unit."
      ))
    }
  }
  
  TRUE
})
