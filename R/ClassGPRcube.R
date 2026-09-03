
#' Class GPRcube
#' 
#' An S4 class to represent 3D ground-penetrating radar (GPR) data. 
#' Array of dimension \eqn{n \times m \times p}  (\eqn{n} samples, 
#' \eqn{m} traces or A-scans along \eqn{x}, 
#' and \eqn{p} traces or A-scans along \eqn{y}), with grid cell sizes 
#' \eqn{dx}, \eqn{dy}, and \eqn{dz}.
#' We assume that the unit along y is the same as the unit along x.
#' @slot dx     (`numeric[1]`) Grid cell size along x
#' @slot dy     (`numeric[1]`) Grid cell size along y
#' @slot dz     (`numeric[1]`) Grid cell size along z
#' @slot ylab   (`character[1|p]`) Label of `y`.
#' @slot center (`numeric[3]`) Coordinates of the bottom left grid corner.
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
    dz     = "numeric",
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
