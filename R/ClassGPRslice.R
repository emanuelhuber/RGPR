

#' Class GPRslice
#' 
#' An S4 class to represent time/depth slices of 
#' ground-penetrating radar (GPR) data.
#' Array of dimension \eqn{1 \times m \times p}  
#' (\eqn{m} traces or A-scans along \eqn{x}, 
#' and \eqn{p} traces or A-scans along \eqn{y}), with grid cell sizes 
#' \eqn{dx}, \eqn{dy}.
#' We assume that the unit along y is the same as the unit along x.
#' @name GPRslice-class
#' @rdname GPRslice-class
#' @export
setClass(
  Class = "GPRslice",  
  contains = "GPRcube",
  slots=c(
    z     = "numeric"    # depth of the slice
    # dlim  = "numeric"     # length 2: value range from the cube
    # in fact the user can set clim from a GPRcube. so no need
  )
)
