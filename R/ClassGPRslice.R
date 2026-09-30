

#' Class GPRslice
#' 
#' An S4 class to represent time/depth slices of 
#' ground-penetrating radar (GPR) data.
#' Array of dimension \eqn{1 \times m \times p}  
#' (\eqn{m} traces or A-scans along \eqn{x}, 
#' and \eqn{p} traces or A-scans along \eqn{y}), with grid cell sizes 
#' \eqn{dx}, \eqn{dy}.
#' We assume that the unit along y is the same as the unit along x.
#' 
#' `GPRslice` has no slots of its own: it's simply a `GPRcube` (see
#' `GPRcube-class`) whose `@z` has length 1 -- the depth/time of that
#' single slice -- and whose validity constraint (`@z` strictly
#' monotonic) is trivially satisfied for a length-1 vector.
#' @name GPRslice-class
#' @rdname GPRslice-class
#' @export
setClass(
  Class = "GPRslice",  
  contains = "GPRcube"
)
