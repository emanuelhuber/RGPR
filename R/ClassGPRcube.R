
#' Class GPRcube
#' 
#' An S4 class to represent 3D ground-penetrating radar (GPR) data. 
#' Array of dimension \eqn{n \times m \time p}  (\eqn{n} samples, 
#' \eqn{m} traces or A-scans along \eqn{x}, 
#' and \eqn{p} traces or A-scans along \eqn{y}), with grid cell sizes 
#' \eqn{dx}, \eqn{dy}, and \eqn{dz}.
#' We assume that the unit along y is the same as the unit along x.
#' @slot dx     [\code{numeric(1)}] Grid cell size along x
#' @slot dy     [\code{numeric(1)}] Grid cell size along y
#' @slot dz     [\code{numeric(1)}] Grid cell size along z
#' @slot ylab   [\code{character(1|p)}] Label of \code{y}.
#' @slot center [\code{numeric(3)}] Coordinates of the bottom left grid corner.
#' @slot rot    [\code{numeric(1)}] Rotation angle
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
    
    center = "numeric",    # coordinates grid corner bottom left (0,0)
    rot    = "numeric"     # affine transformation
  )
)
