
#' Class GPR
#' 
#' An S4 class to represent a ground-penetrating radar (GPR) data.
#' 
#' Matrix of dimension \eqn{n \times m} (\eqn{n} samples, 
#' \eqn{m} traces or A-scans).
#' 
#' @slot z0      (`numeric[m]`)   Time-zero or depth-zero.
#' @slot time    (`numeric[m]`)   Recording time of every trace (UTC).
#' @slot antsep  (`numeric[m]`)   Antenna separation.
#' @slot markers (`character[m]`) Fiducial markers associated with the traces.
#' @slot ann     (`character[m]`) Annotations associated with the traces.
#' @slot coord   (`matrix[m,3]`)  Trace positions.
#' @slot rec     (`matrix[m,3]`)  Receiver positions
#' @slot trans   (`matrix[m,3]`)  Transmitter positions
#' @slot x       (`numeric`)      Relative trace position
#' @slot z       (`numeric`)      Relative sample position
#' @slot angles  (`matrix[m,2]`)  Transmitter positions
#' @name GPR-class
#' @rdname GPR-class
#' @export
setClass(
  Class    = "GPR",
  contains = "GPRvirtual",
  slots    = c(
    z0           = "numeric",    # time-zero or depth zero
    time         = "numeric",    # time of the trace recording
    antsep       = "numeric",    # antenna separation
    markers      = "character",  # fiducial marks/markers
    ann          = "character",  # annotation (e.g. intersections)
    
    coord        = "matrix",     # trace coordinates (x,y,z)
    rec          = "matrix",     # receiver coordinates (x,y,z)
    trans        = "matrix",     # transmitter coordinates (x,y,z)
    
    x            = "numeric",    # trace position
    z            = "numeric",    # depth/time position (vertical)
    
    angles       = "matrix"       # theta, phi
  )
)
