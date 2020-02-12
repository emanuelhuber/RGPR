
#' Class GPR
#' 
#' An S4 class to represent a ground-penetrating radar (GPR) data.
#' 
#' \code{m} traces (A-scans) and \code{n} samples
#' 
#' @slot z0      [\code{\numeric(m)}] Time-zero or depth-zero.
#' @slot time    [\code{\numeric(m)}] Recording time of every trace.
#' @slot antsep  [\code{\numeric(m)}] Antenna separation.
#' @slot markers [\code{\character(m)}] Fiducial markers associated with the traces.
#' @slot ann     [\code{\character(m)}] Annotations associated with the traces.
#' @slot coord   [\code{\matrix(m,3)}] Trace positions.
#' @slot rec     [\code{\matrix(m,3)}] Receiver positions
#' @slot trans   [\code{\matrix(m,3)}] Transmitter positions
#' @slot angles  [\code{\matrix(m,2)}] Transmitter positions


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
    
    angles      = "matrix"       # yaw - pitch - roll
  )
)
