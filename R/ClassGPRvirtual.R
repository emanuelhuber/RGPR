#------------------------------------------#
#----------- CLASS DEFINITION -------------#

#' Class GPR
#' 
#' A virtual S4 class to represent a ground-penetrating radar (GPR) data.
#' 
#' @section \code{mode}: survey mode, difference between reflection and CMP and WARR
#' \describe{
#'   \item{CO}{Common-offset: The trace positions increase from trace to trace. The
#'                     antenna separation distance is constant.
#'                     Amplitude = f(depth, position)}
#'   \item{CMP}{The trace positions, \code{x@pos}, are constant and equal to 
#'              zero. The antenna separation distance increases increase from
#'              trace to trace.
#'              Amplitude = f(depth, antsep)}
#'   \item{WARR}{The trace positions, \code{x@pos}, are not constant because
#'              while the signal is emitted from the same position, the signal
#'              the signal is recorded from different positions. 
#'              The antenna separation distance increases increase from
#'              trace to trace.
#'              Is "CMP" a special case of "WARR"?
#'              Amplitude = f(depth, antsep)}
#'   \item{CMPAnalysis}{No trace positions, \code{x@pos = numeric(0)},
#'                      no antenna separation, \code{x@antsep = numeric(0)},
#'                      coherence/semblance = f(depth, velocity)}
#' }
#' 
#' @section \code{data}:
#' \describe{
#'   \item{class \code{GPR}}{TA \eqn{m \times n} numeric matrix consiting of a 
#'            cross-section of signal amplitudes as a function of the GPR 
#'            position. The columns of \code{data} correspond to the GPR 
#'            traces and the row of \code{data} to the time/depth samples.}
#'   \item{class \code{GPRset}}{3D data}
#'   \item{class \code{GPRcube}}{3D data}
#' }
#' @slot version      [\code{character(1)}] Version of RGPR.
#' @slot name         [\code{character(1)}] Name of the GPR data.
#' @slot path         [\code{character(1)}] File path of the original GPR data.
#' @slot desc         [\code{character(1)}] Description of the GPR data.
#' @slot mode         [\code{character(1)}] Survey mode 
#'                    (e.g., \code{"CO"}, \code{"CMP"}).
#' @slot date         [\code{Date(1)}] Date of the survey (class \code{Date},
#'                    e.g., \code{Sys.Date()}).
#' @slot freq         [\code{numeric(1)}] GPR antennae frequency (in MHz).
#' @slot data         [\code{array}] GPR data.
#' @slot dunit        [\code{character(1)}] GPR data unit.
#' @slot dlab         [\code{character(1)}] GPR data label.
#' @slot crs          [\code{character(1)}] Coordinate reference system following the 
#'                    R notation of proj4string from the PROJ.4 library. 
#' @slot spunit       [\code{character(1)}] Spatial units.
#' @slot xunit        [\code{character(1)}] Unit of \code{x}.
#' @slot xlab         [\code{character(1)}] Label of \code{x}.
#' @slot zunit        [\code{character(1)}] Unit of \code{z}.
#' @slot zlab         [\code{character(1)}] Label of \code{z}.
#' @slot vel          [\code{list}] Velocity model.
#' @slot proc         [\code{list}] Each element corresponds
#'                    to a processing step applied to the data.
#' @slot delineations [\code{list}] Hand-delineated structures.
#' @slot md           [\code{list}] Additional meta data from GPR file.
#' @name GPRvirtual-class
#' @rdname GPRvirtual-class
#' @export
setClass(
  Class = "GPRvirtual",
  contains = "VIRTUAL",
  slots = c(
    version      = "character",  # class version
    name         = "character",  # data name
    path         = "character",  # data file path
    desc         = "character",  # data description
    mode         = "character",  # reflection/CMP/WARR (CMPAnalysis/spectrum/...)?
    date         = "Date",       # survey date (format %Y-%m-%d)
    freq         = "numeric",    # antenna frequency
    
    data         = "array",      # data
    dunit        = "character",  # data unit
    dlab         = "character", 
    
    spunit       = "character",  # spatial unit
    crs          = "character",  # coordinate reference system of @coord
    # ? coordref = "numeric",    # coordinates references or "center" or "centroid"
    
    xunit        = "character",  # horizontal unit
    xlab         = "character",
    
    zunit        = "character",  # time/depth unit
    zlab         = "character",
    
    vel          = "list",   # velocity model (function of x, y, z)
    
    proc         = "list",       # processing steps
    delineations = "list",       # delineations
    md           = "list"        # data from header file/meta-data
  )
)





