# RULE
#  - warnings & messages suppressed in functions other than 'GPRsurvey()'
#  - raise an error when crs/zunit and spunit are different


#' An S4 class to represent a collection of \eqn{k} GPR data. 
#' An object of the class \code{GPRsurvey} does only contain the links to 
#' the GPR data files as well as information about the GPR data such as 
#' coordinates, frequencies, antenna separation etc.  
#' The methods of the \code{GPRsurvey} class exclusively manipulate the trace 
#' (A-scan) coordinates. More specifically, the methods currently allow to:
#' \itemize{
#'   \item display map of the GPR measurement lines that can be superimposed 
#'   on any raster or vector data,
#'   \item display GPR data in three-dimensional interactive graphics 
#'   (e.g., fence  diagram) with openGL,
#'   \item add or interpolate trace coordinates (the elevation coordinates 
#'   can be interpolated from raster data),
#'   \item georeference the trace coordinates (e.g., from a local coordinate
#'   reference system to a regional reference coordinate system),
#'   \item project the GPR data to any coordinate reference system,
#'   \item estimate the spatial shift between two (parallel)GPR profiles.
#' }
#' @slot version   [\code{character(1)}] Version of RGPR.
#' @slot names     [\code{character(k)}] Names of the GPR data.
#' @slot paths     [\code{character(k)}] File paths of the original GPR data.
#' @slot descs     [\code{character(k)}] Descriptions of the GPR data.
#' @slot modes     [\code{character(k)}] Survey modes of the GPR data
#'                 (e.g., \code{"CO"}, \code{"CMP"}).
#' @slot dates     [\code{Date(k)}] Date of the GPR data (class \code{Date},
#'                 e.g., \code{Sys.Date()}).
#' @slot freqs     [\code{numeric(k)}] Antennae frequency of the GPR data 
#'                 (in MHz).               
#' @slot antseps   [\code{numeric(k)}] Antennae separation of the GPR data 
#' @slot spunit    [\code{character(1)}] Spatial units.
#' @slot crs       [\code{character(1)}] Coordinate reference system in wkt format (sf package)
#' @slot coords    [\code{list(k)}] GPR data coordinates
#' @slot intersections [\code{list(k)}] Intersections coordinates of the GPR data
#' @slot markers   [\code{list(k)}] Fiducial markers of the GPR data
#' @slot nz        [\code{integer(k)}] Number of samples in each GPR data.
#' @slot zlengths  [\code{numeric(k)}] Range along \code{z} in each GPR data.
#' @slot zunits    [\code{character(1)}] Unit of \code{z}.
#' @slot nx        [\code{integer(k)}] Number of traces in each GPR data.
#' @slot xlengths  [\code{numeric(k)}] Length of the GPR data.
#' @slot transf  [\code{numeric(5)}] Translation vector before rotation, after and rotation angle
#' @name GPRsurvey-class
#' @rdname GPRsurvey-class
#' @export
setClass(
  Class = "GPRsurvey",  
  slots = c(
    version       = "character",  # version of the class
    paths         = "character", # filepath of the GPR data
    names         = "character",  # names of the GPR profiles
    descs         = "character",  # descriptions of the GPR profiles
    modes         = "character",  # survey mode (reflection/CMP)
    
    dates         = "Date",       # dates  of the GPR profiles
    
    freqs         = "numeric",    # frequencies of the GPR profiles
    antseps       = "numeric",    # antenna separation of the GPR profiles
    
    spunit        = "character",  # position units  !!!length = 1!!!
    crs           = "character",  # coordinates reference system
    #coordref      = "numeric",   # reference position
    coords        = "list",       # (x,y,z) coordinates for each profiles
    
    intersections     = "list",       # (x,y) position of the profile intersections
    markers       = "list",       # fiducials of the GPR profiles
    
    nz            = "integer",
    zlengths      = "numeric",    # depth/time window (vertical)
    zunits        = "character",  # time/depth unit  !!!length = 1!!!
    nx            = "integer",    # to control if nrow(@coord) == ncol(x[[i]])
    xlengths      = "numeric",     # depth/time window (vertical)
    
    transf        = "numeric"
  )
)
