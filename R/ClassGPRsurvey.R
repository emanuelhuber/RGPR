# RULE
#  - warnings & messages suppressed in functions other than 'GPRsurvey()'
#  - raise an error when crs/zunit and spunit are different

#' GPR survey data
#'
#' An S4 class to represent a collection of \eqn{k} GPR data. 
#' An object of the class `GPRsurvey` does only contain the links to 
#' the GPR data files as well as information about the GPR data such as 
#' coordinates, frequencies, antenna separation etc.  
#' The methods of the `GPRsurvey` class exclusively manipulate the trace 
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
#' @slot version   (`character[1]`) Version of RGPR.
#' @slot names     (`character[k]`) Names of the GPR data.
#' @slot paths     (`character[k]`) File paths of the original GPR data.
#' @slot descs     (`character[k]`) Descriptions of the GPR data.
#' @slot modes     (`character[k]`) Survey modes of the GPR data
#'                 (e.g., `"CO"`, `"CMP"`).
#' @slot dates     (`Date[k]`) Date of the GPR data (class `Date`,
#'                 e.g., `Sys.Date()`).
#' @slot freqs     (`numeric[k]`) Antennae frequency of the GPR data 
#'                 (in MHz).               
#' @slot antseps   (`numeric[k]`) Antennae separation of the GPR data 
#' @slot spunit    (`character[1]`) Spatial units.
#' @slot crs       (`character[1]`) Coordinate reference system in wkt format (sf package)
#' @slot coords    (`list[k]`) GPR data coordinates
#' @slot intersections (`list[k]`) Intersections coordinates of the GPR data
#' @slot markers   (`list[k]`) Fiducial markers of the GPR data
#' @slot nz        (`integer[k]`) Number of samples in each GPR data.
#' @slot zlengths  (`numeric[k]`) Range along `z` in each GPR data.
#' @slot zunits    (`character[1]`) Unit of `z`.
#' @slot nx        (`integer[k]`) Number of traces in each GPR data.
#' @slot xlengths  (`numeric[k]`) Length of the GPR data.
#' @slot transf  (`numeric(5)`) Translation vector before rotation, after and rotation angle
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
