# =============================================================================
# io-rds.R
#
# R internal format  —  RDS
#
# Mandatory : *.rds
# Optional  : (none)
#
# RDS files are written by writeGPR() and store a serialised GPR or GPRset
# object directly.  No GPS post-processing is needed; the stored object
# already has fully resolved coordinates.  The reader therefore returns the
# object directly rather than the list(x, x_gps) convention used by the
# other format readers — the dispatcher handles both return shapes.
# =============================================================================


#' Read an R-internal GPR file (.rds)
#'
#' Format-specific reader called by the dispatcher.  Not intended to be called
#' directly by users; use \code{\link{readGPR}} instead.
#'
#' @param dsn     Named list with slot \code{RDS}.
#' @param fName   (`character(1)`) Base filename of the .rds file.
#' @param fPath   (`character(1)`) Full path of the .rds file.
#' @param desc    (`character(1)`) Short data description (unused for RDS;
#'                the stored object already contains its description).
#' @param Vmax    (`numeric(1)|NULL`) Unused for RDS files.
#' @param verbose (`logical(1)`) Print progress messages.
#' @param ...     Currently unused; reserved for future use.
#'
#' @return A named list with:
#'   \item{x}{Object of class \code{GPR} or \code{GPRset}.}
#'   \item{x_gps}{\code{NULL} (RDS objects already contain coordinates).}
#'
#' @keywords internal
.read_rds <- function(dsn, fName, fPath, desc, Vmax, verbose, ...) {
  x <- verboseF(.read_RDS(dsn[["RDS"]]), verbose = verbose)
  list(x = x, x_gps = NULL)
}


# -----------------------------------------------------------------------------
# Format registration
# -----------------------------------------------------------------------------
register_gpr_format(
  id         = "RDS",
  detect_ext = "RDS",
  mandatory  = c(RDS = "RDS"),
  optional   = character(0),
  gps_ext   = NULL,
  reader_fn  = .read_rds
)

.read_RDS <- function(path){
  x <- readRDS(path)
  if(inherits(x, "GPRvirtual") || inherits(x, "GPRsurvey")){
    x@path <- path
  }else if(inherits(x, "list")){
    if(x[["version"]] == "0.1"){
      for(i in seq_along(x[['delineations']])){
        x[['delineations']][[i]][, 5] <- -x[['delineations']][[i]][, 5]
      }
    }
    if( any(x[["version"]] == c("0.1", "0.2"))){
      y <- new("GPR",
               #--- class GPRvirtual
               version = "0.3",
               name = x[['name']],
               path = x[['filepath']],
               desc = x[['description']],
               mode = x[['surveymode']],
               date = x[['date']],
               freq = x[['freq']],
               
               data  = x[['data']],
               dunit = "mV",                          # FIXME???
               dlab  = "amplitude",
               
               spunit = x[['posunit']],
               crs    = x[['crs']],
               
               xunit = x[['posunit']],
               xlab  = "position",                    # FIXME???
               
               zunit = x[['depthunit']],
               zlab  = "two-way travel time",         # FIXME???
               
               vel = list(v = x[['vel']][[1]]),                
               
               proc         = x[['proc']],
               delineations = x[['delineations']],
               md           =  x[['hd']],
               
               #--- class GPR
               z0      = x[['time0']],
               time    = x[['time']],              
               antsep  = x[['antsep']],
               markers = trimStr(x[['fid']]),
               ann     = trimStr(x[['ann']]),
               
               coord = x[['coord']],
               rec   = x[['rec']],      
               trans = x[['trans']],
               
               x = x[['pos']],
               z = x[['depth']]
               
               # angles      = ...
      )
    }else{  # version 0.3
      x <- new("GPR",
               #--- class GPRvirtual
               version = "0.3",
               name = x[['name']],
               path = x[['path']],
               desc = x[['desc']],
               mode = x[['mode']],
               date = x[['date']],
               freq = x[['freq']],
               
               data  = x[['data']],
               dunit = x[['dunit']],           # FIXME???
               dlab  = x[['dlab']],
               
               spunit = x[['spunit']],
               crs    = x[['crs']],
               
               xunit = x[['xunit']],
               xlab  = x[['xlab']],         # FIXME???
               
               zunit = x[['zunit']],
               zlab  = x[['zlab']],         # FIXME???
               
               vel = x[['vel']],                
               
               proc         = x[['proc']],
               delineations = x[['delineations']],
               md           = x[['md']],
               
               #--- class GPR
               z0      = x[['z0']],
               time    = x[['time']],              
               antsep  = x[['antsep']],
               markers = x[['markers']],
               ann     = x[['ann']],
               
               coord = x[['coord']],
               rec   = x[['rec']],      
               trans = x[['trans']],
               
               x = x[['x']],
               z = x[['z']],
               
               angles = x[['angles']]
      )
    }
    x@path <- path
  }
  return(x)
}
