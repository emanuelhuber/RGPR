# =============================================================================
# readGPRsurvey()  —  reconstruct a GPRsurvey from an existing HDF5 file
# =============================================================================

#' Read a GPRsurvey object from an HDF5 file
#'
#' Reconstructs the lightweight \code{GPRsurvey} index from the \code{/survey}
#' group of an RGPR HDF5 file.  Individual GPR lines are not loaded until
#' accessed via \code{[[}.
#'
#' @param file (`character(1)`) Path to the `.h5` file previously written by
#'             \code{\link{GPRsurvey}} or \code{\link{writeGPR}}.
#'
#' @return Object of class \code{GPRsurvey}.
#'
#' @seealso [RGPR::GPRsurvey()], [RGPR::writeGPR()]
#' @export
readGPRsurvey <- function(file) {
  
  if (!requireNamespace("hdf5r", quietly = TRUE)) {
    stop("Package 'hdf5r' is required to read a GPRsurvey HDF5 file.\n",
         "Install it with: install.packages('hdf5r')",
         call. = FALSE)
  }
  
  file <- normalizePath(file, mustWork = TRUE)
  h5   <- hdf5r::H5File$new(file, mode = "r")
  on.exit(h5$close_all(), add = TRUE)
  
  if (!h5$exists("survey")) {
    stop("'", file, "' does not appear to be an RGPR HDF5 survey file ",
         "(missing /survey group).",
         call. = FALSE)
  }
  
  sg      <- h5[["survey"]]
  crs_str <- sg$attr_open("crs")$read()
  survey_crs    <- if (nzchar(crs_str)) crs_str else NA_character_
  
  new("GPRsurvey",
      version   = h5$attr_open("version")$read(),
      filepath  = file,
      names     = sg[["names"]][],
      descs     = sg[["descs"]][],
      modes     = sg[["modes"]][],
      dates     = as.Date(sg[["dates"]][]),
      freqs     = sg[["freqs"]][],
      antseps   = sg[["antseps"]][],
      spunit    = sg$attr_open("spunit")$read(),
      crs       = survey_crs,
      nz        = as.integer(sg[["nz"]][]),
      nx        = as.integer(sg[["nx"]][]),
      zlengths  = sg[["zlengths"]][],
      xlengths  = sg[["xlengths"]][],
      zunits    = sg[["zunits"]][]
  )
}


# =============================================================================
# Internal helpers
# =============================================================================

#' Read a single GPR line from an HDF5 file
#'
#' @param file (`character(1)`) Path to the HDF5 file.
#' @param name (`character(1)`) Line name (sub-group under `/lines/`).
#'
#' @return Object of class \code{GPR}.
#' @keywords internal
.read_GPR_line_hdf5 <- function(file, name) {
  
  h5  <- hdf5r::H5File$new(file, mode = "r")
  on.exit(h5$close_all(), add = TRUE)
  
  if (!h5$exists(file.path("lines", name))) {
    stop("Line '", name, "' not found in '", file, "'.", call. = FALSE)
  }
  
  grp <- h5[["lines"]][[name]]
  nz  <- grp[["data"]]$dims[1L]
  nx  <- grp[["data"]]$dims[2L]
  
  # -- helper: read attribute, return default if absent ----------------------
  .attr <- function(grp, key, default = "") {
    tryCatch(grp$attr_open(key)$read(), error = function(e) default)
  }
  
  crs_str <- .attr(grp, "crs", "")
  crs_val <- if (nzchar(crs_str)) crs_str else NA_character_
  
  # -- coordinates (optional groups) -----------------------------------------
  coord <- matrix(numeric(0), nrow = 0L, ncol = 3L)
  rec   <- numeric(0)
  trans <- numeric(0)
  if (grp$exists("coords")) {
    cg <- grp[["coords"]]
    if (cg$exists("xyz"))   coord <- cg[["xyz"]][]
    if (cg$exists("rec"))   rec   <- cg[["rec"]][]
    if (cg$exists("trans")) trans <- cg[["trans"]][]
  }
  
  # -- velocity model --------------------------------------------------------
  vel <- list(v = NULL)
  if (grp$exists("vel") && grp[["vel"]]$exists("v")) {
    vel$v <- grp[["vel"]][["v"]][]
  }
  
  # -- raw metadata ----------------------------------------------------------
  md <- list()
  if (grp$exists("metadata")) {
    mg   <- grp[["metadata"]]
    keys <- mg$ls()$name
    for (key in keys) {
      val      <- mg[[key]][]
      md[[key]] <- if (identical(val, "NA")) NA else val
    }
  }
  
  new("GPR",
      version  = .attr(grp, "version", "0.3"),
      name     = .attr(grp, "name",    ""),
      path     = file,
      desc     = .attr(grp, "desc",    ""),
      mode     = .attr(grp, "mode",    "CO"),
      date     = as.Date(.attr(grp, "date", format(Sys.Date(), "%Y-%m-%d"))),
      freq     = as.numeric(.attr(grp, "freq",   0)),
      antsep   = as.numeric(.attr(grp, "antsep", 0)),
      crs      = crs_val,
      dunit    = .attr(grp, "dunit",  "mV"),
      xunit    = .attr(grp, "xunit",  "m"),
      zunit    = .attr(grp, "zunit",  "ns"),
      spunit   = .attr(grp, "spunit", ""),
      data     = grp[["data"]][1:nz, 1:nx],
      z        = grp[["z"]][],
      x        = grp[["x"]][],
      z0       = grp[["z0"]][],
      markers  = grp[["markers"]][],
      coord    = coord,
      rec      = rec,
      trans    = trans,
      vel      = vel,
      md       = md
  )
}


