
# =============================================================================
# HDF5 writer  (internal)
# =============================================================================

# =============================================================================
# HDF5 write logic  (internal)
# =============================================================================

#' HDF5 write helper for writeGPR("GPRsurvey")
#'
#' Three cases:
#'
#' \enumerate{
#'   \item \code{dsn} is \code{NULL} or identical to \code{obj@@filepath}:
#'         the file is already current --- no-op, return \code{obj}.
#'   \item \code{dsn} is a different path: copy the backing HDF5 file there
#'         and return an updated \code{obj} pointing at the new location.
#'   \item \code{obj@@filepath} no longer exists: raise an informative error
#'         asking the user to re-create with \code{GPRsurvey()}.
#' }
#'
#' @keywords internal
.writeGPR_h5 <- function(obj, dsn, overwrite, compress) {
  
  src <- obj@filepath
  
  # Case 1 -------------------------------------------------------------------
  if (is.null(dsn) || identical(normalizePath(dsn,  mustWork = FALSE),
                                normalizePath(src,  mustWork = FALSE))) {
    if (!file.exists(src)) {
      stop("The backing HDF5 file no longer exists: '", src, "'.\n",
           "Re-create the GPRsurvey object with GPRsurvey().",
           call. = FALSE)
    }
    message("HDF5 file is already up to date: '", src, "'")
    return(invisible(obj))
  }
  
  # Case 2 -------------------------------------------------------------------
  dst <- normalizePath(dsn, mustWork = FALSE)
  
  if (file.exists(dst)) {
    if (!overwrite) {
      stop("File already exists: '", dst, "'.\n",
           "Use overwrite = TRUE to replace it.",
           call. = FALSE)
    }
    file.remove(dst)
  }
  
  if (!file.exists(src)) {
    stop("The backing HDF5 file no longer exists: '", src, "'.\n",
         "Re-create the GPRsurvey object with GPRsurvey().",
         call. = FALSE)
  }
  
  ok <- file.copy(src, dst)
  if (!ok) stop("Failed to copy '", src, "' to '", dst, "'.", call. = FALSE)
  message("GPRsurvey HDF5 file copied to: '", dst, "'")
  
  obj@filepath <- dst
  invisible(obj)
}



# #' Write a GPRsurvey object to an HDF5 file
# #'
# #' @param x         Object of class \code{GPRsurvey}.
# #' @param file      (`character(1)`) Output `.h5` file path.
# #' @param overwrite (`logical(1)`) Overwrite existing file?
# #' @param compress  (`integer(1)`) gzip level 0–9 for the data array.
# #'
# #' @return Invisibly returns `file`.
# #' @keywords internal
# .writeGPRsurvey_hdf5 <- function(x, file, overwrite = FALSE, compress = 5L) {
#   
#   if (!requireNamespace("hdf5r", quietly = TRUE)) {
#     stop("Package 'hdf5r' is required for HDF5 output.\n",
#          "Install it with: install.packages('hdf5r')",
#          call. = FALSE)
#   }
#   
#   stopifnot(inherits(x, "GPRsurvey"))
#   
#   if (file.exists(file)) {
#     if (!overwrite) {
#       stop("File already exists: '", file, "'.\n",
#            "Use overwrite = TRUE to replace it.",
#            call. = FALSE)
#     }
#     file.remove(file)
#   }
#   
#   h5 <- hdf5r::H5File$new(file, mode = "w")
#   on.exit(h5$close_all(), add = TRUE)
#   
#   # ---- Root attributes ------------------------------------------------------
#   h5$create_attr("version",  x@version)
#   h5$create_attr("software", "RGPR")
#   h5$create_attr("created",  format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
#   
#   # ---- Survey-level metadata group ------------------------------------------
#   sg <- h5$create_group("survey")
#   sg$create_attr("crs",    if (is.na(x@crs))  "" else x@crs)
#   sg$create_attr("spunit", x@spunit)
#   
#   sg[["names"]]    <- x@names
#   sg[["descs"]]    <- x@descs
#   sg[["modes"]]    <- x@modes
#   sg[["dates"]]    <- format(x@dates, "%Y-%m-%d")
#   sg[["freqs"]]    <- x@freqs
#   sg[["antseps"]]  <- x@antseps
#   sg[["nz"]]       <- x@nz
#   sg[["nx"]]       <- x@nx
#   sg[["zlengths"]] <- x@zlengths
#   sg[["xlengths"]] <- x@xlengths
#   sg[["zunits"]]   <- x@zunits
#   
#   # ---- Per-line groups ------------------------------------------------------
#   lg <- h5$create_group("lines")
#   for (nm in x@names) {
#     gpr <- x[[nm]]    # calls the GPRsurvey [[ method to load the GPR object
#     .write_GPR_line_hdf5(lg, name = nm, gpr = gpr, compress = compress)
#     message("Written: ", nm)
#   }
#   
#   invisible(file)
# }


# =============================================================================
# Per-line writer  (internal)
# =============================================================================

#' Write a single GPR line into an open HDF5 group
#'
#' @param parent_grp  An open \code{hdf5r} group object (the `/lines` group).
#' @param name        (`character(1)`) Name for the new sub-group.
#' @param gpr         Object of class \code{GPR}.
#' @param compress    (`integer(1)`) gzip level 0–9.
#'
#' @return Invisibly returns the created group object.
#' @keywords internal
.write_GPR_line_hdf5 <- function(parent_grp, name, gpr, compress = 5L) {
  
  grp <- parent_grp$create_group(name)
  nz  <- nrow(gpr)
  nx  <- ncol(gpr)
  
  # ---- Scalar attributes (light metadata — no dataset overhead) -------------
  grp$create_attr("name",    gpr@name)
  grp$create_attr("date",    format(gpr@date, "%Y-%m-%d"))
  grp$create_attr("freq",    gpr@freq[1L])   # store first/only value
  grp$create_attr("antsep",  if (length(gpr@antsep) == 1L) gpr@antsep
                  else gpr@antsep[1L])
  grp$create_attr("mode",    gpr@mode)
  grp$create_attr("crs",     if (is.na(gpr@crs))  "" else gpr@crs)
  grp$create_attr("dunit",   gpr@dunit)
  grp$create_attr("xunit",   gpr@xunit)
  grp$create_attr("zunit",   gpr@zunit)
  grp$create_attr("version", gpr@version)
  grp$create_attr("desc",    gpr@desc)
  grp$create_attr("spunit",  gpr@spunit)
  
  # ---- Data array  (chunked + compressed) -----------------------------------
  ds <- grp$create_dataset(
    name       = "data",
    robj       = array(0, dim = c(nz, nx)),
    dtype      = hdf5r::h5types$H5T_NATIVE_FLOAT,
    chunk_dims = c(nz, min(nx, 128L)),
    gzip_level = compress
  )
  ds[1:nz, 1:nx] <- gpr@data
  
  # ---- Axes -----------------------------------------------------------------
  grp[["z"]]       <- gpr@z
  grp[["x"]]       <- gpr@x
  grp[["z0"]]      <- gpr@z0
  grp[["markers"]] <- gpr@markers
  
  # ---- Coordinates ----------------------------------------------------------
  cg <- grp$create_group("coords")
  if (!is.null(gpr@coord) && length(gpr@coord) > 0L && nrow(gpr@coord) > 0L) {
    cg[["xyz"]] <- gpr@coord
  }
  if (length(gpr@rec)   > 0L) cg[["rec"]]   <- gpr@rec
  if (length(gpr@trans) > 0L) cg[["trans"]] <- gpr@trans
  
  # ---- Velocity model -------------------------------------------------------
  vg <- grp$create_group("vel")
  if (!is.null(gpr@vel$v)) vg[["v"]] <- gpr@vel$v
  
  # ---- Raw manufacturer metadata --------------------------------------------
  # Only scalar atomics are stored (vectors/lists are skipped silently).
  mg <- grp$create_group("metadata")
  for (key in names(gpr@md)) {
    val <- gpr@md[[key]]
    if (is.atomic(val) && length(val) == 1L) {
      # HDF5 cannot store NA directly; convert to a sentinel string
      mg[[key]] <- if (is.na(val)) "NA" else val
    }
  }
  
  invisible(grp)
}
