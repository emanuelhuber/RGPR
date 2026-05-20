# =============================================================================
# io-registry.R
#
# Format registry infrastructure for readGPR().
#
# A format descriptor is a named list with the following fields:
#
#   detect_ext  character vector (uppercase) of file extensions that trigger
#               this format. The dispatcher matches against the extensions
#               present in the user-supplied `dsn`.
#
#   mandatory   Named character vector: extension -> uppercase extension.
#               These files MUST be present (either supplied or discoverable
#               from the primary path).  The first element is treated as the
#               PRIMARY file (the one whose basename is used for fName/fPath).
#
#   optional    Named character vector: extension -> uppercase extension.
#               These files are resolved with throwError = FALSE.
#
#   gps_ext    character(1) or NULL.  The slot name (from `optional` or
#               `mandatory`) whose value, once resolved, carries GPS/position
#               data that should be passed to .maybe_interp_gps().
#               NULL means the format has no GPS companion file.
#
#   reader_fn   function(dsn_list, fName, fPath, desc, Vmax, verbose, ...)
#               Returns a GPR or GPRset object plus an `x_gps` attribute
#               when the format handles GPS internally (some formats read GPS
#               inside the reader; others leave it to the dispatcher via
#               gps_ext). The function is called with verboseF() by the
#               dispatcher.
#
# Registration is performed by register_gpr_format(), which is called at the
# bottom of each format-specific io-*.R file.  The registry lives in the
# package namespace as .GPR_FORMAT_REGISTRY.
# =============================================================================

#' @keywords internal
.GPR_FORMAT_REGISTRY <- list()


#' Register a GPR file format
#'
#' Called once per format (typically at the bottom of each `io-*.R` file) to
#' add a format descriptor to the package-level registry.
#'
#' @param id          (`character(1)`) Unique format identifier, e.g. `"DT1"`.
#' @param detect_ext  (`character`) One or more uppercase extensions that
#'                    identify this format (e.g. `c("SGY", "SEGY")`).
#' @param mandatory   (`character`) Named vector of required extensions,
#'                    slot name as names, e.g.
#'                    `c(DT1 = "DT1", HD = "HD")`.
#'                    The first element is the primary file.
#' @param optional    (`character`) Named vector of optional extensions.
#'                    Defaults to `character(0)`.
#' @param gps_ext    (`character(1)` or `NULL`) Name of the optional slot
#'                    whose resolved path is the GPS companion file.
#'                    `NULL` if the format has no GPS file or handles GPS
#'                    internally.
#' @param reader_fn   (`function`) Format-specific reader.  Signature:
#'                    `function(dsn, fName, fPath, desc, Vmax, verbose, ...)`.
#'
#' @keywords internal
#' @export
register_gpr_format <- function(id, detect_ext, mandatory, optional = character(0),
                                gps_ext = NULL, reader_fn) {
  stopifnot(
    is.character(id),        length(id) == 1L,
    is.character(detect_ext), length(detect_ext) >= 1L,
    is.character(mandatory),  length(mandatory)  >= 1L, !is.null(names(mandatory)),
    is.character(optional),                              (length(optional) == 0L || !is.null(names(optional))),
    is.null(gps_ext) || (is.character(gps_ext) && length(gps_ext) == 1L),
    is.function(reader_fn)
  )
  .GPR_FORMAT_REGISTRY[[id]] <<- list(
    detect_ext = toupper(detect_ext),
    mandatory  = mandatory,         # named, uppercase extensions
    optional   = optional,          # named, uppercase extensions
    gps_ext   = gps_ext,
    reader_fn  = reader_fn
  )
}


#' Detect the format of a supplied set of file extensions
#'
#' Iterates over the registry in insertion order and returns the first
#' descriptor whose `detect_ext` intersects with `ext_vec`.
#'
#' @param ext_vec (`character`) Uppercase extensions extracted from `dsn`.
#' @return The matching format descriptor list, or `NULL` if none matched.
#'
#' @keywords internal
.detect_format <- function(ext_vec) {
  ext_upper <- toupper(ext_vec)
  for (fmt in .GPR_FORMAT_REGISTRY) {
    if (any(fmt$detect_ext %in% ext_upper)) return(fmt)
  }
  NULL
}

# =============================================================================
# io-resolve.R
#
# Shared helpers for resolving companion file paths and normalising the `dsn`
# argument before format-specific readers are called.
#
# The key public function is resolve_companion_files(), which is called by
# the dispatcher (io-dispatch.R) after a format has been detected.
# =============================================================================


# -----------------------------------------------------------------------------
# resolve_companion_files()
# -----------------------------------------------------------------------------
#
# Given:
#   - `dsn`   : list of connections/paths, names = UPPERCASE extension
#   - `fPath` : named character vector, names = UPPERCASE extension
#   - `fmt`   : a format descriptor from .GPR_FORMAT_REGISTRY
#
# Returns an updated `dsn` list that is guaranteed to contain at least all
# mandatory slots (aborting with an informative error if a mandatory connection
# is absent when working with raw connections), and all discoverable optional
# slots (set to NULL when the companion file does not exist on disk).
#
# The first mandatory slot is the PRIMARY file.  Its fPath entry is used as
# the base path from which companion files are located.

#' Resolve companion file paths for a GPR format
#'
#' @param dsn   Named list, slot name (UPPERCASE ext) -> path or connection.
#' @param fPath Named character vector, UPPERCASE ext -> absolute file path.
#' @param fmt   Format descriptor from `.GPR_FORMAT_REGISTRY`.
#' @return Updated `dsn` list with all mandatory + optional slots populated.
#'
#' @keywords internal
resolve_companion_files <- function(dsn, fPath, fmt) {

  all_connections <- all(sapply(dsn, inherits, "connection"))

  if (all_connections) {
    # ---- connection mode: user is responsible for supplying all mandatory
    #      files; we only verify they are present.
    missing_mandatory <- setdiff(names(fmt$mandatory), toupper(names(dsn)))
    if (length(missing_mandatory) > 0L) {
      stop(
        "Missing connection(s) for mandatory file(s): ",
        paste0("*.", tolower(missing_mandatory), collapse = ", "),
        call. = FALSE
      )
    }
    return(dsn)
  }

  # ---- path mode: resolve companion paths from the primary file path --------
  primary_ext  <- fmt$mandatory[[1L]]   # e.g. "DT1"

  # Identify the primary file path: it may already be in fPath under its own
  # extension, or under one of the detect_ext variants (e.g. RD3/RD7).
  # primary_key <- intersect(names(fPath), fmt$detect_ext)
  primary_key <- names(fPath)[names(fPath) %in% fmt$detect_ext]
  
  primary_ext   <- names(fmt$mandatory)[1L]   # e.g. "RD3"
  primary_path  <- fPath[[primary_ext]]

  if (is.null(primary_path)) {
    # primary file wasn't in the user's dsn; try to recover it from
    # any detect_ext key that IS present (e.g. user passed .rd3 directly)
    detect_key   <- base::intersect(names(fPath), fmt$detect_ext)[1L]
    primary_path <- fPath[[detect_key]]
  }

  if (is.null(primary_path)) {
    stop("Cannot locate primary file path for format ", primary_ext, call. = FALSE)
  }

  if (length(primary_key) == 0L) {
    stop("Cannot locate primary file path for format ", primary_ext,
         call. = FALSE)
  }
  base_path <- fPath[[primary_key[1L]]]   # e.g. "/data/DAT_0052.rd3"

  # Mandatory companions -------------------------------------------------------
  for (slot in names(fmt$mandatory)) {
    ext <- fmt$mandatory[[slot]]
    if (is.null(dsn[[slot]])) {
      resolved <- getFName(base_path, ext = paste0(".", ext))[[tolower(ext)]]
      dsn[[slot]] <- resolved   # getFName() throws if mandatory file absent
    }
  }

  # Optional companions --------------------------------------------------------
  for (slot in names(fmt$optional)) {
    ext <- fmt$optional[[slot]]
    if (is.null(dsn[[slot]])) {
      resolved <- getFName(base_path, ext = paste0(".", ext),
                           throwError = FALSE)[[tolower(ext)]]
      dsn[[slot]] <- resolved   # NULL when file does not exist
    }
  }

  dsn
}


# -----------------------------------------------------------------------------
# .normalise_dsn()
# -----------------------------------------------------------------------------
# Accepts the raw user-supplied `dsn` (character, connection, or list thereof)
# and returns a named list ready for extension-based dispatch.
#
# Returns a list with:
#   $dsn   : named list, UPPERCASE_EXT -> path/connection
#   $fPath : named character, UPPERCASE_EXT -> absolute path
#   $fName : named character, UPPERCASE_EXT -> filename without directory
#   $ext   : character vector of UPPERCASE extensions

#' Normalise the \code{dsn} argument into a named list
#'
#' @param dsn Raw user input (character path, connection, or list thereof).
#' @return List with `$dsn`, `$fPath`, `$fName`, `$ext`.
#'
#' @keywords internal
.normalise_dsn <- function(dsn) {

  # Ensure dsn is a list so we can iterate uniformly
  if (!is.list(dsn)) {
    if (inherits(dsn, "connection")) {
      dsn <- list(dsn)
    } else {
      dsn <- as.list(dsn)
    }
  }
  dsn <- Filter(Negate(is.null), dsn)

  # Derive file paths (works for both characters and connections)
  fPath <- sapply(dsn, getFPath, USE.NAMES = FALSE)
  ext   <- sapply(fPath, .fExt,      USE.NAMES = FALSE)
  fName <- sapply(fPath, .fNameWExt, USE.NAMES = FALSE)

  ext_upper <- toupper(ext)
  names(dsn)   <- ext_upper
  names(fPath) <- ext_upper
  names(fName) <- ext_upper

  list(dsn = dsn, fPath = fPath, fName = fName, ext = ext_upper)
}