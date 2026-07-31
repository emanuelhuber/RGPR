# =============================================================================
# io-dispatch.R
#
# Public entry point: readGPR()
#
# This file contains only the dispatcher and two small post-processing helpers
# (.maybe_interp_gps, .maybe_set_cmp_mode).  All format knowledge lives in the
# format-specific io-*.R files.
# =============================================================================


#' Read a GPR data file
#'
#' Read GPR data file from various manufacturers and interpolate trace
#' positions.
#'
#' @section Supported file formats:
#'
#' \tabular{llll}{
#' **Manufacturer**        \tab **Mandatory files** \tab **Optional GPS files**  \tab **Other optional files**  \cr
#' Sensors & Software      \tab **.dt1**, .hd       \tab .gps                    \tab                           \cr
#' MALA  16 bits           \tab **.rd3**, .rad      \tab .cor                    \tab                           \cr
#' MALA  32 bits           \tab **.rd7**, .rad      \tab .cor                    \tab                           \cr
#' ImpulseRadar            \tab **.iprb**, .iprh    \tab .cor                    \tab .time, .mrk               \cr
#' GSSI                    \tab **.dzt**            \tab .dzg                    \tab .dzx                      \cr
#' Geomatrix (UTSI)        \tab **.dat**, .hdr      \tab .gps, .gpt              \tab                           \cr
#' Radar Systems / SEG-Y   \tab **.sgy/.segy**      \tab                         \tab                           \cr
#' 3d-Radar                \tab **.vol**            \tab                         \tab                           \cr
#' R internal format       \tab **.rds**            \tab                         \tab                           \cr
#' Text files              \tab **.txt**            \tab                         \tab                           \cr
#' }
#'
#' @section Notes:
#'
#' \itemize{
#'   \item If the class of `dsn` is character, `readGPR` is
#'         insensitive to the case of the extension (.DT1 or .dt1).
#'   \item If `dsn` is a list of connections or a character vector, the
#'         order of the elements does not matter.
#'   \item If you use connections, `dsn` must contain at least all the
#'         connections to the mandatory files (in any order). If there is
#'         more than one mandatory file, use a list of connections.
#'   \item If you use a file path for `dsn` (character), you only need to
#'         provide the path to the primary mandatory file (marked in bold in
#'         the table): RGPR will find the other files automatically if they
#'         share the same base name and differ only in extension. If the files
#'         have different names, supply at least the paths to all mandatory
#'         files.
#'   \item If an optional GPS data file is passed in `dsn` or is found
#'         on disk, it will be read even if `interpGPS = FALSE`. The formatted
#'         content is stored as metadata and can be retrieved with
#'         \code{metadata(x)$GPS}.
#'   \item When `interpGPS = TRUE` and a GPS file with longitude/latitude
#'         data exists, the coordinates are by default projected into the
#'         corresponding UTM (WGS 84) zone (see \code{\link{interpCoords}}).
#'   \item Clipped signal values are estimated from the bit depth and stored
#'         as metadata; retrieve with \code{metadata(x)$clip}.
#' }
#'
#' @param dsn       (`character|connection`) Data source name: either the
#'                  filepath to the GPR data (character), or an open file
#'                  connection (can be a vector of file paths or connections).
#' @param desc      (`character(1)`) Short description of the data.
#' @param Vmax      (`numeric(1)|NULL`) Nominal analog input voltage for the
#'                  bits-to-volt transformation. `NULL` skips conversion.
#' @param verbose   (`logical(1)`) If `FALSE`, all messages and warnings are
#'                  suppressed (use with care).
#' @param interpGPS (`logical(1)`) Should trace positions be interpolated from
#'                  GPS data when available?
#' @param UTM       (`logical(1)|character(1)`) If `TRUE`, geographic
#'                  (lon/lat WGS84) coordinates are projected to UTM WGS84.
#'                  Only used when `interpGPS = TRUE`.
#' @param ...       Additional parameters passed to \code{\link{interpCoords}}.
#'
#' @return (`GPR|GPRset`) An object of class `GPR`, or `GPRset` for
#'         multi-channel data.
#'
#' @seealso [RGPR::writeGPR()], [RGPR::interpCoords()], [RGPR::metadata()]
#' @examples
#' \dontrun{
#' # File path
#' x1 <- readGPR(dsn = "data/RD3/DAT_0052.rd3")
#' y1 <- readGPR("data/FILE____050.DZT")
#'
#' # Connection
#' con  <- file("data/RD3/DAT_0052.rd3", "rb")
#' con2 <- file("data/RD3/DAT_0052.rad", "rt")
#' x2   <- readGPR(dsn = list(con, con2))
#' }
#' @name readGPR
#' @rdname readGPR
#' @export
readGPR <- function(dsn, desc = "", Vmax = NULL,
                    verbose = TRUE, interpGPS = TRUE,
                    UTM = TRUE, ...) {

  # ---- argument validation ---------------------------------------------------
  msg <- checkArgInit()
  .is_dsn_element <- function(x){
     inherits(x, "character") || inherits(x, "connection")
  }
  if (length(dsn) > 1L) {
    test <- sapply(dsn, .is_dsn_element, USE.NAMES = FALSE)
  } else {
    test <- .is_dsn_element(dsn)
  }
  if (!all(test)) {
    msg <- c(msg, 
    "arg 'dsn': Must be a character or a connection of length one or more\n")
  }
  msg <- checkArg(desc,      msg, "STRING")
  msg <- checkArg(Vmax,      msg, "NUMERIC1_NULL", Inf)
  msg <- checkArg(verbose,   msg, "LOGICAL_LEN",   1)
  msg <- checkArg(interpGPS, msg, "LOGICAL_LEN",   1)
  checkArgStop(msg)
  # ---- end argument validation -----------------------------------------------

  # ---- normalise dsn into a named list indexed by UPPERCASE extension --------
  norm  <- .normalise_dsn(dsn)
  dsn   <- norm$dsn
  fPath <- norm$fPath
  fName <- norm$fName
  ext   <- norm$ext

  # ---- detect format ---------------------------------------------------------
  fmt <- .detect_format(ext)
  if (is.null(fmt)) {
    stop(
      "File extension not recognised: ", paste(unique(ext), collapse = ", "), "\n",
      "Supported extensions: DT1, RD3, RD7, IPRB, DZT, DAT, SGY, SEGY, VOL, RDS, TXT.",
      call. = FALSE
    )
  }

  # ---- resolve companion files (paths or connections) ------------------------
  dsn <- resolve_companion_files(dsn, fPath, fmt)

  # ---- derive fName / fPath for the primary slot after resolution ------------
  primary_slot <- names(fmt$mandatory)[1L]

  # For RDS the primary path is already in dsn[[primary_slot]]
  primary_path <- if (!is.null(fPath[[primary_slot]])) {
    fPath[[primary_slot]]
  } else {
    # Fallback: recover from the resolved dsn entry (character path)
    if (is.character(dsn[[primary_slot]])) dsn[[primary_slot]] else ""
  }
  primary_fName <- .fNameWExt(primary_path)

  # ---- call the format-specific reader ---------------------------------------
  result <- verboseF(
    fmt$reader_fn(dsn,
                  fName   = primary_fName,
                  fPath   = primary_path,
                  desc    = desc,
                  Vmax    = Vmax,
                  verbose = verbose,
                  ...),
    verbose = verbose
  )

  # The reader returns either:
  #   - a GPR/GPRset object directly (x_gps handled internally, e.g. RDS), or
  #   - a list(x = <GPR>, x_gps = <sf|NULL>) when the dispatcher must handle
  #     GPS interpolation.
  if (is.list(result) && !is.null(result$x)) {
    x     <- result$x
    x_gps <- result$x_gps
  } else {
    # Reader returned the GPR object directly (no GPS companion)
    x     <- result
    x_gps <- NULL
  }

  # ---- GPS interpolation / storage -------------------------------------------
  x <- .maybe_interp_gps(x, x_gps, dsn,
                          interpGPS = interpGPS, UTM = UTM,
                          verbose   = verbose, ...)

  # ---- CMP mode post-processing ----------------------------------------------
  x <- .maybe_set_cmp_mode(x)

  return(x)
}


# =============================================================================
# Post-processing helpers (shared across all formats)
# =============================================================================

#' Apply GPS interpolation or store GPS as metadata
#'
#' @param x         GPR object.
#' @param x_gps     GPS data returned by the reader (sf object or NULL).
#' @param dsn       Resolved dsn list (used only for the "no GPS found" warning).
#' @param interpGPS logical(1).
#' @param UTM       logical(1) or character(1).
#' @param verbose   logical(1).
#' @param ...       Passed to interpCoords().
#' @return Updated GPR object.
#'
#' @keywords internal
.maybe_interp_gps <- function(x, x_gps, dsn,
                               interpGPS, UTM, verbose, ...) {
  if (!is.null(x_gps)) {
    if (!inherits(x_gps, "sf")) stop("'x_gps' must inherit 'sf'!", call. = FALSE)

    if (interpGPS) {
      if (verbose) message("Coordinates interpolation from GPS data")
      x <- tryCatch({
        dots     <- list(...)
        r        <- dots[["r"]]
        interp3D <- dots[["interp3D"]] %||% FALSE
        tol      <- dots[["tol"]]
        plot     <- dots[["plot"]]    %||% FALSE
        method   <- dots[["method"]]  %||% c("linear", "linear", "linear")
        if (length(method) != 3L) {
          if (verbose) warning("'method' must have 3 elements. Using defaults.")
          method <- c("linear", "linear", "linear")
        }
        interpCoords(x, x_gps, tt = NULL, r = r,
                     UTM      = UTM,
                     interp3D = interp3D,
                     tol      = tol,
                     verbose  = verbose,
                     plot     = plot,
                     method   = method)
      },
      error = function(cond) {
        if (verbose) {
          message("I could not interpolate the GPS data. ",
                  "You can retrieve the GPS data with `metadata(x)$GPS`")
        }
        x@md[["GPS"]] <- x_gps
        x
      })
    } else {
      x@md[["GPS"]] <- x_gps
      if (verbose) {
        message("GPS coordinates found. Retrieve them with `metadata(x)$GPS`")
      }
    }
  } else if (isTRUE(verbose) && isTRUE(interpGPS) && !is.null(dsn[["GPS"]])) {
    # A GPS slot was resolved but produced no data
    warning(x@name, ": Either no GPS file was found or it contained no ",
            "coordinates.", call. = FALSE)
  }
  x
}


#' Set CMP mode fields when the acquisition mode is CMP
#'
#' @param x GPR object.
#' @return Updated GPR object.
#'
#' @keywords internal
.maybe_set_cmp_mode <- function(x) {
  if (grepl("CMP", x@mode)) {
    x@mode <- "CMP"
    x@xlab <- "Antenna separation"
    if (length(x@rec) == 0L || length(x@trans) == 0L) {
      x@antsep <- x@x
    } else {
      x@antsep <- sqrt(colSums((x@rec - x@trans)^2))
    }
  }
  x
}

