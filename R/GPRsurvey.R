#------------------------------------------#
#-------------- CONSTRUCTOR ---------------#

#' Create a GPRsurvey object
#'
#' Reads a set of GPR data files, collects survey-level metadata, writes
#' everything to a single HDF5 file, and returns a lightweight
#' `GPRsurvey` object backed by that file. A `GPRsurvey` object is backed
#' by an HDF5 file. The R object contains survey-level metadata and
#' references line data stored in the backing file.
#'
#' ## How the file is written
#' The backing file is built in a **temporary file in the same directory as
#' `dsn`**, under a **lock** that prevents two processes from building/
#' replacing the same file at the same time (see `.h5_lock_acquire()` in
#' `hdf5_update.R`). Only once every line has been written, survey-level
#' metadata has been written, intersections have been computed from the
#' *final* coordinates of every line, and (by default) every dataset has
#' been read back to validate its checksum, is the temporary file atomically
#' swapped in for `dsn` (`file.rename()`). If anything fails partway through
#' -- a malformed input file, a disk error, an interrupted session -- `dsn`
#' is left completely untouched: either the previous file (if `overwrite =
#' TRUE` and one existed) or nothing at all. You never end up with a
#' truncated/corrupt file sitting at the path you expect a valid backup.
#'
#' ## Precision and compression
#' The main data array is always stored as 64-bit floating point
#' (`H5T_NATIVE_DOUBLE`), matching R's native numeric precision -- so
#' writing to HDF5 never loses precision relative to the in-memory `GPR`
#' object. Every dataset (including small metadata vectors) is chunked and
#' protected with an HDF5 fletcher32 checksum. gzip compression (with a
#' byte-shuffle pre-filter, which typically improves the ratio noticeably
#' for floating-point data) is applied to the radar-data arrays and, for
#' large surveys, to per-line coordinates; see `compress`.
#'
#' Whether compression is worth it for GPR data depends on the data:
#' amplitude-sampled radargrams are noisy and don't compress as well as,
#' say, images with large flat regions, so don't expect dramatic ratios --
#' but shuffle+gzip typically still buys a modest (roughly 1.3-2x)
#' reduction for a low CPU cost, which is usually worth it for a backup
#' copy that is written once and read occasionally. If you process huge
#' surveys very frequently and disk space is not a concern, set
#' `compress = 0L` to skip compression entirely and maximize write/read
#' speed.
#'
#' @param x        (`character[k]`) Vector of `k` file paths to GPR data
#'                 files. All formats supported by [RGPR::readGPR()] are accepted.
#' @param dsn      (`character(1)`) Path for the output HDF5 file (must end
#'                 in `.h5` by convention). If it already exists and
#'                 `overwrite = FALSE`, an error is raised before any work
#'                 is done; if `overwrite = TRUE`, the existing file is only
#'                 replaced at the very end, once the new file has been
#'                 fully built and verified (see Details).
#' @param name     (`character(1)`) Name of the survey.
#' @param desc     (`character(1)`) Description of the survey.
#' @param overwrite (`logical(1)`) Overwrite an existing HDF5 file? Default
#'                 `FALSE`.
#' @param compress (`integer(1)`) gzip compression level 0-9 for the data
#'                 arrays inside the HDF5 file; `0` disables compression.
#'                 Default `5L`. See Details for guidance on whether
#'                 compression is worth it for GPR data.
#' @param verify   (`logical(1)`) Re-read every dataset after writing to
#'                 validate checksums before the file is swapped in.
#'                 Default `TRUE`.
#' @param verbose  (`logical(1)`) Print progress messages.
#' @param ...      Additional arguments passed to [RGPR::readGPR()].
#'
#' @return An object of class `GPRsurvey`.
#'
#' @seealso [RGPR::readGPRsurvey()], [RGPR::writeGPR()]
#' @name GPRsurvey
#' @export
GPRsurvey <- function(x, dsn,
                       name = "", desc = "",
                       overwrite = FALSE, compress = 5L,
                       verify = TRUE, verbose = TRUE, ...) {

  if (!requireNamespace("hdf5r", quietly = TRUE)) {
    stop("Package 'hdf5r' is required to create a GPRsurvey object.\n",
         "Install it with: install.packages('hdf5r')",
         call. = FALSE)
  }

  # ---- validate arguments ---------------------------------------------------
  if (missing(dsn) || !nzchar(dsn)) {
    stop("Argument 'dsn' is required: provide a path for the HDF5 output ",
         "dsn, e.g. GPRsurvey(paths, dsn = 'survey.h5').",
         call. = FALSE)
  }
  dsn <- normalizePath(dsn, mustWork = FALSE)
  if (file.exists(dsn) && !overwrite) {
    stop("File already exists: '", dsn, "'.\n",
         "Use overwrite = TRUE to replace it.",
         call. = FALSE)
  }

  compress <- as.integer(compress)
  if (length(compress) != 1L || is.na(compress) || compress < 0L || compress > 9L) {
    stop("'compress' must be an integer between 0 and 9.", call. = FALSE)
  }

  LINES <- x
  n     <- length(LINES)

  line_paths    <- LINES
  # ---- per-line accumulator vectors -----------------------------------------
  line_names    <- character(n)
  line_descs    <- character(n)
  line_modes    <- character(n)
  line_dates    <- as.Date(rep(NA, n))
  line_freq     <- numeric(n)
  line_antsep   <- numeric(n)
  line_spunit   <- character(n)
  line_xunit    <- character(n)
  line_crs      <- character(n)
  line_nz       <- integer(n)
  line_zlengths <- numeric(n)
  line_zunits   <- character(n)
  line_nx       <- integer(n)
  line_xlengths <- numeric(n)
  line_markers  <- vector("list", n)

  xyzCoords <- vector("list", n)

  # ---- acquire an exclusive lock on 'dsn' ------------------------------------
  # Prevents two R sessions from building/replacing the same backing file at
  # the same time. See .h5_lock_acquire()/.h5_lock_release() in hdf5_update.R.
  lock <- .h5_lock_acquire(dsn)

  # ---- build the file in a temporary location, then swap it in atomically --
  # dsn itself is never opened for writing directly: if anything below fails,
  # dsn (whatever it was before this call -- possibly nothing) is untouched.
  tmp <- .h5_temp_path(dsn)

  # Registered in the exact order they must run at exit -- see the identical
  # pattern (and rationale) in .h5_update_survey() in hdf5_update.R.
  h5 <- hdf5r::H5File$new(tmp, mode = "w")
  on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  on.exit(.h5_lock_release(lock), add = TRUE)

  h5$create_attr("format_version", "1.0")   # HDF5 layout/schema version
  h5$create_attr("software",       "RGPR")
  h5$create_attr("created",        format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
  h5$create_attr("name", name)
  h5$create_attr("desc", desc)

  lg <- h5$create_group("lines")   # per-line data groups written as we go

  # ---- read and write each GPR line -----------------------------------------
  for (i in seq_along(LINES)) {
    verboseF(message("Reading ", basename(LINES[i]), " ..."), verbose = verbose)

    gpr <- verboseF(readGPR(LINES[[i]], verbose = verbose, ...), verbose = verbose)

    if (inherits(gpr, "GPRset")) {
      stop(
        "Multi-channel (GPRset) profiles are not yet supported in GPRsurvey.\n",
        "Affected file: ", LINES[[i]], "\n",
        "Track progress at: https://github.com/emanuelhuber/RGPR/issues",
        call. = FALSE
      )
    }

    # -- guard against empty profiles -----------------------------------------
    if (nrow(gpr) == 0L || ncol(gpr) == 0L) {
      stop(
        "File '", LINES[[i]], "' produced an empty GPR profile ",
        "(nz = ", nrow(gpr), ", nx = ", ncol(gpr), "); ",
        "empty profiles cannot be backed up.",
        call. = FALSE
      )
    }

    # -- unique name ------------------------------------------------------------
    line_names[i] <- if (nzchar(gpr@name[1L])) gpr@name[1L] else "default_name"
    if (i > 1L) {
      line_names[i] <- safeName(x = line_names[i], y = line_names[seq_len(i - 1L)])
    }

    # -- metadata with length-zero guards ---------------------------------------
    line_descs[i]     <- gpr@desc
    line_modes[i]      <- gpr@mode
    line_dates[i]      <- .setSlotDefault(gpr, "date",   Sys.Date(),
                                           msg = paste0(LINES[[i]], "\n date has length zero"),
                                           verbose)
    line_freq[i]       <- .setSlotDefault(gpr, "freq",   0,
                                           paste0(LINES[[i]], "\n frequency has length zero"),
                                           verbose)
    line_antsep[i]      <- .setSlotDefault(gpr, "antsep", 0,
                                            paste0(LINES[[i]], "\n antenna separation has length zero"),
                                            verbose)
    line_spunit[i]      <- gpr@spunit
    line_xunit[i]       <- gpr@xunit
    line_zunits[i]       <- gpr@zunit
    line_crs[i]          <- gpr@crs
    line_nz[i]           <- nrow(gpr)
    line_nx[i]           <- ncol(gpr)
    line_zlengths[i]     <- abs(diff(range(gpr@z)))
    line_xlengths[i]     <- abs(diff(range(gpr@x)))
    line_markers[[i]]    <- .normalizeMarkers(gpr@markers, ncol(gpr), verbose = verbose)
    # line_ann[[i]]    <- .normalizeMarkers(gpr@ann, ncol(gpr), verbose = verbose)
    # 
    # line_angles[[i]] <- gpr@angles
    # 
    # line_times[i] <- gpr@time
    # line_dlab[i] <- gpr@dlab
    # line_xlab[i] <- gpr@xlab
    # line_zlab[i] <- gpr@zlab
    
    xyzCoords[[i]] <- gpr@coord
    if (ncol(gpr@coord) == 3L) colnames(xyzCoords[[i]]) <- c("x", "y", "z")

    # -- write GPR line to HDF5 using the finalised name ------------------------
    .write_GPR_line_hdf5(lg, name = line_names[i], gpr = gpr, compress = compress)
    # .write_GPR_line_hdf5(lg, name = .h5_line_group_id(i), gpr = gpr, compress = compress)
    
    verboseF(message("  Written to HDF5: ", line_names[i]), verbose = verbose)
  }

  # ---- resolve survey-level CRS and spatial unit -----------------------------
  if (length(unique(line_crs)) > 1L && isTRUE(verbose)) {
    warning(
      "Not all coordinate reference systems (CRS) are identical.\n",
      "Using the first valid CRS.",
      call. = FALSE
    )
  }
  survey_crs    <- .checkCRS(line_crs[!is.na(line_crs)][1L])
  survey_spunit <- if (is.na(survey_crs)) {
    line_xunit[!is.na(line_xunit)][1L]
  } else {
    crsUnit(survey_crs)
  }

  # ---- assemble the S4 object -------------------------------------------------
  survey <- new("GPRsurvey",
                version   = "0.3",
                path      = dsn,
                name      = name,
                desc      = desc,

                paths     = line_paths,
                names     = line_names,
                descs     = line_descs,
                modes     = line_modes,
                dates     = line_dates,
                freqs     = line_freq,
                antseps   = line_antsep,
                spunit    = survey_spunit,
                crs       = if (is.na(survey_crs)) NA_character_ else survey_crs,
                coords    = xyzCoords,       # (x,y,z) coordinates for each profile

                markers   = line_markers,

                nz        = line_nz,
                nx        = line_nx,
                zlengths  = line_zlengths,
                xlengths  = line_xlengths,
                zunits    = line_zunits,
                transf    = numeric(0),
                view      = FALSE
  )

  # ---- compute line intersections ONCE, now that every line's coordinates
  #      are final, then persist survey metadata + intersections -------------
  survey <- findIntersection(survey)

  .write_survey_group_hdf5(h5, survey)
  .write_intersections_hdf5(h5, survey)

  h5$flush()
  h5$close_all()

  if (isTRUE(verify)) {
    verboseF(message("Verifying checksums..."), verbose = verbose)
    .h5_verify_checksums(tmp)
  }

  .h5_atomic_replace(tmp, dsn)
  verboseF(message("GPRsurvey HDF5 file written: ", dsn), verbose = verbose)

  survey
}

#' Create an empty GPRsurvey object
#'
#' Creates and initializes an empty \code{GPRsurvey} object containing
#' metadata and placeholders for \code{n} GPR profiles. The returned object
#' can subsequently be populated with imported or manually created
#' \code{GPR} datasets.
#'
#' @param n Integer. Number of profiles to initialize in the survey.
#' Must be greater than 0. Values are rounded and coerced to integer.
#'
#' @return
#' An object of class \code{\linkS4class{GPRsurvey}} with slots initialized
#' to empty values of the appropriate type and length.
#'
#' @details
#' The function allocates storage for profile-level metadata including:
#' \itemize{
#' \item file paths and profile names,
#' \item acquisition dates,
#' \item antenna frequencies,
#' \item antenna separations,
#' \item coordinate reference system (CRS),
#' \item spatial coordinates,
#' \item profile dimensions and units.
#' }
#'
#' The survey-level fields \code{path}, \code{name}, and \code{desc} are
#' initialized as empty character vectors, while profile-specific
#' information is stored in the corresponding plural slots
#' (\code{paths}, \code{names}, \code{descs}, etc.).
#'
#' @seealso
#' \code{\linkS4class{GPRsurvey}},
#' \code{\link{GPRsurvey}}
#'
#' @examples
#' \dontrun{
#' # Create an empty survey containing a single profile
#' s <- GPRsurveyInit()
#'
#' # Create an empty survey for 10 profiles
#' s <- GPRsurveyInit(10)
#'
#' # Check number of allocated profiles
#' length(s@paths)
#' }
#' @export
GPRsurveyInit <- function(n = 1){
  n <- as.integer(round(n))
  if(n < 1) stop("'n' must be strictly positiv!")
  new("GPRsurvey",
      version       = "0.3",
      path          = character(n),      
      name          = character(1),     
      desc          = character(1),
      
      paths          = character(n),      
      names          = character(n),     
      descs          = character(n),
      modes      = character(n),
      dates     = as.Date(rep(NA, n)),
      freqs     = numeric(n),
      antseps   = numeric(n),
      spunit    = NA_character_,
      crs       = NA_character_,
      coords    = vector("list", n),       # (x,y,z) coordinates for each profile
      
      markers   = vector("list", n),
      
      nz        = integer(n),
      nx        = integer(0),
      zlengths  = numeric(n),
      xlengths  = numeric(n),
      zunits    = character(n),
      transf    = numeric(0),
      view      = FALSE

  )
}


# Compute the orientation angle of GPR profile
# TODO: compute all the angles (maybe not a good idea...)
gprAngle <- function(x){
  #dEN <- x@coord[1:(length(x) - 1),1:2] - x@coord[2:length(x),1:2]
  # return(atan2(dEN[1,2], dEN[1,1]))
  dEN <- x@coord[1,1:2] - tail(x@coord[,1:2],1)
  return(atan2(dEN[2], dEN[1]))
}

# is angle b between aref - 1/2*atol and aref + 1/2*atol?
inBetAngle <- function(aref, b, atol = pi/10){
  dot <- cos(b)*cos(aref) + sin(b) * sin(aref)
  return(acos(dot) <= atol)
}

