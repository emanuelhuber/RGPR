#------------------------------------------#
#-------------- CONSTRUCTOR ---------------#

#' Create a GPRsurvey object
#'
#' Reads a set of GPR data files, collects survey-level metadata, writes
#' everything to a single HDF5 file, and returns a lightweight
#' \code{GPRsurvey} object backed by that file.
#'
#' @param x        (`character[k]`) Vector of \code{k} file paths to GPR data
#'                 files.  All formats supported by \code{\link{readGPR}} are
#'                 accepted.
#' @param dsn     (`character(1)`) Path for the output HDF5 file (must end
#'                 in \code{.h5} by convention).  The file is created; if it
#'                 already exists and \code{overwrite = FALSE} an error is
#'                 raised.
#' @param name    (`character(1)`) Name of the survey
#' @param desc    (`character(1)`) Description of the survey
#' @param overwrite (`logical(1)`) Overwrite an existing HDF5 file?
#'                  Default \code{FALSE}.
#' @param compress (`integer(1)`) gzip compression level 0–9 for the data
#'                 arrays inside the HDF5 file.  Default \code{5L}.
#' @param verbose  (`logical(1)`) Print progress messages.
#' @param ...      Additional arguments passed to \code{\link{readGPR}}.
#'
#' @return An object of class \code{GPRsurvey}.
#'
#' @seealso [readGPRsurvey()], [writeGPR()]
#' @name GPRsurvey
#' @export
GPRsurvey <- function(x, dsn, 
                      name = "", desc = "",
                      overwrite = FALSE, compress = 5L,
                      verbose = TRUE, ...) {
  
  if (!requireNamespace("hdf5r", quietly = TRUE)) {
    stop("Package 'hdf5r' is required to create a GPRsurvey object.\n",
         "Install it with: install.packages('hdf5r')",
         call. = FALSE)
  }
  
  # ---- validate 'dsn' argument ---------------------------------------------
  if (missing(dsn) || !nzchar(dsn)) {
    stop("Argument 'dsn' is required: provide a path for the HDF5 output ",
         "dsn, e.g. GPRsurvey(paths, dsn = 'survey.h5').",
         call. = FALSE)
  }
  dsn <- normalizePath(dsn, mustWork = FALSE)
  if (file.exists(dsn)) {
    if (!overwrite) {
      stop("File already exists: '", dsn, "'.\n",
           "Use overwrite = TRUE to replace it.",
           call. = FALSE)
    }
    file.remove(dsn)
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
  
  xyzCoords <- list()
  
  # ---- open HDF5 file for writing -------------------------------------------
  h5 <- hdf5r::H5File$new(dsn, mode = "w")
  on.exit(h5$close_all(), add = TRUE)
  
  h5$create_attr("version",  "1.0")
  h5$create_attr("software", "RGPR")
  h5$create_attr("created",  format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
  h5$create_attr("name", name)
  h5$create_attr("desc", desc)
  
  lg <- h5$create_group("lines")   # per-line data groups written as we go
  sg <- NULL                       # survey group written after the loop
  
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
    
    # -- unique name ----------------------------------------------------------
    line_names[i] <- if (nzchar(gpr@name[1L])) gpr@name[1L] else "default_name"
    if (i > 1L) {
      line_names[i] <- safeName(x = line_names[i], y = line_names[seq_len(i - 1L)])
    }
    
    # -- metadata with length-zero guards -------------------------------------
    line_descs[i]     <- gpr@desc
    line_modes[i]     <- gpr@mode
    line_dates[i]     <- .setSlotDefault(gpr, "date",   Sys.Date(),
                                          msg = paste0(LINES[[i]], "\n date has length zero"),
                                          verbose)
    line_freq[i]      <- .setSlotDefault(gpr, "freq",   0,
                                          paste0(LINES[[i]], "\n frequency has length zero"),
                                          verbose)
    line_antsep[i]    <- .setSlotDefault(gpr, "antsep", 0,
                                          paste0(LINES[[i]], "\n antenna separation has length zero"),
                                          verbose)
    line_spunit[i]    <- gpr@spunit
    line_xunit[i]     <- gpr@xunit
    line_zunits[i]    <- gpr@zunit
    line_crs[i]       <- gpr@crs
    line_nz[i]        <- nrow(gpr)
    line_nx[i]        <- ncol(gpr)
    line_zlengths[i]  <- abs(diff(range(gpr@z)))
    line_xlengths[i]  <- abs(diff(range(gpr@x)))
    
    xyzCoords[[i]]         <- gpr@coord
    if(ncol(gpr@coord) == 3 )  colnames(xyzCoords[[i]]) <- c("x", "y", "z")
    
    # -- write GPR line to HDF5 using the finalised name ----------------------
    .write_GPR_line_hdf5(lg, name = line_names[i], gpr = gpr, compress = compress)
    verboseF(message("  Written to HDF5: ", line_names[i]), verbose = verbose)
  }
  
  # ---- resolve survey-level CRS and spatial unit ----------------------------
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
  
  # ---- write survey-level metadata group ------------------------------------
  sg <- h5$create_group("survey")
  sg$create_attr("crs",    if (is.na(survey_crs)) "" else survey_crs)
  sg$create_attr("spunit", survey_spunit)
  
  sg[["names"]]    <- line_names
  sg[["descs"]]    <- line_descs
  sg[["modes"]]    <- line_modes
  sg[["dates"]]    <- format(line_dates, "%Y-%m-%d")
  sg[["freqs"]]    <- line_freq
  sg[["antseps"]]  <- line_antsep
  sg[["nz"]]       <- line_nz
  sg[["nx"]]       <- line_nx
  sg[["zlengths"]] <- line_zlengths
  sg[["xlengths"]] <- line_xlengths
  sg[["zunits"]]   <- line_zunits
  
  # ---- assemble the S4 object -----------------------------------------------
  survey <- new("GPRsurvey",
                version   = "0.3",
                path      = dsn,
                name      = name,
                desc      = desc,
                
                names     = line_names,
                descs     = line_descs,
                modes     = line_modes,
                dates     = line_dates,
                freqs     = line_freq,
                antseps   = line_antsep,
                spunit    = survey_spunit,
                crs       = if (is.na(survey_crs)) NA_character_ else survey_crs,
                coords        = xyzCoords,       # (x,y,z) coordinates for each profiles
                nz        = line_nz,
                nx        = line_nx,
                zlengths  = line_zlengths,
                xlengths  = line_xlengths,
                zunits    = line_zunits
  )
  
  # ---- compute line intersections and write to HDF5 -------------------------
  # intersect() is called on the assembled object; the result is written back
  # to the HDF5 file under /survey/intersections/ if present.
  survey <- findIntersection(survey)
  .write_intersections_hdf5(h5, survey)
  
  return(survey)
}


#' Write line intersection data into the HDF5 file
#'
#' Writes the \code{@intersections} slot (if non-empty) to
#' \code{/survey/intersections/} as a dataset of crossing coordinates.
#' Called once at the end of the constructor, after \code{intersect()}.
#'
#' @param h5     Open \code{hdf5r} H5File object.
#' @param survey \code{GPRsurvey} object returned by \code{intersect()}.
#'
#' @keywords internal
.write_intersections_hdf5 <- function(h5, survey) {
  if (!.hasSlot(survey, "intersections")) return(invisible(NULL))
  ints <- survey@intersections
  if (length(ints) == 0L)              return(invisible(NULL))
  
  ig <- h5[["survey"]]$create_group("intersections")
  for (nm in names(ints)) {
    val <- ints[[nm]]
    if (is.numeric(val) && length(val) > 0L) {
      ig[[nm]] <- val
    }
  }
  invisible(NULL)
}




#  #' Create an object of the class GPRsurvey
#  #'
#  #' Create an object of the class GPRsurvey using a vector of GPR data filepath
#  #' @param x (`character[k]`)     Vector of `k` file paths of GPR data.
#  #' @param verbose (`logical[1]`) If `TRUE` the function prints some
#  #'                                    information.
#  #' @param ...     Additional parameters to be passed to [readGPR()].
#  #' @name GPRsurvey
#  #' @export
# # LINES = list of datapath
# GPRsurvey <- function(x, verbose = TRUE, ...){
#   LINES <- x
#   n <- length(LINES)
#   line_paths    <- character(n)
#   line_names    <- character(n)
#   line_descs    <- character(n)
#   line_modes    <- character(n)
#   line_dates    <- as.Date(rep(NA, n))
#   line_freq     <- numeric(n)
#   line_antsep   <- numeric(n)
#   line_lengths  <- numeric(n)
#   line_spunit   <- character(n)
#   line_xunit   <- character(n)
#   line_crs      <- character(n)
#   line_nz       <- integer(n)
#   line_zlengths <- numeric(n)
#   line_zunits   <- character(n)
#   line_nx       <- integer(n)
#   line_xlengths <- numeric(n)
#   xyzCoords     <- list()
#   line_markers  <- list()
#   
#   for(i in seq_along(LINES)){
#     verboseF(message("Read ", basename(LINES[i]), "..."), verbose = verbose)
#     gpr <- verboseF( readGPR(LINES[[i]], verbose = verbose, ...), verbose = verbose)
#     if(inherits(gpr, "GPRset")){
#       stop("HOW TO HANDLE GPRset OBJECT????")
#     }
#     line_paths[i] <- .saveTempFile(gpr)
#     # FIX ME!
#     #  > check if name(gpr) is unique
#     line_nx[i]           <- ncol(gpr)
#     line_nz[i]           <- nrow(gpr)
#     line_zlengths[i]     <- abs(diff(range(gpr@z)))
#     line_xlengths[i]     <- abs(diff(range(gpr@x)))
#     line_names[i]        <- gpr@name[1]
#     if(line_names[i] == ""){
#       line_names[i] <- "default_name"
#     }
#     if(i > 1){
#       line_names[i] <- safeName(x = line_names[i], 
#                                 y = line_names[1:(i - 1)])
#     }
#     line_descs[i] <- gpr@desc
#     line_modes[i]  <- gpr@mode
#     if(length(gpr@date) == 0){
#       # should never happen
#       if(isTRUE(verbose)){
#         warning(LINES[[i]], "\n", "date has length zero. Should never happen!")
#       }
#       line_dates[i]        <- Sys.Date()
#     }else{
#       line_dates[i]        <- gpr@date
#     }
#     if(length(gpr@freq) == 0){
#       # should never happen
#       if(isTRUE(verbose)){
#         warning(LINES[[i]], "\n", "frequency has length zero Should never happen!")
#       }
#       line_freq[i]         <- 0
#     }else{
#       line_freq[i]         <- gpr@freq
#     }
#     if(length(gpr@antsep) == 0){
#       # should never happen
#       if(isTRUE(verbose)){
#         warning(LINES[[i]], "\n", "ant. sep. has length zero")
#       }
#       line_antsep[i]       <- 0
#     }else{
#       line_antsep[i]       <- gpr@antsep
#     }
#     line_spunit[i]         <- gpr@spunit
#     line_xunit[i]          <- gpr@xunit
#     line_zunits[i]         <- gpr@zunit  
#     line_crs[i]            <- gpr@crs
#     xyzCoords[[i]]         <- gpr@coord
#     if(ncol(gpr@coord) == 3 )  colnames(xyzCoords[[i]]) <- c("x", "y", "z")
# 
#     line_markers[[i]]      <- trimStr(gpr@markers)
#   }
#   # line_crs <- .checkCRSsurvey(line_crs)
#   
#   if(length(unique(line_crs)) > 1){
#     if(isTRUE(verbose)){
#       warning("Not all the coordinate reference systems (CRS) are identical!\n",
#             "I take the first valid CRS!")
#     }
#   }
#   line_crs <- .checkCRS(line_crs[!is.na(line_crs)][1])
#   if(is.na(line_crs)){
#     line_spunit <- line_xunit[!is.na(line_xunit)][1]
#   }else{
#     line_spunit <- crsUnit(line_crs)
#   }
#   
#   x <- new("GPRsurvey",
#            version       = "0.3",        # version of the class
#            # paths         = LINES,        # filepath of the GPR data
#            paths         = line_paths,        # filepath of the GPR data
#            names         = line_names,   # names of the GPR profiles
#            descs         = line_descs,   # descriptions of the GPR profiles
#            modes         = line_modes,  # survey mode (reflection/CMP)
#            
#            dates         = line_dates,       # dates  of the GPR profiles
#            
#            freqs         = line_freq,    # frequencies of the GPR profiles
#            antseps       = line_antsep,    # antenna separation of the GPR profiles
#            
#            spunit        = line_spunit,  # position units  !!!length = 1!!!
#            crs           = line_crs,  # coordinates reference system
#            #coordref      = "numeric",   # reference position
#            coords        = xyzCoords,       # (x,y,z) coordinates for each profiles
#            
#            # intersections     = "list",       # (x,y) position of the profile intersections
#            markers       = line_markers,       # fiducials of the GPR profiles
#            
#            nz            = line_nz,
#            zlengths      = line_zlengths,    # depth/time window (vertical)
#            zunits        = line_zunits,  # time/depth unit  !!!length = 1!!!
#            nx            = line_nx,    # to control if nrow(@coord) == ncol(x[[i]])
#            xlengths      = line_xlengths     # depth/time window (vertical)
#   )
#   x <- intersect(x)
#   return(x)
# }

