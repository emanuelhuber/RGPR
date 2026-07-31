# ============================================================================ #
# io-dzt.R
#
# GSSI  —  DZT  (+ DZX + DZG)
#
# Mandatory : *.dzt (data + header combined)
# Optional  : *.dzx (extended XML metadata, trace positions, markers)
#             *.dzg (GPS NMEA data)
# ============================================================================ #




# ============================================================================ #
# Top-level reader (called by the dispatcher)
# ============================================================================ #

#' Read a GSSI GPR file (.dzt)
#'
#' Format-specific reader called by the dispatcher.  Not intended to be called
#' directly by users; use \code{\link{readGPR}} instead.
#'
#' @param dsn     Named list with slot \code{DZT} (mandatory) and optionally
#'                \code{DZX} and \code{GPS} (= .dzg path).
#' @param fName   (`character(1)`) Base filename of the .dzt file.
#' @param fPath   (`character(1)`) Full path of the .dzt file.
#' @param desc    (`character(1)`) Short data description.
#' @param Vmax    (`numeric(1)|NULL`) Nominal input voltage for bit conversion.
#' @param verbose (`logical(1)`) Print progress messages.
#' @param ...     Currently unused; reserved for future use.
#'
#' @return A named list with:
#'   \item{x}{Object of class \code{GPR} or \code{GPRset}.}
#'   \item{x_gps}{An \code{sf} object with GPS data, or \code{NULL}.}
#'
#' @keywords internal
.read_dzt <- function(dsn, fName, fPath, desc, Vmax, verbose, ...) {
  
  dzt <- verboseF(readDZT(dsn[["DZT"]]), verbose = verbose)
  
  if (!is.null(dsn[["DZX"]])) {
    dzx <- verboseF(readDZX(dsn[["DZX"]]), verbose = verbose)
    dzt <- c(dzt, list(dzx = dzx))
  }
  
  x <- verboseF(
    .gprDZT(dzt, fName = fName, fPath = fPath, desc = desc, Vmax = Vmax),
    verbose = verbose
  )
  
  x_gps <- NULL
  # GPS slot is keyed as "GPS" (resolved from the .dzg companion)
  if (!is.null(dsn[["GPS"]])) {
    x_gps <- verboseF(readDZG(dsn[["GPS"]], UTM = FALSE), verbose = verbose)
    # UTM projection is handled centrally in .maybe_interp_gps()
  }
  
  list(x = x, x_gps = x_gps)
}

# ============================================================================ #
# Format registration
# ============================================================================ #
register_gpr_format(
  id         = "DZT",
  detect_ext = "DZT",
  mandatory  = c(DZT = "DZT"),
  optional   = c(DZX = "DZX", GPS = "DZG"),
  gps_ext   = "GPS",
  reader_fn  = .read_dzt
)


# ============================================================================ #
# GSSI-specific utilities
# ============================================================================ #

#' Look up nominal antenna frequency from GSSI antenna name string
#'
#' GSSI encodes the antenna model in a 14-character string stored in the file
#' header.  This function maps known model strings to their nominal centre
#' frequency in MHz.
#'
#' @param ant_name (`character`) Antenna name string(s) as read from the header.
#' @return (`numeric`) Frequency in MHz for each element of `ant_name`.
#'         `NA` is returned for unrecognised names.
#'
#' @references GSSI SIR-30 / SIR-4000 technical documentation.
#' @keywords internal
getAntFreqGSSI <- function(ant_name) {
  # Lookup table: partial antenna name (regex) -> nominal frequency (MHz)
  # Ordered from most-specific to most-generic to avoid false matches.
  lut <- c(
    "^3107"   = 100,
    "^3101"   = 100,
    "^5106"   = 200,
    "^5103"   = 200,
    "^5104"   = 400,
    "^5208"   = 400,
    "^5106"   = 200,
    "^62"     = 270,
    "^5112"   = 500,
    "^5114"   = 600,
    "^5100"   = 900,
    "^5102"   = 900,
    "^3200MLF"= 40,
    "^3200MF" = 80,
    "^3207AP" = 200,
    "^3207"   = 200,
    "^4108"   = 1000,
    "^4105"   = 1500,
    "^41"     = 1000,
    "^42"     = 1000,
    "^43"     = 1000,
    "^50200"  = 200,
    "^50300"  = 300,
    "^5403"   = 400,
    "^50600"  = 600,
    "^50700"  = 700,
    "^50800"  = 800,
    "^508"    = 800,
    "^801"    = 800,
    "^802"    = 1000,
    "^803"    = 1600,
    "^50"     = 500
  )
  
  freq <- rep(NA_real_, length(ant_name))
  for (i in seq_along(ant_name)) {
    nm <- trimws(ant_name[i])
    for (pattern in names(lut)) {
      if (grepl(pattern, nm, ignore.case = TRUE)) {
        freq[i] <- lut[[pattern]]
        break
      }
    }
  }
  freq
}


#' Extract a frequency value (in MHz) from a free-form antenna name string
#'
#' Falls back to pattern matching on strings like `"800 MHz"`, `"1.5GHz"`, etc.
#' when \code{\link{getAntFreqGSSI}} returns \code{NA}.
#'
#' @param ant_name (`character`) Antenna name string(s).
#' @return (`numeric`) Frequency in MHz, or \code{NA} if none found.
#'
#' @keywords internal
freqFromString <- function(ant_name) {
  # Patterns: "NNN MHz", "N.N GHz", "NNNMHz", etc.
  ghz_pat <- "([0-9]+\\.?[0-9]*)\\s*[Gg][Hh][Zz]"
  mhz_pat <- "([0-9]+\\.?[0-9]*)\\s*[Mm][Hh][Zz]"
  
  freq <- rep(NA_real_, length(ant_name))
  for (i in seq_along(ant_name)) {
    nm <- trimws(ant_name[i])
    m  <- regmatches(nm, regexpr(ghz_pat, nm, perl = TRUE))
    if (length(m) > 0L) {
      freq[i] <- as.numeric(sub(ghz_pat, "\\1", m, perl = TRUE)) * 1000
      next
    }
    m <- regmatches(nm, regexpr(mhz_pat, nm, perl = TRUE))
    if (length(m) > 0L) {
      freq[i] <- as.numeric(sub(mhz_pat, "\\1", m, perl = TRUE))
    }
  }
  freq
}


#' Resolve antenna frequency for GSSI data
#'
#' Tries \code{getAntFreqGSSI()} first; falls back to
#' \code{freqFromString()} for any \code{NA} values.
#' Returns a list suitable for populating \code{GPR@freq} and the
#' y-axis of a \code{GPRset}.
#'
#' @param ant_name (`character`) Antenna name vector (one element per channel).
#' @return A named list: \code{$freq} (numeric), \code{$unit} (character).
#'
#' @keywords internal
.resolve_gssi_antfreq <- function(ant_name) {
  freq <- getAntFreqGSSI(ant_name)
  
  is_na <- is.na(freq)
  if (any(is_na)) {
    freq[is_na] <- freqFromString(ant_name[is_na])
  }
  
  is_na <- is.na(freq)
  if (all(is_na)) {
    # Nothing could be determined — use channel index as placeholder
    return(list(freq = seq_along(ant_name), unit = ""))
  }
  
  still_na <- is.na(freq)
  if (any(still_na)) {
    freq[still_na] <- 0
    message("Frequency of ",
            paste0(ant_name[still_na], collapse = ", "),
            " set to 0 MHz. Set it with 'antfreq(x) <- ...'")
  }
  
  list(freq = unname(freq), unit = "MHz")
}


# ============================================================================ #
# GPR object constructor for GSSI data
# ============================================================================ #

#' Build a GPR or GPRset object from parsed GSSI DZT data
#'
#' Assembles the S4 object from the list returned by \code{\link{readDZT}}
#' and the optional extended metadata from \code{\link{readDZX}}.
#'
#' @param x     List returned by \code{readDZT()}, optionally extended with
#'              a \code{$dzx} element from \code{readDZX()}.
#' @param fName (`character(1)`) Base filename (no path).
#' @param fPath (`character(1)`) Full path to the .dzt file.
#' @param desc  (`character(1)`) Short data description.
#' @param Vmax  (`numeric(1)|NULL`) Nominal voltage for bit conversion.
#'
#' @return An object of class \code{GPR} (single channel) or
#'         \code{GPRset} (multi-channel).
#'
#' @keywords internal
.gprDZT <- function(x, fName = character(0), fPath = character(0),
                    desc = character(0), Vmax = NULL) {
  
  if (is.null(Vmax)) Vmax <- 50
  
  # --- Date ------------------------------------------------------------------
  dd <- as.Date(x$hd$DATE, format = "%Y-%m-%d")
  if (is.na(dd)) dd <- Sys.Date()
  
  # --- Name ------------------------------------------------------------------
  x_name <- if (length(fName) == 0L || fName == "") "LINE" else fName
  
  # --- Default spatial metadata ----------------------------------------------
  x_posunit   <- "m"
  x_depthunit <- "ns"
  x_pos       <- x$pos[seq_len(ncol(x$data))]
  x_depth     <- x$depth[seq_len(nrow(x$data))]
  x_dx        <- 1 / x$hd$SPM
  
  # --- Fiducial markers ------------------------------------------------------
  x_fid  <- rep("", ncol(x$data))
  test   <- which(x$hd$MRKS < 0)
  if (length(test) > 0L) {
    fidval  <- LETTERS[as.numeric(as.factor(x$hd$MRKS[test]))]
    ufidval <- unique(fidval)
    for (i in seq_along(ufidval)) {
      test2  <- which(fidval == ufidval[i])
      fid_nb <- seq_along(test2)
      x_fid[test][test2] <- paste0(
        ufidval[i],
        sprintf(paste0("%0", max(nchar(fid_nb)), "d"), fid_nb)
      )
    }
  }
  
  # --- Override with DZX extended metadata (if present) ---------------------
  if (!is.null(x$dzx)) {
    if (!is.null(x$dzx$pos))  x_pos <- x$dzx$pos
    if (!is.null(x$dzx$dx))   x_dx  <- x$dzx$dx
    if (all(x_fid == "") &&
        !is.null(x$dzx$markers) &&
        length(x$dzx$markers) == ncol(x$data)) {
      x_fid <- x$dzx$markers
    }
    if (!is.null(x$dzx$hUnit)) {
      x_posunit <- if (grepl("in", x$dzx$hUnit)) "in" else x$dzx$hUnit
    }
  }
  
  # --- Antenna frequency resolution ------------------------------------------
  af      <- .resolve_gssi_antfreq(x$hd$ANT)
  antfreq <- af$freq
  y_unit  <- af$unit
  # y-axis values for GPRset (frequency per channel, or channel index)
  y_freq  <- antfreq
  
  # --- Propagation velocity --------------------------------------------------
  v <- 2 * x$hd$DEPTH / x$hd$RANGE
  
  # --- Antenna separation (unknown from header; user must set) ---------------
  antsep <- 0
  message("Antenna separation set to 0 ", x_posunit,
          ". Set it with 'antsep(x) <- ...'")
  
  # --- Data unit -------------------------------------------------------------
  dunit <- if (isFALSE(Vmax)) "bits" else "mV"
  
  # --- Common slot values (passed to both GPR and GPRset) --------------------
  common_slots <- list(
    version  = "0.3",
    name     = x_name,
    path     = fPath,
    desc     = desc,
    mode     = "CO",
    date     = dd,
    freq     = unname(antfreq),
    dunit    = dunit,
    dlab     = "amplitude",
    spunit   = "",
    crs      = NA_character_,
    xunit    = x_posunit,
    xlab     = "position",
    zunit    = x_depthunit,
    zlab     = "two-way travel time",
    vel      = list(v = v),
    md       = x$hd,
    z0       = rep(0, ncol(x$data)),
    antsep   = antsep,
    markers  = trimStr(x_fid),
    x        = x_pos,
    z        = x_depth
  )
  
  # --- Dispatch on channel count ---------------------------------------------
  if (x$hd$NCHAN > 1L) {
    dimnames(x$data) <- list(NULL, seq_along(x_pos), NULL)
    do.call(
      methods::new,
      c(list("GPRset"),
        common_slots,
        list(
          data  = bits2volt(Vmax = Vmax, nbits = x$hd$BITS) * x$data,
          y     = y_freq,
          yunit = y_unit,
          ylab  = "frequency"
        ))
    )
  } else {
    colnames(x$data) <- seq_len(ncol(x$data))
    do.call(
      methods::new,
      c(list("GPR"),
        common_slots,
        list(
          data = bits2volt(Vmax = Vmax, nbits = x$hd$BITS) * x$data[, , 1L]
        ))
    )
  }
}


# ============================================================================ #
# Low-level binary / text readers  (readDZT, readDZG, readDZX)
# ============================================================================ #

#' Read GSSI GPR data (.dzt)
#'
#' Reads the binary DZT file and returns the raw data array together with the
#' parsed header and axis vectors.
#'
#' @param dsn (`character(1)|connection`) Path or open binary connection to
#'            the .dzt file.
#' @return A list with elements:
#'   \item{hd}{Parsed header (list).}
#'   \item{data}{3-D array \code{[nSamples, nScans, nChannels]}.}
#'   \item{depth}{Time vector (ns).}
#'   \item{pos}{Nominal position vector (m).}
#'
#' @seealso [RGPR::readDZG()], [RGPR::eadDZX()]
#' @name readDZT
#' @rdname readDZT
#' @export
readDZT <- function(dsn) {
  dsn <- .openFileIfNot(dsn)
  on.exit(.closeFileIfNot(dsn))
  
  hd <- list()
  MINHEADSIZE <- 1024L
  nScans <- 0L
  
  # ---- Header ---------------------------------------------------------------
  hd$TAG         <- .readBin_ushort(dsn)
  hd$OFFSETDATA  <- .readBin_ushort(dsn)
  hd$NSAMP       <- .readBin_ushort(dsn)
  hd$BITS        <- .readBin_ushort(dsn)
  hd$ZERO        <- .readBin_short(dsn)
  hd$SPS         <- .readBin_float(dsn)
  hd$SPM         <- .readBin_float(dsn)
  hd$MPM         <- .readBin_float(dsn)
  hd$POSITION    <- .readBin_float(dsn)
  hd$RANGE       <- .readBin_float(dsn)
  hd$NPASS       <- .readBin_ushort(dsn)
  
  # Creation / modification dates (byte offsets 31 and 36 from file start)
  creaDT  <- .readRFDate(dsn, where = 32L)
  modDT   <- .readRFDate(dsn, where = 36L)   # not currently used
  hd$DATE <- creaDT$date
  hd$TIME <- creaDT$time
  
  seek(dsn, where = 44L, origin = "start")
  hd$OFFSETTEXT <- .readBin_ushort(dsn)
  hd$NTEXT      <- .readBin_ushort(dsn)
  hd$PROC       <- .readBin_ushort(dsn)
  hd$NPROC      <- .readBin_ushort(dsn)
  hd$NCHAN      <- .readBin_ushort(dsn)
  hd$EPSR       <- .readBin_float(dsn)
  hd$TOP        <- .readBin_float(dsn)
  hd$DEPTH      <- .readBin_float(dsn)
  
  # Antenna name (14 chars per channel, channel n at offset 98 + 1024*(n-1))
  ant_name <- character(hd$NCHAN)
  for (i in seq_len(hd$NCHAN)) {
    seek(dsn, where = 98L + MINHEADSIZE * (i - 1L), origin = "start")
    ant_name[i] <- suppressWarnings(readChar(dsn, nchars = 14L, useBytes = FALSE))
  }
  hd$ANT    <- ant_name
  hd$VSBYTE <- .readBin_ushort(dsn)
  
  # ---- Compute data offset --------------------------------------------------
  nB <- .flen(dsn)
  
  if (hd$OFFSETDATA < MINHEADSIZE) {
    hd$OFFSETDATA <- MINHEADSIZE * hd$OFFSETDATA
  } else {
    hd$OFFSETDATA <- MINHEADSIZE * hd$NCHAN
  }
  
  # nScans == 0 means "read all scans" (the only mode currently used)
  nNumScans     <- (nB - hd$OFFSETDATA) / (hd$NCHAN * hd$NSAMP * hd$BITS / 8L)
  nNumSkipScans <- 0L
  
  seek(dsn, where = hd$OFFSETDATA, origin = "start")
  
  # ---- Read data matrix -----------------------------------------------------
  if (hd$BITS == 8L) {
    invisible(readBin(dsn, "integer",
                      n = hd$NSAMP * nNumSkipScans * hd$NCHAN, size = 2L))
    A    <- matrix(nrow = hd$NSAMP, ncol = nNumScans * hd$NCHAN)
    A[]  <- readBin(dsn, what = "int", n = prod(dim(A)), size = 1L)
    test <- A > 0
    A[ test] <- A[ test] - 129L
    A[!test] <- A[!test] + 127L
    
  } else if (hd$BITS == 16L) {
    invisible(readBin(dsn, "integer",
                      n = hd$NSAMP * nNumSkipScans * hd$NCHAN, size = 2L))
    A    <- matrix(nrow = hd$NSAMP, ncol = nNumScans * hd$NCHAN)
    A[]  <- readBin(dsn, what = "int", n = prod(dim(A)), size = 2L)
    test <- A > 0
    A[ test] <- A[ test] - 32769L
    A[!test] <- A[!test] + 32767L
    
  } else if (hd$BITS == 32L) {
    A    <- matrix(nrow = hd$NSAMP, ncol = nNumScans * hd$NCHAN)
    A[]  <- readBin(dsn, what = "int", n = prod(dim(A)), size = 4L)
  }
  
  # ---- Reshape into 3-D array [samples, scans, channels] -------------------
  tt <- (seq_len(hd$NSAMP) - 1L) * hd$RANGE / (hd$NSAMP - 1L)
  yy <- (seq_len(ncol(A) / hd$NCHAN) - 1L) / hd$SPM
  
  Adata <- array(dim = c(length(tt), length(yy), hd$NCHAN))
  for (i in seq_len(hd$NCHAN)) {
    Adata[, , i] <- A[, seq(i, by = hd$NCHAN, to = ncol(A))]
    if (i == 1L) {
      hd$MRKS      <- Adata[2L, , 1L]
      Adata[1L:2L, , 1L] <- 0
    }
  }
  
  # .closeFileIfNot(dsn)
  list(hd = hd, data = Adata, depth = tt, pos = yy)
}



#' Read GSSI GPS data
#' 
#' @param dsn (`character[1]|connection`) data source name: 
#'             either the filepath to the GPR data (character),
#'            or an open file connection.
#' @param UTM (`logical[1]`) If `TRUE` project coordinates to 
#'              the corresponding UTM zone. 
#' @return (`data.frame(,5)`) position (`x`, `y`, `z`),
#'         trace id (`id`), and time (`time`).
#' @seealso [RGPR::readDZT()], [RGPR::readDZX()]
#' @name readDZG
#' @rdname readDZG
#' @export
readDZG <- function(dsn, UTM = TRUE){
  x <- scan(dsn, what = character(), sep = "\n", quiet = TRUE)
  on.exit(.closeFileIfNot(dsn))
  
  test_gssis <- grepl("(\\$GSSIS)", x, ignore.case = TRUE, useBytes = TRUE )
  test_gpgga <- grepl("(\\$GPGGA)", x, ignore.case = TRUE, useBytes = TRUE )
  
  if(sum(test_gssis) != sum(test_gpgga)){
    stop("File '.dzg' is corrupted! I cannot read it... sorry.")
  }
  
  pat_gssis <- paste0("\\$(?<ID>GSSIS),(?<tr>[0-9]+),(?<time>[-]?[0-9.]+)") 
  pat_gpgga <- paste0("\\$(?<ID>GPGGA),(?<UTC>[0-9.]+),(?<lat>[0-9.]+),",
                      "(?<NS>[NS]),(?<lon>[0-9.]+),(?<EW>[EW]),(?<fix>[0-9]),",
                      "(?<NbSat>[0-9.]+),(?<HDOP>[0-9.]+),(?<H>[0-9.]+),",
                      "(?<mf>[MmFf]+)") 
  #,(?<HGeoid>[0-9.]+),(?<mf2>[mMfF+),",
  # "(?<TDGPS>[0-9.]+),(?<DGPSID> [A-z0-9.]+)"
  # )
  
  # matches <- regexpr(pat_gpgga, x[xgpgga], perl=TRUE)
  # first <- attr(matches, "capture.start")
  # last <- first + attr(matches, "capture.length") -1
  # gpgga <- mapply(substring, x[xgpgga], first, last, USE.NAMES = FALSE)
  gpgga <- extractPattern(x[test_gpgga], pattern = pat_gpgga, 
                          start = 0, stop = -1)  
  gssis <- extractPattern(x[test_gssis], pattern = pat_gssis, 
                          start = 0, stop = -1)
  
  dim(gpgga) <- c(sum(test_gpgga), 11)
  gpgga <- as.data.frame(gpgga, stringsAsFactors = FALSE)
  colnames(gpgga) <- c("ID", "UTC", "lat", "NS", "lon", "EW", 
                       "fix", "NbSat", "HDOP", "H", "mf")
  dim(gssis) <- c(sum(test_gssis), 3)
  gssis <- as.data.frame(gssis, stringsAsFactors = FALSE)
  colnames(gssis) <- c("ID", "trace", "time")
  
  xyzt <- .getLonLatFromGPGGA(gpgga)
  
  out <-projectXYZT(xyzt, UTM = UTM, 
                    NS = gpgga$NS,
                    EW = gpgga$EW)
  
  
  # trace number start at 0!!
  mrk <- cbind(out$xyzt[ ,1:3], as.integer(gssis$trace) + 1,  out$xyzt[ ,4])
  # mrk <- as.matrix(mrk)
  names(mrk) <- c("x", "y", "z", "id", "time")
  
  mrk <- sf::st_as_sf(x      = mrk,
                      coords = c("x", "y", "z"),
                      crs    = out$xyzt_crs)
  
  return(mrk)
}

#' Read GSSI extended XML metadata (.dzx)
#'
#' Extracts trace positions, spatial sampling, horizontal units, and fiducial
#' markers from the XML companion file written by GSSI instruments.
#'
#' @param dsn (`character(1)|connection`) Path or open binary connection to
#'            the .dzx file.
#' @return A list with some or all of the following elements:
#'   \item{pos}{Interpolated position for each scan (numeric vector).}
#'   \item{dx}{Mean spatial sampling interval (numeric).}
#'   \item{markers}{Character vector of marker labels, one per scan.}
#'   \item{hUnit}{Horizontal distance unit string (e.g. \code{"m"}).}
#'   \item{vUnit}{Vertical unit string.}
#'   \item{unitsPerMark}{Units per odometer mark (numeric).}
#'   \item{unitsPerScan}{Units per scan (numeric).}
#'   Returns \code{NULL} for empty or unreadable files.
#'
#' @seealso [RGPR::readDZT()], [RGPR::readDZG()]
#' @name readDZX
#' @rdname readDZX
#' @export
readDZX <- function(dsn) {
  
  dsn <- .openFileIfNot(dsn)
  on.exit(.closeFileIfNot(dsn))
  
  xmltxt <- verboseF(readLines(dsn), verbose = FALSE)
  if (length(xmltxt) == 0L) {
    .closeFileIfNot(dsn)
    return(NULL)
  }
  
  doc <- verboseF(XML::xmlParse(xmltxt), verbose = FALSE)
  lst <- list()
  
  # ---- Global properties ----------------------------------------------------
  glbProp <- XML::xmlChildren(doc)$DZX[["GlobalProperties"]]
  if (!is.null(glbProp)) {
    lst <- .dzx_read_global_props(glbProp, lst)
  }
  
  # ---- Per-file scan range and trace positions ------------------------------
  # NOTE: multi-channel support is a known limitation (FIXME marker retained)
  fl <- XML::xmlChildren(doc)$DZX[["File"]]
  if (!is.null(fl)) {
    lst <- .dzx_read_file_props(fl, lst)
  }
  
  # .closeFileIfNot(dsn)
  if (length(lst) > 0L) lst else NULL
}


# ---- DZX sub-helpers --------------------------------------------------------

#' @keywords internal
.dzx_read_global_props <- function(glbProp, lst) {
  .xml_numeric_tag <- function(node, tag) {
    el <- XML::xmlElementsByTagName(node, tag)
    if (length(el) > 0L) {
      val <- suppressWarnings(as.numeric(XML::xmlValue(el[[1L]])))
      if (!is.na(val) && val > 0) return(val)
    }
    NULL
  }
  .xml_text_tag <- function(node, tag) {
    el <- XML::xmlElementsByTagName(node, tag)
    if (length(el) > 0L) XML::xmlValue(el[[1L]]) else NULL
  }
  
  if (!is.null(v <- .xml_numeric_tag(glbProp, "unitsPerMark"))) lst$unitsPerMark <- v
  if (!is.null(v <- .xml_numeric_tag(glbProp, "unitsPerScan"))) lst$unitsPerScan <- v
  if (!is.null(v <- .xml_text_tag(glbProp, "verticalUnit")))    lst$vUnit <- v
  if (!is.null(v <- .xml_text_tag(glbProp, "horizontalUnit")))  lst$hUnit <- v
  lst
}


#' @keywords internal
.dzx_read_file_props <- function(fl, lst) {
  s1 <- XML::xmlElementsByTagName(fl, "scanRange", recursive = TRUE)
  if (length(s1) == 0L) return(lst)
  
  s0     <- as.integer(strsplit(XML::xmlValue(s1[[1L]]), split = ",")[[1L]])
  nscans <- length(s0[1L]:s0[2L])
  
  dst <- XML::xmlElementsByTagName(fl, "distance", recursive = TRUE)
  if (length(dst) == 0L) return(lst)
  
  extract_node <- function(dist_node) {
    papa <- XML::xmlParent(dist_node)
    scan_val <- as.numeric(XML::xmlValue(
      XML::xmlElementsByTagName(papa, "scan")
    ))
    mark_el  <- XML::xmlElementsByTagName(papa, "mark")
    mark_val <- if (length(mark_el) > 0L) XML::xmlValue(mark_el[[1L]]) else ""
    dist_val <- as.numeric(XML::xmlValue(dist_node))
    unname(c(scan_val, mark_val, dist_val))
  }
  
  uu <- sapply(dst, extract_node, USE.NAMES = FALSE)
  
  if (!inherits(uu, "matrix")) {
    message("I was unable to read the markers in the file *.dzx")
    return(lst)
  }
  
  id      <- as.integer(uu[1L, ]) + 1L     # 0-based -> 1-based
  pos     <- as.numeric(uu[3L, ])
  lst$dx      <- mean(diff(pos) / (diff(id) - 1L))
  lst$pos     <- stats::approx(id, pos, seq_len(nscans))$y
  lst$markers <- character(nscans)
  lst$markers[id] <- uu[2L, ]
  
  lst
}


# ---- Shared XML sibling helper (used elsewhere in the package) --------------

#' @keywords internal
.xmlValueSibling <- function(x, after = FALSE) {
  XML::xmlValue(XML::getSibling(x, after = after))
}


# ---- Date decoding helper (also used by other GSSI utilities) ---------------

#' Decode an RF creation/modification date from a DZT file
#'
#' Reads 4 bytes at the specified offset and decodes them as a packed
#' DOS-style date+time stamp.
#'
#' @param con   Open binary file connection.
#' @param where (`integer(1)`) Byte offset from the start of the file.
#' @return A list with \code{$date} (character, \code{"YYYY-MM-DD"}) and
#'         \code{$time} (character, \code{"HH:MM:SS"}).
#'
#' @keywords internal
.readRFDate <- function(con, where = 31L) {
  seek(con, where = where, origin = "start")
  rhb_cdt0 <- readBin(con, what = "raw", n = 4L, size = 1L, endian = "little")
  aa <- rawToBits(rhb_cdt0)
  
  xdate <- paste(
    .bit2int(aa[25L + (7L:1L)]) + 1980L,
    sprintf("%02d", .bit2int(aa[21L + (4L:1L)])),
    sprintf("%02d", .bit2int(aa[16L + (5L:1L)])),
    sep = "-"
  )
  xtime <- paste(
    sprintf("%02d", .bit2int(aa[11L + (5L:1L)])),
    sprintf("%02d", .bit2int(aa[5L  + (6L:1L)])),
    sprintf("%02d", .bit2int(aa[5L:1L]) * 2L),
    sep = ":"
  )
  list(date = xdate, time = xtime)
}




