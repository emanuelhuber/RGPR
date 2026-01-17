#------------------------------------------#
#-------------- CONSTRUCTOR ---------------#
#' Create an object of the class GPRsurvey
#'
#' Create an object of the class GPRsurvey using a vector of GPR data filepath
#' @param x (`character[k]`)     Vector of `k` file paths of GPR data.
#' @param verbose (`logical[1]`) If `TRUE` the function prints some
#'                                    information.
#' @param ...     Additional parameters to be passed to [readGPR()].
#' @name GPRsurvey
#' @export
# LINES = list of datapath
GPRsurvey <- function(x, verbose = TRUE, ...){
  LINES <- x
  n <- length(LINES)
  line_paths    <- character(n)
  line_names    <- character(n)
  line_descs    <- character(n)
  line_modes    <- character(n)
  line_dates    <- as.Date(rep(NA, n))
  line_freq     <- numeric(n)
  line_antsep   <- numeric(n)
  line_lengths  <- numeric(n)
  line_spunit   <- character(n)
  line_xunit   <- character(n)
  line_crs      <- character(n)
  line_nz       <- integer(n)
  line_zlengths <- numeric(n)
  line_zunits   <- character(n)
  line_nx       <- integer(n)
  line_xlengths <- numeric(n)
  xyzCoords     <- list()
  line_markers  <- list()
  
  for(i in seq_along(LINES)){
    verboseF(message("Read ", basename(LINES[i]), "..."), verbose = verbose)
    gpr <- verboseF( readGPR(LINES[[i]], verbose = verbose, ...), verbose = verbose)
    if(inherits(gpr, "GPRset")){
      stop("HOW TO HANDLE GPRset OBJECT????")
    }
    line_paths[i] <- .saveTempFile(gpr)
    # FIX ME!
    #  > check if name(gpr) is unique
    line_nx[i]           <- ncol(gpr)
    line_nz[i]           <- nrow(gpr)
    line_zlengths[i]     <- abs(diff(range(gpr@z)))
    line_xlengths[i]     <- abs(diff(range(gpr@x)))
    line_names[i]        <- gpr@name[1]
    if(line_names[i] == ""){
      line_names[i] <- "default_name"
    }
    if(i > 1){
      line_names[i] <- safeName(x = line_names[i], 
                                y = line_names[1:(i - 1)])
    }
    line_descs[i] <- gpr@desc
    line_modes[i]  <- gpr@mode
    if(length(gpr@date) == 0){
      # should never happen
      if(isTRUE(verbose)){
        warning(LINES[[i]], "\n", "date has length zero. Should never happen!")
      }
      line_dates[i]        <- Sys.Date()
    }else{
      line_dates[i]        <- gpr@date
    }
    if(length(gpr@freq) == 0){
      # should never happen
      if(isTRUE(verbose)){
        warning(LINES[[i]], "\n", "frequency has length zero Should never happen!")
      }
      line_freq[i]         <- 0
    }else{
      line_freq[i]         <- gpr@freq
    }
    if(length(gpr@antsep) == 0){
      # should never happen
      if(isTRUE(verbose)){
        warning(LINES[[i]], "\n", "ant. sep. has length zero")
      }
      line_antsep[i]       <- 0
    }else{
      line_antsep[i]       <- gpr@antsep
    }
    line_spunit[i]         <- gpr@spunit
    line_xunit[i]          <- gpr@xunit
    line_zunits[i]         <- gpr@zunit  
    line_crs[i]            <- gpr@crs
    xyzCoords[[i]]         <- gpr@coord
    if(ncol(gpr@coord) == 3 )  colnames(xyzCoords[[i]]) <- c("x", "y", "z")

    line_markers[[i]]      <- trimStr(gpr@markers)
  }
  # line_crs <- .checkCRSsurvey(line_crs)
  
  if(length(unique(line_crs)) > 1){
    if(isTRUE(verbose)){
      warning("Not all the coordinate reference systems (CRS) are identical!\n",
            "I take the first valid CRS!")
    }
  }
  line_crs <- .checkCRS(line_crs[!is.na(line_crs)][1])
  if(is.na(line_crs)){
    line_spunit <- line_xunit[!is.na(line_xunit)][1]
  }else{
    line_spunit <- crsUnit(line_crs)
  }
  
  # if(isTRUE(verbose)){
  #   # if( length(unique(line_spunit)) > 1 ){
  #   #   warning("Position units are not identical: \n",
  #   #           "check 'spunit(x)")
  #   # }
  #   if(length(unique(line_zunits)) > 1){
  #     warning("Depth units are not identical: \n",
  #             "check 'zunit(x)'")
  #   }
  #   # ucrs <- unique(line_crs[!is.na(line_crs)])
  #   # print(line_crs)
  #   if(length(line_crs) > 1){
  #     warning("Not all the coordinate reference systems are identical:\n",
  #             "check 'crs(x)'")
  #   }
  # }
  x <- new("GPRsurvey",
           version       = "0.3",        # version of the class
           # paths         = LINES,        # filepath of the GPR data
           paths         = line_paths,        # filepath of the GPR data
           names         = line_names,   # names of the GPR profiles
           descs         = line_descs,   # descriptions of the GPR profiles
           modes         = line_modes,  # survey mode (reflection/CMP)
           
           dates         = line_dates,       # dates  of the GPR profiles
           
           freqs         = line_freq,    # frequencies of the GPR profiles
           antseps       = line_antsep,    # antenna separation of the GPR profiles
           
           spunit        = line_spunit,  # position units  !!!length = 1!!!
           crs           = line_crs,  # coordinates reference system
           #coordref      = "numeric",   # reference position
           coords        = xyzCoords,       # (x,y,z) coordinates for each profiles
           
           # intersections     = "list",       # (x,y) position of the profile intersections
           markers       = line_markers,       # fiducials of the GPR profiles
           
           nz            = line_nz,
           zlengths      = line_zlengths,    # depth/time window (vertical)
           zunits        = line_zunits,  # time/depth unit  !!!length = 1!!!
           nx            = line_nx,    # to control if nrow(@coord) == ncol(x[[i]])
           xlengths      = line_xlengths     # depth/time window (vertical)
  )
  x <- intersect(x)
  return(x)
}

