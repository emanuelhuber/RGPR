# =============================================================================
# io-vol.R
#
# 3d-Radar  —  VOL
#
# Mandatory : *.vol
# Optional  : (none)
#
# NOTE: This format is still experimental.  3D data returns a 'GPRcube'
# object with complex numbers; current processing and plotting functions
# will most likely not work on the returned object.
# =============================================================================


#' Read a 3d-Radar VOL file (.vol)
#'
#' Format-specific reader called by the dispatcher.  Not intended to be called
#' directly by users; use \code{\link{readGPR}} instead.
#'
#' @param dsn     Named list with slot \code{VOL}.
#' @param fName   (`character(1)`) Base filename.
#' @param fPath   (`character(1)`) Full path to the .vol file.
#' @param desc    (`character(1)`) Short data description.
#' @param Vmax    (`numeric(1)|NULL`) Nominal input voltage for bit conversion.
#' @param verbose (`logical(1)`) Print progress messages.
#' @param ...     Currently unused; reserved for future use.
#'
#' @return A named list with:
#'   \item{x}{Object of class \code{GPR} or \code{GPRcube}.}
#'   \item{x_gps}{\code{NULL} (VOL carries no GPS companion file).}
#'
#' @keywords internal
.read_vol <- function(dsn, fName, fPath, desc, Vmax, verbose, ...) {
  
  A <- verboseF(readVOL(dsn[["VOL"]]), verbose = verbose)
  x <- verboseF(.gprVOL(A, fName = fName, fPath = fPath,
                        desc = desc, Vmax = Vmax),
                verbose = verbose)
  
  if (A$hd$dim == "3D") {
    warning(
      "Returning a 'GPRcube' object with complex numbers. ",
      "Current processing and plotting functions will most likely not ",
      "work on the returned object. Please contact the maintainer:\n",
      "emanuel.huber@pm.me",
      call. = FALSE
    )
  } else {
    warning(
      "3d-Radar VOL support is still experimental. ",
      "Please contact the maintainer:\n",
      "emanuel.huber@pm.me",
      call. = FALSE
    )
  }
  
  list(x = x, x_gps = NULL)
}


# -----------------------------------------------------------------------------
# Format registration
# -----------------------------------------------------------------------------
register_gpr_format(
  id         = "VOL",
  detect_ext = "VOL",
  mandatory  = c(VOL = "VOL"),
  optional   = character(0),
  gps_ext   = NULL,
  reader_fn  = .read_vol
)

.gprVOL <- function(x, fName = "", fPath = "", desc = "", Vmax = 50){
  
  if(is.null(Vmax)) Vmax <- 50
  
  
  if(is.null(x$hd$zmin) && !is.null(x$hd$dz)){
    x_depth <- seq(x$hd$zmin, by = as.numeric(x$hd$dz), length.out = x$hd$z_dim)
  }else{
    x_depth <- seq_len(x$hd$z_dim)
  }
  if(x$hd$dim == "2D"){
    y <- new("GPR",   
             version      = "0.2",
             data        = bits2volt(Vmax = Vmax, nbits = x$hd$bits) * x$data,
             traces      = 1:ncol(x$data),
             fid         = rep("", ncol(x$data)),
             #coord = coord,
             coord       = matrix(nrow = 0, ncol = 0),
             pos         = 1:ncol(x$data),
             depth       = 1:nrow(x$data),
             rec         = matrix(nrow = 0, ncol = 0),
             trans       = matrix(nrow = 0, ncol = 0),
             time0       = rep(0, ncol(x$data)),
             # time = x$hdt[1,] * 3600 + x$hdt[2,] * 60 + x$hdt[3,],
             time        = rep(0, ncol(x$data)),
             proc        = character(0),
             vel         = list(),
             name        = fName,
             description = desc,
             filepath    = fPath,
             dz          =  1, 
             dx          = 1,
             depthunit   = "ns",
             posunit     = "m",
             freq        = 0, 
             antsep      = 0,     # check
             surveymode  = "reflection",
             date        = format(Sys.time(), "%d/%m/%Y"),
             crs         = character(0),
             hd          = x$hd
    )
  }
  if(x$hd$dim == "3D"){
    y <- new("GPRcube",
             version      = "0.2",
             name         = fName,
             date         = format(Sys.time(), "%d/%m/%Y"),  
             freq         = 0,
             filepaths    = fPath,
             x            = seq_len(x$hd$x_dim),
             y            = seq_len(x$hd$y_dim),
             data         = x$data * bits2volt(Vmax = Vmax, nbits = x$hd$bits),
             coord        = numeric(0),
             posunit      = "m",
             crs          = character(0),
             depth        = x_depth,
             depthunit    = "ns",
             vel          = list(),               
             delineations = list(),
             obs          = list(),
             transf       = numeric()
    )
  }
  return(y)
}


#' Read *.vol data
#'
#' Read *.vol data
#' @export
readVOL <- function(dsn){
  if(!inherits(dsn, "connection")){
    dsn <- file(dsn, 'rb')
  }
  dsn <- dsn
  hd <- c()
  
  #================================ HEADER ======================================#
  # The header consists of at least 60 bytes of binary data. 
  # Each field in the header is a 32 bit (4 byte) word in 
  # network byte order (“big endian”), making a total of 
  # at least 15 header words. This implies that the byte order has to be swapped 
  # to read the values on an Intel-based PC.
  seek(dsn, where = 0, origin = "start")
  # 0 Magic token. This is always 192837465 (decimal)
  hd$magic_token <- readBin(dsn, what = "integer", size = 4, endian = "big")
  # 1 Header size in bytes, including the magic token and size fields
  hd$header_size <- readBin(dsn, what = "integer", size = 4, endian = "big")
  # 2 The size of the 3d matrix size in the z dimension
  hd$z_dim <- readBin(dsn, what = "integer", size = 4, endian = "big")
  # 3 The size of the 3d matrix size in the y dimension
  hd$y_dim <- readBin(dsn, what = "integer", size = 4, endian = "big")
  # 4 The size of the 3d matrix size in the x dimension
  hd$x_dim <- readBin(dsn, what = "integer", size = 4, endian = "big")
  # 5 Bits per sample. This should always be 64 for radar data
  hd$bits <- readBin(dsn, what = "integer", size = 4, endian = "big")
  # reserved bits
  seek(dsn, where = 40, origin = "start")
  # 10 Major file format version
  hd$major_vers <- readBin(dsn, what = "integer", size = 4, endian = "big")
  # 11 Minor file format version
  hd$minor_vers <- readBin(dsn, what = "integer", size = 4, endian = "big")
  # 12 File format revision number
  hd$rev <- readBin(dsn, what = "integer", size = 4, endian = "big")
  
  
  # These two words define the file offset and size of a block of XML data 
  # in 8 bit ASCII that define further metadata for the volume file.
  seek(dsn, where = 60, origin = "start")
  if(hd$header_size >= 68){
    # 15 XML data file offset
    hd$xml_fo <- readBin(dsn, what = "integer", size = 4, endian = "big")
    # 16 XML data size
    hd$xml_size <- readBin(dsn, what = "integer", size = 4, endian = "big")
    
    seek(dsn, where = hd$xml_fo, origin = "start")
    hd$XML <- readBin(dsn, what = "character", n = 1, size = 1, endian = "big")
    
    data <- XML::xmlParse(hd$XML)
    
    els <- XML::getNodeSet(data, "//MetadataDictionary/entry[@name]")
    if(length(els) > 0){
      metaD <- sapply(els, function(el) XML::xmlGetAttr(el, "value"))
      names(metaD) <- sapply(els, function(el) XML::xmlGetAttr(el, "name"))
      hd$meta_cst <- metaD
    }
    
    els2 <- XML::getNodeSet(data, "//meta-data[@DataDomainType]")
    if(length(els2) > 0){
      metaD2 <- XML::xmlAttrs(els2[[1]])
      if(length(metaD2) > 0 && !is.null(metaD2)){
        if(!is.null(metaD2["DeltaValueZ"])){
          hd$dz   <- as.numeric(metaD2["DeltaValueZ"])
        }
        if(!is.null(metaD2["MinValueZ"])){
          hd$zmin <- as.numeric(metaD2["MinValueZ"])
        }
        if(!is.null(metaD2["MaxValueZ"])){
          hd$zmax <- as.numeric(metaD2["MaxValueZ"])
        }
      }
    }
  }
  
  #================================ Binary Data =================================#
  seek(dsn, where = hd$header_size , origin = "start")
  XYZ_dim <- c(hd$x_dim, hd$y_dim, hd$z_dim)
  test <- which(XYZ_dim == 1)
  if(length(test) > 0){
    hd$dim <- "2D"
    XYZ_dim <- XYZ_dim[-test]
    XYZ <- array(dim = XYZ_dim)
    for(i in 1:XYZ_dim[1]){
      for(j in 1:XYZ_dim[2]){
        XYZ[i,j] <-  readBin(dsn, what = "numeric", size = 4, endian = "big")
      }
    }
  }else{
    hd$dim <- "3D"
    XYZ <- array(dim = XYZ_dim)
    for(k in seq_len(hd$z_dim)){
      for(i in seq_len(hd$x_dim)){
        for(j in seq_len(hd$y_dim)){
          #XYZ[i,j,k] <- readBin(dsn, what = "numeric", size = 8, endian = "big")
          realPart <- readBin(dsn, what = "integer", size = hd$bits/8/2, endian = "big")
          imagPart <- readBin(dsn, what = "integer", size = hd$bits/8/2, endian = "big")
          XYZ[i,j,k] <- complex(real = realPart,
                                imaginary = imagPart)
        }
      }
    }
  }
  
  .closeFileIfNot(dsn)
  
  return(list(hd = hd, data = XYZ))
}