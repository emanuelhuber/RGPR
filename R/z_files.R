getFPath <- function(x){
  if(inherits(x, "connection")){
    summaryCon <- summary.connection(x)
    return( summaryCon$description )
  }else{
    return(x)
  }
}


#' Filepath(s) with correct extension(s)
#' 
#' Returns the filepaths with the correct extension and check for 
#' upper and lower case extension (e.g., ".txt" or ".TXT")
#' @param fPath      [`character[1]`] Filepath.
#' @param ext        [`character`] Extensions to check 
#'                   (e.g., `".hd"`).
#' @param throwError (`logical[1]`) If TRUE, an error is thrown if the 
#'                   filepath with one of the extension does not exist. 
#'                   If FALSE, it returns NULL for the missing extension
#' @return           [`list`] The list keys correspond to `ext` and 
#'                   the values to the filepaths 
#'                   (e.g., `$hd  -> xline01.hd`).
#' @export  
#' @concept I/O
getFName <- function(fPath, ext = c(".hd", ".dt1"), throwError = TRUE){
  fp <- file.path(dirname(fPath), .fNameWExt(fPath))
  ext <- tolower(ext)
  Ext <- toupper(ext)
  mfile <- list()
  for(i in seq_along(ext)){
    if(file.exists(f1 <- paste0(fp, Ext[i]))){
      mfile[[gsub("^[.]",  "", ext[i])]] <- f1
    }else if(file.exists(f2 <- paste0(fp, ext[i]))){
      mfile[[gsub("^[.]",  "", ext[i])]] <- f2
    }else{
      if(isTRUE(throwError)){
        stop("Files '", f1, "' and '", f2, "' do not exist!\n",
             "Check the filepath!")
      }else{
        mfile[[gsub("^[.]",  "", ext[i])]] <- NULL
      }
    }
  }
  return(mfile)
}

# NAME: safe file path
# test if the file already exists
# if yes, add a suffix to the filepath
safeFPath <- function(fPath = NULL){
  dirName   <- dirname(fPath)
  fName <- .fNameWExt(fPath)
  ext <- .fExt(fPath)
  if(dirName == '.'){
    fPath <- fName
  }else{
    fPath <- paste0(dirName, '/' , fName)
  }
  fPath_orgi <- fPath
  k <- 0
  while(file.exists(paste0(fPath, ".", ext)) || 
        file.exists( paste0(fPath, ".HD"))){
    fPath <- paste0(fPath_orgi, "_", k)
    k <- k + 1
  }
  newfPath <- paste0(fPath, ".", ext)
  return(newfPath)
}

safeName <- function(x, y){
  xold <- x
  k <- 1
  while(any(x == y)){
    x <- paste0(xold, "_", k)
    k <- k + 1
  }
  return(x)
}


# #' return filename without extension
# #' @export
.fNameWExt <- function(x){
  sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(x))
  # unlist(lapply(strsplit(basename(x),"[.]"), head , 1 ), use.names = FALSE)
}

# #' return the file extension.
# #' @export
.fExt <- function(x){
  #   cat("with caution... because split \'.\' may not be so good\n")
  # unlist(lapply(strsplit(basename(x),"[.]"), tail , 1 ), use.names = FALSE)
  
  gsub(".*\\.([a-zA-Z0-9]{3,4})$", "\\1", x)
  
}


.saveTempFile <- function(x){
  tmpf <- tempfile(pattern = x@name, fileext = ".rds")
  writeGPR(x, dsn = tmpf, type = "rds", overwrite = TRUE)
  return(tmpf)
}



#' Get properties of ASCII file to read it with `read.table`
#' 
#' To get header, separator, column with na values, etc.
#' 
#' don't forget to skip blank line when reading dsn
#' @param dsn   (character) File path or connection
#' @param lns     (numeric) Number of lines to read to get the properties of the ASCII file
#' @param verbose (boolean) If `TRUE` print messages allowed.
#' @return 1) header, 2) skip, 3) 
#' @export
#' @concept I/O
detectASCIIProp <- function(dsn, lns = 20, verbose = TRUE){
  
  #---------------------- read first 'lns' lines ------------------------------#
  x <- readLines(dsn, n = lns, skipNul = TRUE)
  x <- x[ x!= ""]
  
  #------------------------------ detect header -------------------------------#
  # y <- strsplit(x, split = "[^[:alnum:]]+")
  # split at all punctuations signs except '-' and '-'
  y <- strsplit(x, split = "[^[:alnum:]\\.\\-]+") 
  test0 <- suppressWarnings(lapply(y, as.numeric))
  test <- sapply(test0, function(x) sum(is.na(x)))
  if( all(test[-1] > 0) ){
    if(length(unique(test)) == 1){
      nHeader <- 0
    }else{
      nHeader <- 1
      if(verbose){
        warning("Cannot detect header with certitude. ",
                "I assume that the first non-empty line it the header.")
      }
    }
  }else{
    nHeader <- which(test > 0)
    #message("there is ", length(nHeader), " header lines!")
  }
  
  if(length(nHeader) > 0 && nHeader > 0){
    x0 <- x[-nHeader]
    header <- TRUE
    skip <- max(nHeader) - 1
  }else{
    x0 <- x
    header <- FALSE
    skip <- 0
  }
  
  #--------------------------- detect column separator ------------------------#
  sep <- unique(unlist(lapply(x0, detectSep)))
  sepName <- sep
  if(length(sep) > 1){
    stop("seems that you have different column delimiters: ", sepName, "\n")
  }else{
    #message("Column delimiter is '", sepName, "'")
  }
  
  if(sep == "\t"){
    sepName <- "\\t"
  }
  #--------------------------- number of columns ------------------------------#
  # z <- strsplit(x0, split = "[^[:alnum:]\\.\\-]+")
  z <- strsplit(x0, split = sep)
  nCols <- unique(sapply(z, length))
  
  return(list(header = header, skip = skip, sep = sep, nCols = nCols))
}

detectSep <- function(x){ 
  i <- gregexpr("[^[:alnum:]\\.\\-\\+]+", x, perl = TRUE)
  sep <- unique(substring(x, i[[1]], i[[1]]))
}

rmNaCol <- function(x){
  # remove NA columns
  rmCol <- which(apply(x, 2, function(x) sum(is.na(x))) > 0)
  if(length(rmCol) > 0)    x <- x[, - rmCol]
  return(x)
}
