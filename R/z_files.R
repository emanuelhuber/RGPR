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
#' @param fPath      [\code{character(1)}] Filepath.
#' @param ext        [\code{character}] Extensions to check 
#'                   (e.g., \code{".hd"}).
#' @param throwError [\code{logical(1)}] If TRUE, an error is thrown if the 
#'                   filepath with one of the extension does not exist. 
#'                   If FALSE, it returns NULL for the missing extension
#' @return           [\code{list}] The list keys correspond to \code{ext} and 
#'                   the values to the filepaths 
#'                   (e.g., \code{$hd  -> xline01.hd}).
#' @export  
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
  unlist(lapply(strsplit(basename(x),"[.]"), head , 1 ), use.names = FALSE)
}

# #' return the file extension.
# #' @export
.fExt <- function(x){
  #   cat("with caution... because split \'.\' may not be so good\n")
  unlist(lapply(strsplit(basename(x),"[.]"), tail , 1 ), use.names = FALSE)
}


# .saveTempFile <- function(x){
#   tmpf <- tempfile(x@name)
#   writeGPR(x, type = "rds", overwrite = FALSE, fPath = tmpf)
#   return(paste0(tmpf, ".rds"))
# }

