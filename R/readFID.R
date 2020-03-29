# FID <- choose.files(caption = " txt files",filters = c("txt","*.txt"))
# output = list of data frame (one for each file from FID) 
#    with c("x","y","z","trace_number") structure
#' read fiducial marker files
#' 
#' read fiducial marker files
#' @param FID list
#' @param sep separator
#' @param verbose logical
#' @return list
#' @export
readFID <- function(FID, sep = NULL, verbose = TRUE){
  myFid <- list() 
  for(i in seq_along(FID)){
    if(verbose) message("read ", FID[[i]], "...")
    pp <- verboseF( detectASCIIProp(FID[[i]]), verbose = verbose)
    A <- read.table(file             = FID[[i]], 
                    sep              = pp$sep, 
                    stringsAsFactors = FALSE, 
                    header           = pp$header,
                    skip             = pp$skip)
    # A <- read.table(FID[[i]], sep=",", stringsAsFactors=FALSE,header=TRUE)
    # colnames(A) <- toupper(colnames(A))
    # if(!all(c("E","N","Z","TRACE") %in% colnames(A))){
    #   stop("The headers should be \"E\",\"N\",\"Z\",\"TRACE\"!\n")
    # }
    # myFid[[i]] <- A[,c("E","N","Z","TRACE")]
    if(ncol(A) < 4){
      stop(FID[[i]], " must have 4 columns: x, y, z and trace number!")
    }
    # else if(ncol(A) > 4 && verbose){
    #   warning(FID[[i]], " has ", ncol(A), ". I take only the 4 first columns.")
    # }
    if(!is.null(colnames(A))){
      colnames(A) <- toupper(colnames(A))
      if(all(c("E", "N", "Z", "TRACE") %in% colnames(A))){
        A <- A[, c("E", "N", "Z", "TRACE")]
      }else if(all(c("X", "Y", "Z", "TRACE") %in% colnames(A))){
        A <- A[, c("X", "Y", "Z", "TRACE")]
      }
    }
    colnames(A)[1:4] <- c("x", "y", "z", "tn")
    myFid[[i]] <- A[, 1:4]
  }
  if(!is.list(FID)){
    return(myFid[[1]])
  }else{
    return(myFid)
  }
}
