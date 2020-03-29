#' read topo file
#' 
#' read topo file
#' @param TOPO file
#' @param sep  separatro
#' @param verbose logical(1)
#' @return list
#' @export
readTopo <- function(TOPO, sep = NULL, verbose = TRUE){
  myTopo <- list() 
  for(i in seq_along(TOPO)){
    if(verbose) message("read ", TOPO[[i]], "...")
    pp <- verboseF( detectASCIIProp(TOPO[[i]]), verbose = verbose)
    A <- read.table(file             = TOPO[[i]], 
                    sep              = pp$sep, 
                    stringsAsFactors = FALSE, 
                    header           = pp$header,
                    skip             = pp$skip)
    if(ncol(A) < 3){
      stop(TOPO[[i]], " must have 3 columns: x, y and z!")
    }else if(ncol(A) > 3 && verbose){
      warning(TOPO[[i]], " has ", ncol(A), ". I take only the 3 first columns.")
    }
    colnames(A)[1:3] <- c("x", "y", "z")
    myTopo[[i]] <- A[,1:3]
  }
  if(!is.list(TOPO)){
    return(myTopo[[1]])
  }else{
    return(myTopo)
  }
}