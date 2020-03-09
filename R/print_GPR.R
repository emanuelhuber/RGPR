.GPR.print   <-  function(x){
  toprint <- character(9 + length(x@proc))
  toprint[1] <- paste0("*** Class GPR (", x@version, ") ***")
  toprint[2] <- paste0("name        = ", x@name)
  if(length(x@path) > 0) toprint[3] <- paste0("filepath    = ", x@path)
  nbfid <- sum(trimStr(x@markers)!= "")
  toprint[4] <- paste0(nrow(x@data), " samples, ", ncol(x@data), " traces")
  if(nbfid > 0)          toprint[5] <- paste0(nbfid, " markers(s)")
  if(length(x@desc) > 0) toprint[6] <- paste0("description = ", x@desc)
  if(length(x@date) > 0) toprint[7] <- paste0("survey date = ", x@date)
  toprint[8] <- paste0("survey mode: ", x@mode, "; ",
                       "line length: ", diff(range(x@x)), " ", x@xunit, "; ",
                       "window length = ", diff(range(x@z)), " ", x@zunit, "; ",
                       "frequency: ", x@freq, " MHz;")
  i <- 0
  if(length(x@proc)>0){
    toprint[9] <- "> PROCESSING"
    for(i in seq_along(x@proc)){
      toprint[9 + i] <- paste0("  ", i, ". ", x@proc[i])
    }      
  }
  toprint[9 + i + 1] <- "***********************"
  return(toprint[toprint != ""])      
}    


#' Print GPR
#' @param x [\code{GPR}] 
#' @param ... Not used. 
#' @export
print.GPR <- function(x, ...){
  jj <- .GPR.print(x, ...)
  cat(jj, sep = "\n")
  return(invisible(jj))
}


#' Show some information on the GPR object
#'
#' Identical to print().
#' @param object [\code{GPR}] 
#' @export
setMethod("show", "GPR", function(object){
  print.GPR(object)
}) 