#--- REMOVE DUPLICATED TRACES (TRACES WITH (ALMOST) SAME POSITIONS) ---#
#' Remove traces with duplicated trace positions
#' 
#' checks for duplicates trace positions (up to precision defined by 'tol') 
#' and remove them from 'x' (object of the class GPR or GPRsurvey). 
#' If there is a series of consecutive traces with an inter-distance smaller
#' than the tolerance threshold defined by the computer precision, 
#' the function starts by removing traces every two traces and repeat 
#' the procedure until the trace inter-distances are larger that the threshold.
#' Example with 5 traces:
#' \itemize{
#'   \item distance between trace 1 and 2 < tol
#'   \item distance between trace 2 and 3 < tol
#'   \item distance between trace 3 and 4 < tol
#'   \item distance between trace 4 and 5 < tol
#' }
#' The algorithm first remove trace 2 and 4 and recompute 
#' the inter-trace distances:
#' \itemize{
#'   \item distance between trace 1 and 3 < tol
#'   \item distance between trace 3 and 5 > tol
#' }  
#' The algorithm remove trace 3. END!
#' @param x   An object of the class GPR
#' @param tol Length-one numeric vector: if the horizontal distance between two 
#'            consecutive traces is smaller than \code{tol}, then
#'            the second trace is removed.
#'            If \code{tol = NULL}, \code{tol} is set equal to
#'            \code{sqrt(.Machine$double.eps)}.
#' @param verbose Logical. \code{TRUE}: a message will be thrown, 
#'                \code{FALSE}: no message will be thrown.
#' @name dropDuplicatedCoords
setGeneric("dropDuplicatedCoords", function(x, tol = NULL, verbose = TRUE)
  standardGeneric("dropDuplicatedCoords"))
 
#' @rdname dropDuplicatedCoords            
#' @export
setMethod("dropDuplicatedCoords", "GPR", function(x, tol = NULL, verbose = TRUE){
  if(length(x@coord) == 0 ){
    warning("No trace coordinates!")
    return(x)
  }
  if(is.null(tol))  tol <- .Machine$double.eps
  dist2D <- pathRelPos(x@coord[, 1:2])  # FIXME this is only 2D... missing 3D option
  tdbl <- which(abs(diff(dist2D)) < tol)
  check <- 0L
  while(length(tdbl) > 0){
    rmTr <- c()
    for(i in seq_along(tdbl)){
      if(i > 1 && tdbl[i-1] == tdbl[i] - 1){
        tdbl[i] <- -999
      }else{
        rmTr <- c(rmTr, tdbl[i])
        check <- check + 1L
      }
    }
    x <- x[, -rmTr]  # remove trace in x
    dist2D <- pathRelPos(x@coord[, 1:2])
    tdbl <- which(abs(diff(dist2D)) < tol)
  }
  if(verbose){
    message(check, " duplicated trace(s) removed from 'x'!")
  }

  proc(x) <- getArgs()
  return(x)
})

