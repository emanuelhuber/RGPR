#--- REMOVE DUPLICATED TRACES (TRACES WITH (ALMOST) SAME POSITIONS) ---#
#' Remove traces with duplicated trace positions
#' 
#' Checks for duplicates trace positions (up to precision defined by 'tol') 
#' and remove them from 'x' (object of the class GPR or GPRsurvey). 
#' @param x   An object of the class GPR
#' @param tol Length-one numeric vector: if the horizontal distance between two 
#'            consecutive traces is smaller than \code{tol}, then
#'            the second trace is removed.
#'            If \code{tol = NULL}, \code{tol} is set equal to
#'            \code{sqrt(.Machine$double.eps)}.
#' @param z [\code{logical(1)}] If \code{TRUE}, the third dimension 
#'          (z-dimension) will also be accounted for.
#' @param verbose Logical. \code{TRUE}: a message will be thrown, 
#'                \code{FALSE}: no message will be thrown.
#' @name dropDuplicatedCoords
setGeneric("dropDuplicatedCoords", function(x, tol = NULL, z = FALSE, verbose = TRUE)
  standardGeneric("dropDuplicatedCoords"))
 
#' @rdname dropDuplicatedCoords            
#' @export
setMethod("dropDuplicatedCoords", "GPR", function(x, tol = NULL, z = FALSE, verbose = TRUE){
  i <- .dropDuplicatedCoords(x@coord, tol = tol, z = z, verbose = verbose)
  x <- x[, !i]
  # tdbl <- which(abs(diff(dist2D)) < tol)
  # check <- 0L
  # while(length(tdbl) > 0){
  #   rmTr <- c()
  #   for(i in seq_along(tdbl)){
  #     if(i > 1 && tdbl[i-1] == tdbl[i] - 1){
  #       tdbl[i] <- -999
  #     }else{
  #       rmTr <- c(rmTr, tdbl[i])
  #       check <- check + 1L
  #     }
  #   }
  #   x <- x[, -rmTr]  # remove trace in x
  #   dist2D <- pathRelPos(x@coord[, 1:2])
  #   tdbl <- which(abs(diff(dist2D)) < tol)
  # }
  if(verbose){
    message(sum(i), " duplicated trace(s) removed from 'x'!")
  }

  proc(x) <- getArgs()
  return(x)
})

#' @rdname dropDuplicatedCoords            
#' @export
setMethod("dropDuplicatedCoords", "matrix", function(x, tol = NULL, z = FALSE, verbose = TRUE){
  i <- .dropDuplicatedCoords(x, tol = tol, z = z, verbose = verbose)
  x <- x[!i, ]
  if(verbose){
    message(sum(i), " duplicated trace(s) removed from 'x'!")
  }
  return(x)
})


.dropDuplicatedCoords <- function(x, tol = NULL, z = FALSE, verbose = TRUE){
  if(length(x) == 0 ){
    warning("No coordinates!")
    return(x)
  }
  if(is.null(tol))  tol <- sqrt(.Machine$double.eps)
  v <- 1:2
  if(isTRUE(z)) v <- 1:3
  coordDist <- pathRelPos(x[, v])  
  i <- duplicated(round(coordDist/tol, 0))
  return(i)
}
