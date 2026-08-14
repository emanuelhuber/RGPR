

# Extract or replace parts of a GPR object

# Object of the class GPR can be manipulated as matrix
# @param x [\code{GPR}]
# @param i [\code{integer}] Indices specifying elements to extract or replace.
# @param j [\code{integer}] Indices specifying elements to extract or replace.
# @param value [\code{numeric}] Value to set.
# @param ... Not used.
# @param drop Not used.
# @return [\code{GPR}|\code{numeric}] Returns a numeric vector
#        only if \code{x[]}.
# @name subset-GPR
#NULL
# setGeneric("[", function(x, i, j, ..., drop)
  # standardGeneric("["))

# #' @name GPR-subset
# #' @docType methods
# #' @rdname GPR-subset
# #' @rdname subset-GPR
# #' @export

# setMethod("[", signature(x = "testClass", i = "ANY", j="ANY"),
#           function (x, i, j, ..., drop){
#             print("void function")
#           }
# )

     
            
#' Extract and replace parts of a GPR object
#' 
#' Extract parts of a GPR object
#' @param x (`GPR`)
#' @param i (`integer`) Indices specifying elements to extract or replace.
#' @param j (`integer`) Indices specifying elements to extract or replace.
#' @param ... Not used.
#' @param drop Not used.
#' @param value (`numeric`) Value to set.
#' @return (`GPR|`)numeric`] Returns a numeric vector
#'        only if `x[]`.
#' @aliases [,GPR-method
#' @rdname subset-GPR
#' @export
setMethod("[", signature(x = "GPR", i = "ANY", j = "ANY"),
  function(x, i, j, ..., drop){
    rval <- x@data
    if(missing(i) && missing(j)){
      return(as.vector(x@data))
    }
    if(missing(i) || length(i) == 0) i <- seq_len(nrow(rval))
    if(missing(j) || length(j) == 0) j <- seq_len(ncol(rval))
    rval <- rval[i, j , drop = FALSE]
    x@z       <- x@z[i]
    # trace related slots
    x@x       <- x@x[j]
    x@z0      <- x@z0[j]
    x@time    <- x@time[j]
    x@markers <- x@markers[j]
    # if(length(x@antsep) > 1) x@antsep <- x@antsep[j]
    x@antsep <- .subsetVec(x@antsep, j)
    # if(length(x@coord) > 0)  x@coord  <- x@coord[j, , drop = FALSE]
    # if(length(x@rec) > 0)    x@rec    <- x@rec[j, , drop= FALSE]
    # if(length(x@trans) > 0)  x@trans  <- x@trans[j, , drop = FALSE]
    x@coord        <- .subsetMat(x@coord, j)   
    x@rec          <- .subsetMat(x@rec, j)
    x@trans        <- .subsetMat(x@trans, j)
    
    x@md[["clipData"]] <- .subsetclipData(x, i, j)
  
    x@data <- rval
    return(x)
  }
)

# #' Extract or replace parts of a GPR object
# #' 
# #' @param x [\code{GPR}]
# #' @param i [\code{integer}] Indices specifying elements to extract or replace.
# #' @param j [\code{integer}] Indices specifying elements to extract or replace.
# #' @param ... Not used.
# #' @export
# #' @name [<-
# #' @rdname GPR-subset


#' @aliases [<-,GPR-method
#' @rdname subset-GPR
#' @export
setReplaceMethod("[", signature(x = "GPR", i = "ANY", j = "ANY"),
  function(x, i, j, ..., value){
    rval <- x@data
    n <- nrow(rval)
    if(missing(i)) i <- 1:n
    if(missing(j)){
      if(length(dim(i)) == 2 ){
        i <- as.matrix(i)
        if(!is.matrix(i)){
          stop("invalid subscript: cannot be converted to matrix...")
        }
        x@data[i] <- value
        x@proc <- c(x@proc, "[<-")
        return (x)
      }else{
        j <- 1:ncol(x@data)
      }
    } 
    if(length(dim(x@data)) == 2) {
      x@data[i, j] <- value
    }else{
      stop("Problem: not a matrix. Please contact me: emanuel.huber@pm.me")
    }
    x@proc <- c(x@proc, "[<-")
    return (x)
  }
)
