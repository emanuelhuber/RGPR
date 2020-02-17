

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
#' @param x [\code{GPR}]
#' @param i [\code{integer}] Indices specifying elements to extract or replace.
#' @param j [\code{integer}] Indices specifying elements to extract or replace.
#' @param ... Not used.
#' @param drop Not used.
#' @param value [\code{numeric}] Value to set.
#' @return [\code{GPR}|\code{numeric}] Returns a numeric vector
#'        only if \code{x[]}.
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
    if(length(dim(rval)) == 2) {
      drop <- FALSE
      if(missing(j)){
        rval <- rval[i, , drop = drop]
        x@z <- x@z[i]
        if(!is.null(x@md[["clip"]])){
          test <- .clipMat(x@md[["clip"]], n = nrow(x@data))
          x@md[["clip"]][["clipmin"]] <- apply(test[i, ], 2, function(x) which(x == -1))
          x@md[["clip"]][["clipmax"]] <- apply(test[i, ], 2, function(x) which(x == 1))
          # if(!is.null(x@md[["clip"]][["clipmin"]])){
          #   x@md[["clip"]][["clipmin"]] <- lapply(x@md[["clip"]][["clipmin"]], function(x, i){
          #     x[which(x %in% i)]
          #   }, i)
          # }
          # if(!is.null(x@md[["clip"]][["clipmax"]])){
          #   x@md[["clip"]][["clipmax"]] <- lapply(x@md[["clip"]][["clipmax"]], function(x, i){
          #     x[which(x %in% i)]
          #   }, i)
          # }
        }
      }else { 
        if(length(j) == 0) j <- seq_len(ncol(rval))
        # rval <- rval[i, j, drop = drop.]
        rval <- rval[i, j, drop = drop]
        x@z       <- x@z[i]
        # trace related slots
        x@x       <- x@x[j]
        x@z0      <- x@z0[j]
        x@time    <- x@time[j]
        x@markers <- x@markers[j]
        if(length(x@antsep) > 1) x@antsep <- x@antsep[j]
        if(length(x@coord) > 0)  x@coord  <- x@coord[j, , drop = FALSE]
        if(length(x@rec) > 0)    x@rec    <- x@rec[j, , drop= FALSE]
        if(length(x@trans) > 0)  x@trans  <- x@trans[j, , drop = FALSE]
        if(!is.null(x@md[["clip"]])){
          if(!is.null(x@md[["clip"]][["clipmin"]])){
            x@md[["clip"]][["clipmin"]] <- x@md[["clip"]][["clipmin"]][j]
          }
          if(!is.null(x@md[["clip"]][["clipmax"]])){
            x@md[["clip"]][["clipmax"]] <- x@md[["clip"]][["clipmax"]][j]
          }
        }
      }
      if(drop && length(rval) == 1){ rval <- c(rval)}
    }else if(length(i) > 0){
      rval <- rval[i]
      x@z <- x@z[i]
      if(!is.null(x@md[["clip"]])){
        test <- .clipMat(x@md[["clip"]], n = nrow(x@data))
        x@md[["clip"]][["clipmin"]] <- apply(test[i, ], 2, function(x) which(x == -1))
        x@md[["clip"]][["clipmax"]] <- apply(test[i, ], 2, function(x) which(x == 1))
      }
    }
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
    if(missing(j)) j <- 1:ncol(x@data)
    if(length(dim(x@data)) == 2) {
      x@data[i,j] <- value
    }else{
      rval <- rval[i]
    }
    x@proc <- c(x@proc, "[<-")
    return (x)
  }
)
