
#' Extract and replace parts of a GPRset object
#' 
#' Extract parts of a GPR object
#' @param x [class \code{GPRset}]
#' @param i [\code{integer}] Indices specifying elements to extract or replace.
#' @param j [\code{integer}] Indices specifying elements to extract or replace.
#' @param k [\code{integer}] Indices specifying elements to extract or replace.
#' @param ... Not used.
#' @param drop Not used.
#' @param value [\code{numeric}] Value to set.
#' @return [\code{GPR}|\code{numeric}] Returns a numeric vector
#'        only if \code{x[]}.
#' @aliases [,GPRset-method
#' @rdname subset-GPRset
#' @export
setMethod("[", signature(x = "GPRset", i = "ANY", j = "ANY"),
  function(x, i, j, k, ..., drop){
    # rval <- x@data
    if(missing(i) && missing(j) && missing(k)){
      return(as.vector(x@data))
    }
    if(missing(i) || length(i) == 0) i <- seq_len(nrow(x@data))
    if(missing(j) || length(j) == 0) j <- seq_len(ncol(x@data))
    if(missing(k) || length(k) == 0) k <- seq_len(dim(x@data)[3])
    # return a x-z object
    if(length(k) == 1) {
      # print("length(k) == 1")
      # CASE where GPRset <- antenna 1, antenna 2, antenna 3, ...
      x_freq <- x@freq
      if(x@ylab == "frequency"){
        x_freq <- x@y[k]
      }
      x <- new("GPR",   
          #--- class GPRvirtual
          version      = "0.3",  
          name         = trimStr(paste0(x@name, " - ", x@ylab, ": ", x@y[k], " ", x@yunit)),
          path         = x@path,
          desc         = x@desc,
          mode         = x@mode,
          date         = x@date,
          freq         = x_freq, #x@freq, 
          
          data         = x@data[i, j ,k],     
          dunit        = x@dunit,  
          dlab         = x@dlab, 
          
          spunit       = x@spunit,  
          crs          = x@crs,  
          
          xunit        = x@xunit,  
          xlab         = x@xlab,
          
          zunit        = x@zunit,  
          zlab         = x@zlab,
          
          vel          = x@vel,   
          
          proc         = x@proc,
          delineations = x@delineations,
          md           = x@md,  
          
          #--- class GPR
          z0           = x@z0[j],    
          
          time         = x@time[j],    
          antsep       = .subsetVec(x@antsep, j),    
          markers      = .subsetVec(x@markers, j), 
          ann          = .subsetVec(x@ann, j), 
          
          coord        = .subsetMat(x@coord, j),     
          rec          = .subsetMat(x@rec, j),     
          trans        = .subsetMat(x@trans, j),     
          
          x            = x@x[j],    
          z            = x@z[i]
          
          #--- class GPRset
          # y            = seq_len(x$hd$NCHAN),    # y-values, length = p
          # yunit        = "MHz",  # set units, length = 1|p
          # ylab         = "Antenna"  # set names, length = 1|p
      )
    }else if(length(i) > 0){
      print("kk")
    }
    # x@data <- rval
    return(x)
  })

# #' Extract or replace parts of a GPR object
# #' 
# #' @param x [\code{GPR}]
# #' @param i [\code{integer}] Indices specifying elements to extract or replace.
# #' @param j [\code{integer}] Indices specifying elements to extract or replace.
# #' @param ... Not used.
# #' @export
# #' @name [<-
# #' @rdname GPR-subset


#' @aliases [<-,GPRset-method
#' @rdname subset-GPRset
#' @export
setReplaceMethod("[", signature(x = "GPRset", i = "ANY", j = "ANY"),
  function(x, i, j, k, ..., value){
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
