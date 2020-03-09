


#' Extract and replace parts of a GPRsurvey object
#' 
#' Extract parts of a GPRsurvey object
#' @param x [class \code{GPRsurvey}]
#' @param i [\code{integer}] Indices specifying elements to extract or replace.
#' @param j [\code{integer}] Not used.
#' @param ... Not used.
#' @param drop Not used.
#' @return [\code{GPR}|\code{numeric}] Returns a numeric vector
#'        only if \code{x[]}.
#' @aliases [,GPRsurvey-method
#' @rdname subset-GPRsurvey
#' @export
setMethod("[", signature(x = "GPRsurvey", i = "ANY", j = "ANY"),
          function(x, i, j, ..., drop){
    if(missing(i)) i <- j
    # cat(typeof(i),"\n")
    # cat(j,"\n")
    # i <- as.numeric(i)
    y <- x
    y@paths          <- x@paths[i]
    y@names          <- x@names[i]
    y@descs          <- x@descs[i]
    y@modes          <- x@modes[i]
    
    y@dates          <- x@dates[i]
    y@freqs          <- x@freqs[i]
    y@antseps        <- x@antseps[i]
    
    y@spunit         <- x@spunit[i]
    # FIXME
    if(length(x@crs) == 1){
      y@crs <- x@crs
    }else{
      y@crs <- x@crs[i]
    }
    y@coords         <- x@coords[x@names[i]]
    y@markers        <- x@markers[x@names[i]]
    y@intersections  <- x@intersections[x@names[i]]
    y@nz             <- x@nz[i]
    y@zlengths       <- x@zlengths[i]
    y@zunits         <- x@zunits[i]
    y@nx             <- x@nx[i]
    y@xlengths       <- x@xlengths[i]
    
    return(y)
  }
)