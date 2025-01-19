


#' Extract and replace parts of a GPRsurvey object
#' 
#' Extract parts of a GPRsurvey object
#' @param x (`GPRsurvey`)
#' @param i (`integer`) Indices specifying elements to extract or replace.
#' @param j (`integer`) Not used.
#' @param ... Not used.
#' @param drop Not used.
#' @param exact Not used.
#' @return (`GPRsurvey`)
#' @aliases [,GPRsurvey-method
#' @rdname subset-GPRsurvey
#' @export
setMethod("[", signature(x = "GPRsurvey", i = "ANY", j = "ANY"),
          function(x, i, j, ..., drop = TRUE){
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
    y@coords         <- x@coords[i]
    y@markers        <- x@markers[i]
    y@intersections  <- x@intersections[i]
    y@nz             <- x@nz[i]
    y@zlengths       <- x@zlengths[i]
    y@zunits         <- x@zunits[i]
    y@nx             <- x@nx[i]
    y@xlengths       <- x@xlengths[i]
    y@transf         <- x@transf
    return(y)
})



#' @aliases [[,GPRsurvey-method
#' @rdname subset-GPRsurvey
#' @export
setMethod("[[", signature(x = "GPRsurvey", i = "ANY", j = "ANY"),
          function(x, i, j, ..., exact = TRUE){
            if(missing(i)) i <- j
            y <- getGPR(x, id = i)
            return(y)
})


