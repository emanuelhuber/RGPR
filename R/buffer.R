
#' Buffer the GPR lines
#' 
#' Returns the buffered lines as polygon(s)
#' 
#' @param x (`GPR|GPRsurvey`) An object of the class `GPR` or
#'          `GPRsurvey`
#' @param d (`numeric[1]`) buffer distance 
#' @param combine (`logical[1]`) If `TRUE`, returns the buffer for
#'                all the GPR lines together. Otherwise it returns the buffer
#'                for each line.
#' @return (`sfc`) Polygon as a simple feature geometry list-column.
#' @name buffer
#' @concept spatial computation
setGeneric(name = "buffer", 
           signature = c("x", "d"),
           def = function(x, d, combine = TRUE) 
  standardGeneric("buffer"))


#' @rdname buffer   
#' @export
setMethod("buffer", "GPR", function(x, d){
  if(length(x@coord) > 0){
    xsf <- as.sf(x)
    # use 'verboseF' to  suppress the following message from sf package
    #    "st_as_s2(): dropping Z and/or M coordinate"
    return( verboseF(sf::st_buffer(xsf, d), FALSE) )  
  }else{
    stop("x has no coordinates.")
  }
})

#' @rdname buffer   
#' @export
setMethod("buffer", "GPRsurvey", function(x, d, combine = TRUE){
  sel <- sapply(x@coords, function(x) length(x) > 0)
  if(all(!sel)){
    stop("No coordinates. Set first coordinates either with 'coordinates(x) <-'\n",
         "  or with ' spInterp(x, ...)'.")
    
  }
  xsf <- as.sf(x[sel])
  if(isTRUE(combine)){
    xsf <- sf::st_combine(xsf)
  }
  if(any(!sel)){
    warning("I cannot coerce the following GPR data to 'sf' ",
            "(they don't have coordinates):\n", 
            paste0("  #", which(!sel), ": ", x@names[!sel], collapse ="\n"), 
            ".")
  }
  # use 'verboseF' to  suppress the following message from sf package
  #    "st_as_s2(): dropping Z and/or M coordinate"
  return(verboseF(sf::st_buffer(xsf, d), FALSE))

})

#' @rdname buffer   
#' @export
setMethod("buffer", "sfc", function(x, d, combine = TRUE){
  if(isTRUE(combine)){
    x <- sf::st_combine(x)
  }
  # use 'verboseF' to  suppress the following message from sf package
  #    "st_as_s2(): dropping Z and/or M coordinate"
  return(verboseF(sf::st_buffer(x, d), FALSE))
})

#' @rdname buffer   
#' @export
setMethod("buffer", "sf", function(x, d, combine = TRUE){
  if(isTRUE(combine)){
    x <- sf::st_combine(x)
  }
  # use 'verboseF' to  suppress the following message from sf package
  #    "st_as_s2(): dropping Z and/or M coordinate"
  return(verboseF(sf::st_buffer(x, d), FALSE))
})


