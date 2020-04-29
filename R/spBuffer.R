
#' Buffer the GPR lines
#' 
#' Returns the buffered lines as polygon(s)
#' 
#' @param x [\code{GPR|GPRsurvey}] An object of the class \code{GPR} or
#'          \code{GPRsurvey}
#' @param d [\code{numeric(1)}] buffer distance 
#' @param combine [\code{logical(1)}] If \code{TRUE}, returns the buffer for
#'                all the GPR lines together. Otherwise it returns the buffer
#'                for each line.
#' @return [\code{sfc}] Polygon as a simple feature geometry list-column.
#' @name spBuffer
setGeneric(name = "spBuffer", 
           signature = c("x", "d"),
           def = function(x, d, combine = TRUE) 
  standardGeneric("spBuffer"))


#' @rdname spBuffer   
#' @export
setMethod("spBuffer", "GPR", function(x, d){
  if(length(x@coord) > 0){
    xsf <- as.sf(x)
    return(sf::st_buffer(xsf, d))
  }else{
    stop("x has no coordinates.")
  }
})

#' @rdname spBuffer   
#' @export
setMethod("spBuffer", "GPRsurvey", function(x, d, combine = TRUE){
  sel <- sapply(x@coords, function(x) length(x) > 0)
  if(all(!sel)){
    stop("No coordinates. Set first coordinates either with 'coord(x) <-'\n",
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
  return(sf::st_buffer(xsf, d))

})

#' @rdname spBuffer   
#' @export
setMethod("spBuffer", "sfc", function(x, d, combine = TRUE){
  if(isTRUE(combine)){
    x <- sf::st_combine(x)
  }
  return(sf::st_buffer(x, d))
})

#' @rdname spBuffer   
#' @export
setMethod("spBuffer", "sf", function(x, d, combine = TRUE){
  if(isTRUE(combine)){
    x <- sf::st_combine(x)
  }
  return(sf::st_buffer(x, d))
})


