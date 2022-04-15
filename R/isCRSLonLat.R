
#' Check wether the Coordinate Reference Systems is lon/lat
#' 
#' Returns \code{TRUE} or \code{FALSE}.
#' @param x [\code{character}] CRS (one or more)
#' @return [\code{logical}] \code{TRUE} if lon/lat else \code{FALSE}
#' @name isCRSLonLat
setGeneric("isCRSLonLat", function(x) 
  standardGeneric("isCRSLonLat"))


#' @rdname isCRSLonLat   
#' @export
setMethod("isCRSLonLat", "numeric", function(x){
  .isCRSLonLat(x)
})

#' @rdname isCRSLonLat   
#' @export
setMethod("isCRSLonLat", "integer", function(x){
  .isCRSLonLat(x)
})


#' @rdname isCRSLonLat   
#' @export
setMethod("isCRSLonLat", "character", function(x){
  .isCRSLonLat(x)
})

#' @rdname isCRSLonLat   
#' @export
setMethod("isCRSLonLat", "GPR", function(x){
  .isCRSLonLat(x@crs)
})

.isCRSLonLat <- function(x){
  y <- .checkCRS(x)
  if(anyNA(y)){
    warning("Invalid CRS:\n",
            paste(c(x[is.na[y]], ""), collapse = "\n"))
    #return(FALSE)
  } 
  grepl("+proj=longlat", y)
}