
#' Check wether the Coordinate Reference Systems is lon/lat
#' 
#' Returns \code{TRUE} or \code{FALSE}.
#' @param x [\code{character}] CRS (one or more)
#' @return [\code{logical}] \code{TRUE} if lon/lat else \code{FALSE}
#' @name isCRSGeographic
setGeneric("isCRSGeographic", function(x) 
  standardGeneric("isCRSGeographic"))


#' @rdname isCRSGeographic   
#' @export
setMethod("isCRSGeographic", "numeric", function(x){
  .isCRSGeographic(x)
})

#' @rdname isCRSGeographic   
#' @export
setMethod("isCRSGeographic", "integer", function(x){
  .isCRSGeographic(x)
})


#' @rdname isCRSGeographic   
#' @export
setMethod("isCRSGeographic", "character", function(x){
  .isCRSGeographic(x)
})

#' @rdname isCRSGeographic   
#' @export
setMethod("isCRSGeographic", "GPR", function(x){
  .isCRSGeographic(x@crs)
})

.isCRSGeographic <- function(CRSobj){
  if(length(CRSobj) == 1){
    CRSobj <- tryCatch(sf::st_crs(CRSobj),
                       error = function(e){
                         message("Invalid CRS! Try something like 'EPSG:3857'!")
                         return(NA_character_)
                       })
    # CRSobj <- as.character(CRSobj)
    return(CRSobj$IsGeographic)
  }else{
    sapply(CRSobj, .isCRSGeographic, USE.NAMES = FALSE)
  }
}
#   y <- .checkCRS(x)
#   if(anyNA(y)){
#     warning("Invalid CRS:\n",
#             paste(c(x[is.na[y]], ""), collapse = "\n"))
#     #return(FALSE)
#   } 
#   grepl("+proj=longlat", y)
# }