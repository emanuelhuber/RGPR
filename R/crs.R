

#' Coordinate reference system (CRS) of the GPR data
#' 
#' Coordinate reference system (CRS) of the GPR data
#' 
#' Modified slots
#' \itemize{
#'   \item `crs` the coordinate reference system.
#'   \item `spunit` the spatial units are updated accroding to the new
#'                       coordinate reference system.
#' }
#' @param x      (`GPR class`) An object of the class `GPR`
#' @param value  (`character[1]`) A string accepted by GDAL 
#'               (e.g., `"EPSG:2056"`, WKT-string).
#' @examples 
#' \dontrun{
#' x <- readGPR("LINE.DT1")
#' crs(x) <- "EPSG:3857"
#' }
#' @return (`GPR class`) An object of the class `GPR`
#' @name crs
#' @concept getters/setters
setGeneric("crs", function(x) 
  standardGeneric("crs"))


#' @rdname crs
#' @aliases crs<-,GPR-method
setGeneric("crs<-",function(x,value){standardGeneric("crs<-")})

 
#' @rdname crs   
#' @export
setMethod("crs", "GPR", function(x){
  if(is.na(x@crs)){
    return(x@crs)
  }else if(!is.null(a <- sf::st_crs(x@crs)$epsg)){
    u <- paste0("EPSG:", a)
    names(u) <- sf::st_crs(x@crs)$Name
    return(u)
  }else{
    return(sf::st_crs(x@crs))
  }
})

#' @rdname crs
#' @export
setReplaceMethod("crs", signature="GPR", function(x, value){
    x@crs    <- .checkCRS(value)
    x@spunit <- crsUnit(x@crs)
    x@proc   <- c(x@proc, "crs<-")
    # message("Replacing CRS does not reproject the data.\n", 
    #         "Use `projectCoords()` for that")
    return(x)
})



#' @rdname crs   
#' @export
setMethod("crs", "GPRsurvey", function(x){
  if(is.na(x@crs)){
    return(x@crs)
  }else if(!is.null(a <- sf::st_crs(x@crs)$epsg)){
    u <- paste0("EPSG:", a)
    names(u) <- sf::st_crs(x@crs)$Name
    return(u)
  }else{
    return(sf::st_crs(x@crs))
  }
})


#' @rdname crs
#' @export
setReplaceMethod("crs", signature="GPRsurvey", function(x, value){
  # value <- as.character(value)[1]
  
  #------------------- check arguments
  msg <- checkArgInit()
  # msg <- checkArg(value,  msg, "LENGTH", c(1, length(x@names)))
  msg <- checkArg(value,  msg, "LENGTH", 1)
  checkArgStop(msg)
  #------------------- end check
  
  # x@crs <- sapply(value, .checkCRS, USE.NAMES = FALSE)
  x@crs <- .checkCRS(value)
  x@spunit <- crsUnit(x@crs)
  message("Replacing CRS does not reproject the data.\n", 
          "Use `projectCoords()` for that")
  return(x)
})


# # for class GPRsurvey!
# # FIXME -> delete! ONLY USED in coercion_spatial.R for sp.
# .getCheckedCRS <- function(x){
#   if(length(x@crs) == 0 || all(is.na(x@crs))){
#     warning("no CRS defined!\n")
#   }else{
#     if(length(unique(x@crs)) > 1){
#       warning( "Not all the coordinate reference systems are identical: \n",
#                "Check with 'crs(x)'!\n",
#                "I take the first one!") 
#     } 
#   }
#   return( sp::CRS(x@crs[1]) )
# }

# .checkCRSsurvey <- function(x){
#   x_crs <- sapply(x, .checkCRS, USE.NAMES = FALSE)
#   ucrs <- unique(x_crs[!is.na(x_crs)])
#   if(length(ucrs) == 1){
#     return( ucrs )
#   }else if(length(ucrs) == 0){
#     return(NA_character_)
#   }else{
#     return( x_crs )
#   }
# }


#' Check if the CRS is valid
#' @param x wkt code, crs class of sf package, integer or EPSG:#### (length 1)
#' @return (`character`) WKT code.
#' @noRd
.checkCRS <- function(x){
  tryCatch(sf::st_crs(x)$wkt,
           error = function(e){
             message("Invalid CRS! Try something like 'EPSG:3857'!")
             return(NA_character_)
           })
}
  # if(is.list(x)) x <- unlist(x, use.names = FALSE)[1]
  # if(length(x) > 1){
  #   return( sapply(x, .checkCRS , USE.NAMES = FALSE) )
  # } 
  # if(is.na(x) || (is.character(x) && x == "")){
  #   return(NA_character_)
  # }else if(inherits(x, "CRS")){
  #   return(unname(x@projargs))
  # }else if(inherits(x, "crs")){
  #   # .checkCRS(x[[1]])
  #   .checkCRS(x$proj4string)
  # }else{
  #   test_num <- verboseF(as.numeric(x), verbose = FALSE)
  #   if(!is.na(test_num) && test_num %% 1 == 0 && test_num > 0){
  #     x <- paste0("+init=epsg:", as.integer(x))
  #     return(sf::st_crs(x)$proj4string)
  #     # return(sf::st_crs(x)[[2]])
  #   }else if(is.character(x)){
  #     return(sf::st_crs(x)$proj4string)
  #   }else{
  #     warning("I could not understand the CRS...")
  #     return(NA_character_)
  #   }
  # }
