

#' Coordinate reference system (CRS) of the GPR data
#' 
#' Coordinate reference system (CRS) of the GPR data
#' 
#' Modified slots
#' \itemize{
#'   \item \code{crs} the coordinate reference system.
#'   \item \code{spunit} the spatial units are updated accroding to the new
#'                       coordinate reference system.
#' }
#' @param x      [\code{GPR class}] An object of the class \code{GPR}
#' @param value  [\code{sp::CRS|character}] CRS object (one or more)
#' @return [\code{GPR class}] An object of the class \code{GPR}
#' @name crs
setGeneric("crs", function(x) 
  standardGeneric("crs"))


#' @rdname crs
#' @aliases crs<-,GPR-method
setGeneric("crs<-",function(x,value){standardGeneric("crs<-")})

 
#' @rdname crs   
#' @export
setMethod("crs", "GPR", function(x){
  return(x@crs)
})

#' @rdname crs
#' @export
setReplaceMethod("crs", signature="GPR", function(x, value){
    # value <- as.character(value)[1]
    x@crs    <- .checkCRS(value)
    x@spunit <- crsUnit(x@crs)
    x@proc   <- c(x@proc, "crs<-")
    return(x)
})



#' @rdname crs   
#' @export
setMethod("crs", "GPRsurvey", function(x){
  return(x@crs)
})


#' @rdname crs
#' @export
setReplaceMethod("crs", signature="GPRsurvey", function(x, value){
  # value <- as.character(value)[1]
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(value,  msg, "LENGTH", c(1, length(x@names)))
  checkArgStop(msg)
  #------------------- end check
  
  # x@crs <- sapply(value, .checkCRS, USE.NAMES = FALSE)
  x@crs <- .checkCRSsurvey(value)
  x@spunit <- crsUnit(x@crs)
  return(x)
})


# for class GPRsurvey!
# FIXME -> delete! ONLY USED in coercion_spatial.R for sp.
.getCheckedCRS <- function(x){
  if(length(x@crs) == 0 || all(x@crs == "")){
    warning("no CRS defined!\n")
  }else{
    if(length(unique(x@crs)) > 1){
      warning( "Not all the coordinate reference systems are identical: \n",
               "Check with 'crs(x)'!\n",
               "I take the first one!") 
    } 
  }
  return( sp::CRS(x@crs[1]) )
}

.checkCRSsurvey <- function(x){
  x_crs <- sapply(x, .checkCRS, USE.NAMES = FALSE)
  ucrs <- unique(x_crs[!is.na(x_crs)])
  if(length(ucrs) == 1){
    return( ucrs )
  }else if(length(ucrs) == 0){
    return(NA_character_)
  }else{
    return( x_crs )
  }
}


# x = character, integer or CRSobj (length 1 or more)
.checkCRS <- function(x){
  if(is.na(x) || (is.character(x) && x == "")){
    return(NA_character_)
  }else if(inherits(x, "CRS")){
    return(unname(x@projargs))
  }else if(inherits(x, "crs")){
    return(x$proj4string)
  }else{
    test_num <- verboseF(as.numeric(x), verbose = FALSE)
    if(!is.na(test_num) && test_num %% 1 == 0 && test_num > 0){
      x <- paste0("+init=epsg:", as.integer(x))
      return(sf::st_crs(x)$proj4string)
      # return(sf::st_crs(x)[[2]])
    }else if(is.character(x)){
      return(sf::st_crs(x)$proj4string)
    }else{
      warning("I could not understand the CRS...")
      return(NA_character_)
    }
  }
}
