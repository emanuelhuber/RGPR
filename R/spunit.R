
#' Spatial unit
#' 
#' Spatial unit of the trace coordinates
#' @param x      [\code{GPR class}] An object of the class \code{GPR}
#' @param value  [\code{character(1)}] Spatial units: "m", "feet", etc.
#'                                     see units package.
#' @return [\code{GPR class}] An object of the class \code{GPR}
#' @name spunit
setGeneric("spunit", function(x) 
  standardGeneric("spunit"))


#' @rdname spunit
#' @aliases spunit<-,GPR-method
setGeneric("spunit<-",function(x,value){standardGeneric("spunit<-")})


#-------------------------------- GPR -----------------------------------------#

#' @rdname spunit   
#' @export
setMethod("spunit", "GPR", function(x){
  return(x@spunit)
})

#' @rdname spunit
#' @export
setReplaceMethod("spunit", signature="GPR", function(x, value){
  if(is.na(x@crs)){
    x@spunit <- .checkUnit(value)
    x@proc <- c(x@proc, "spunit<-")
  }else{
    warning("Cannot change the spatial unit because it depends on the CRS. ",
            "Either update the CRS with 'crs(x) <- ...' or ",
            "'remove' the CRS with 'crs(x) <- NA'")
  }
  return(x)
})


#-------------------------------- GPRsurvey -----------------------------------#

#' @rdname spunit   
#' @export
setMethod("spunit", "GPRsurvey", function(x){
  return(x@spunit)
})


#' @rdname spunit
#' @export
setReplaceMethod("spunit", signature="GPRsurvey", function(x, value){
  if(is.na(x@crs)){
    x@spunit <- .checkUnit(value)
  }else{
    warning("Cannot change the spatial unit because it depends on the CRS. ",
            "Either update the CRS with 'crs(x) <- ...' or ",
            "'remove' the CRS with 'crs(x) <- NA'")
  }
  return(x)
})

# used in GPRsurvey.R
.checkSpunitsurvey <- function(x){
  sp_unit  <- unique(x[x != ""])
  if(length(sp_unit) == 1){
    return( .checkUnit(rep(sp_unit, length(x))) )
  }else if(length(sp_unit) == 0){
    return(x)
  }else{
    return( .checkUnit(x) )
  }
}
