

#' @export
setAs(from = "GPRsurvey", to = "SpatialLines",
      def = function (from) as.SpatialLines(from))



#' Coerce object to an object of the class SpatialLines
#'
#' Coerce object to an object of the class SpatialLines
#' 
#' @param x      [\code{class GPR|GPRsurvey}]
#' @return [\code{SpatialLines object}] An object of the class GPR.
# #' @seealso \code{\link{time0Cor}} to shift the traces such that they start
# #'          at time-zero.
#' @name as.SpatialLines
setGeneric("as.SpatialLines", function(x) 
  standardGeneric("as.SpatialLines"))

#' @rdname as.SpatialLines
#' @export
setMethod("as.SpatialLines", signature(x = "GPRsurvey"), function(x){
  # remove NULL from list
  # FIXME
  # Filter(Negate(is.null), x) = alternative
  isNotNull <- !sapply(x@coords, is.null)
  if(any(isNotNull)){
    xyz <- x@coords[isNotNull]
    lineList <- lapply(unname(xyz), .xyToLine)
    linesList <- lapply(seq_along(lineList), .lineToLines, lineList, 
                        names(xyz))
    mySpatLines <- sp::SpatialLines(linesList)
    
    sp::proj4string(mySpatLines) <- .getCheckedCRS(x)
    
    return(mySpatLines)
  }else{
    warning("no coordinates!")
    return(NULL)   
  }
})




#' @export
setAs(from = "GPRsurvey", to = "SpatialPoints",
      def = function (from) as.SpatialPoints(from))    



#' Coerce object to an object of the class SpatialPoints
#'
#' Coerce object to an object of the class SpatialPoints
#' 
#' @param x      [\code{class GPR|GPRsurvey}]
#' @return [\code{SpatialPoints object}] An object of the class GPR.
# #' @seealso \code{\link{time0Cor}} to shift the traces such that they start
# #'          at time-zero.
#' @name as.SpatialPoints
setGeneric("as.SpatialPoints", function(x) 
  standardGeneric("as.SpatialPoints"))



#' @rdname as.SpatialPoints
#' @export
setMethod("as.SpatialPoints", signature(x = "GPRsurvey"), function(x){
  allTopo <- do.call(rbind, x@coords)  #  N, E, Z
  allTopo2 <- as.data.frame(allTopo)
  names(allTopo2) <- c("x", "y", "z")
  sp::coordinates(allTopo2) <- ~ x + y
  
  sp::proj4string(allTopo2) <- .getCheckedCRS(x)
  
  return(allTopo2)
})

.xyToLine <- function(x){
  sp::Line(x[,1:2])
}

.lineToLines <- function(i,pp, myNames){
  sp::Lines(pp[i],myNames[i])
}
