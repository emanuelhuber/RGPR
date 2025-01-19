
#' @name shiftGridLine
#' @rdname shiftGridLine
#' @export
setGeneric("shiftGridLine", function(x, i, dx = 0, dy = 0, dz = 0){ 
  standardGeneric("shiftGridLine")})


#' Shift trace positions of one GPR data
#'
#' Shift trace positions of GPR data \code{i} by \code{dx} along x-axis and
#' by \code{dy} along y-axis.
#' @param x (`GPRsurvey`) Index of the line to be shifted
#' @param i (`numeric[1]`) Index of the line to be shifted
#' @param dx (`numeric[1]`) How much should the line be shifted in the x-direction (in spatial unit).
#' @param dy (`numeric[1]`) How much should the line be shifted in the y-direction (in spatial unit).
#' @param dz (`numeric[1]`) How much should the line be shifted in the z-direction (in spatial unit).
#' @rdname shiftGridLine
#' @export
setMethod("shiftGridLine", "GPRsurvey", function(x, i, dx = 0, dy = 0, dz = 0){
  # if you want to shift the coordinates by 1 m along x-direction, 
  # 0.5 m along the y-direction
  # for your 3rd GPR data line, do that
  if(is.numeric(i)){
    x@coords[[i]] <- t(t(x@coords[[i]]) + c(dx, dy , dz))
  }else if(is.character(i)){
    i <- which(x@names == i)
    x@coords[[i]] <- t(t(x@coords[[i]]) + c(dx, dy , dz))
  }else{
    stop("type not defined!")
  }
  return(x)
})