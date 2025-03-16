
#' Get depth
#' 
#' Retunrs an object identical to `obj` but with values equal to the signal
#' depth
#' @param obj    (`GPR* object`) An object of the class `GPR`
#' @return (`GPR* object`) with signal as a function of depth.
#' @name getDepth
#' @concept signal processing
setGeneric("getDepth", 
           function(obj) 
             standardGeneric("getDepth"))


#' @rdname getDepth
#' @export
setMethod("getDepth", "GPR", function(obj){
  if(length(obj@vel) == 0) stop(msg_no_vel)
  if(any(obj@z0 > 0))      stop(msg_do_shiftToTime0)
  if(!isZTime(obj))        stop(msg_set_zunitToDepth)
  obj_vel <- .getVel(obj, type = "vint", strict = FALSE)
  objdepth <- timeToDepth(twt = obj@z, t0 = 0, v = obj_vel, 
                          antsep = obj@antsep) 
  if(inherits(objdepth, "matrix")){
    obj@data <- objdepth
  }else{
    obj < obj[, 1]
  }
  obj@mode <- "depth"
  obj@vel <- list()
  obj@data[] <- objdepth
  obj@dunit <- paste0(obj@xunit)
  obj@xlab <- "position" 
  obj@dlab <- "depth"
  obj@name <- "Depth"
  return(obj)
})
