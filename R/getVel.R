#' Get velocity model as GPR object
#' 
#' Return the velocity model (either the root-mean square or internal velocity).
#' 
#' @param obj (`GPR class`) An object of the class `GPR`
#' @param type (`vrm|vint`) Set the velocity you want (either root-mean square or internal velocity)
#' @return (`GPR class`) An object of the class `GPR` containing the velocity model.
#' @name getVel
#' @rdname getVel
#' @concept velocity model
setGeneric("getVel", function(obj, type = c("vint", "vrms")) standardGeneric("getVel"))

#' @rdname getVel
#' @export
setMethod("getVel", "GPR", function(obj, type = c("vint", "vrms")){
  velAsGPR(obj, type = type)
})



velAsGPR <- function(obj, type = c("vint", "vrms")){
  type <- match.arg(type, c("vint", "vrms"))
  obj_vel <- .getVel(obj, type = type, strict = FALSE)
  if(is.null(dim(obj_vel))){
    obj <- obj[,1]
    obj@antsep <- 0
    obj@x <- 0
  }else{
    obj <- obj[1:nrow(obj_vel),1:ncol(obj_vel)]
  }
  obj@mode <- "velModel"
  obj@vel <- list()
  obj@data[] <- obj_vel
  
  obj@dunit <- paste0(obj@xunit, "/", obj@zunit)
  obj@xlab <- "position" 
  obj@dlab <- "velocity"
  obj@name <- "Velocity"
  #obj@z0 <- obj@z0
  # obj_tv@time0 <- 0
  #proc(obj) <- getArgs()
  return(obj)
}
