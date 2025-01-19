#' Get velocity model as GPR object
#' 
#' Return the velocity model (either the root-mean square or internal velocity).
#' 
#' @param x (`GPR class`) An object of the class `GPR`
#' @param type (`vrm|vint`) Set the velocity you want (either root-mean square or internal velocity)
#' @return (`GPR class`) An object of the class `GPR` containing the velocity model.
#' @name getVel
#' @rdname getVel
#' @concept velocity model
setGeneric("getVel", function(x, type = c("vrms", "vint")) standardGeneric("getVel"))

#' @rdname getVel
#' @export
setMethod("getVel", "GPR", function(x, type = c("vrms", "vint")){
  velAsGPR(x, type = type)
})



velAsGPR <- function(x, type = c("vrms", "vint")){
  type <- match.arg(type, c("vrms", "vint"))
  x_vel <- .getVel(x, type = type, strict = FALSE)
  if(is.null(dim(x_vel))){
    x <- x[,1]
  }else{
    x <- x[1:nrow(x_vel),1:ncol(x_vel)]
  }
  x@mode <- "velModel"
  x@antsep <- 0
  x@vel <- list()
  x@x <- 0
  x@data[] <- x_vel
  
  x@dunit <- paste0(x@xunit, "/", x@zunit)
  x@xlab <- "position" 
  x@dlab <- "velocity"
  x@name <- "Velocity"
  x@z0 <- x@z0
  # x_tv@time0 <- 0
  #proc(x) <- getArgs()
  return(x)
}
