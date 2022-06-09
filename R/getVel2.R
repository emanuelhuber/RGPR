get
#' Get velocity model
#' 
#' Return the velocity model (either the root-mean square or internal velocity).
#' 
#' @param x [\code{GPR class}] An object of the class \code{GPR}
#' @param type [\code{vrm|vint}] Set the velocity you want (either root-mean square or internal velocity)
#' @return [\code{GPR class}] An object of the class \code{GPR} containing the velocity model.
#' @name getVel2
#' @rdname getVel2
setGeneric("getVel2", function(x, type = c("vrms", "vint")) standardGeneric("getVel2"))

#' @rdname getVel2
#' @export
setMethod("getVel2", "GPR", function(x, type = c("vrms", "vint")){
  velAsGPR(x, type = type)
})



velAsGPR <- function(x, type = c("vrms", "vint")){
  type <- match.arg(type, c("vrms", "vint"))
  x_vel <- .getVel2(x, type = type, strict = FALSE)
  if(is.null(dim(x_vel))){
    x <- x[,1]
  }else{
    x <- x[1:nrow(x_vel),1:ncol(x_vel)]
  }
  x@surveymode <- "velModel"
  x@antsep <- 0
  x@vel <- list()
  x@pos <- numeric()
  x@data[] <- x_vel
  
  # x@dunit <- paste0(x@xunit, "/", x@zunit)
  # x@xlab <- "position" 
  # x@dlab <- "velocity"
  x@name <- "Velocity"
  x@time0 <- numeric()
  # x_tv@time0 <- 0
  #proc(x) <- getArgs()
  return(x)
}