#' Appla Normal Move-Out (NMO) correction and stack the traces
#' 
#' Removes the Normal Move-Out (NMO) from the trace given a velocity and 
#' stacks (sums) the traces.
#' @param x An object of the class \code{GPR}
#' @param thrs [\code{numeric(1)|NULL}] Definite the threshold for muting
#'             (i.e., suppressing) the values where the NMO-stretching is
#'             above the threshold. Setting \code{thrs = NULL}, the full data
#'             will be used.
#' @param v A length-one numeric vector defining the radar wave velocity in 
#'          the ground
#' @param method [\code{character(1)}] Interpolation method to be applied:
#'               one of \code{pchip}, \code{linear}, \code{nearest}, 
#'               \code{spline}, \code{cubic} 
#'               (see also \code{\link[signal]{interp1}}). 
#' @name NMOstack
setGeneric("NMOstack", function(x, thrs = NULL, v = NULL, 
                                method = c("linear", "nearest", "pchip",   
                                           "cubic", "spline"))
  standardGeneric("NMOstack"))



#' @rdname NMOstack
#' @export
setMethod("NMOstack", "GPR", function(x, thrs = NULL, v = NULL, 
                                      method = c("linear", "nearest", "pchip",   
                                                 "cubic", "spline")){
  # NMOstack <- function(x, thrs = NULL){
  x_NMOcor <- NMOcorrect(x, thrs = thrs, v = v, method = method)
  x <- rowMeans(x_NMOcor, na.rm = TRUE)    
  return(x)
})