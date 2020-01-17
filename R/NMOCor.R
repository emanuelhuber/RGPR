
#------------------------- CMP ANALYSIS -----------------------------------#

#' @name NMOCor
#' @rdname NMOCor-methods
#' @exportMethod NMOCor
setGenericVerif("NMOCor", function(x, v = NULL) 
  standardGeneric("NMOCor"))



#' Normal Move-Out correction
#' 
#' Remove the Normal Move-Out (NMO) from the trace given a constant velocity: 
#' this is a non-linear 
#' correction of the time axis that requires interpolation. Note that
#' only the conventional NMO correction is currently implemented. The 
#' conventional NMO introduces a streching effect. A nonstretch NMO will
#' be implemented in a near future.
#' 
#' Assuming a horizontal reflecting plane and homogeneous medium, the two-way
#' bistatic travel time of the reflected wave 
#' for an antenna separation \eqn{x} follows directly from the Pythagorean 
#' theorem:
#' \deqn{t_{TWT}(x,z) = \sqrt{\frac{x^2}{v^2} + \frac{4z^2}{v^2}}}
#' where \eqn{t_{TWT}(x)} is the two-way travel time at antenna
#' separation \eqn{x} of the wave reflected at depth \eqn{z} with propagation
#' velocity \eqn{v}. This equation defines an hyperbola (keep \eqn{z} constant,
#' increase the antenna separation \eqn{x} and you obtain a hyperbola similar
#' to the reflection signals you obtain with common-mid point survey).
#' The idea behind NMO-correction is to correct the signal for the antenna 
#' separation (offset) and therefore to transform the signal to the signal we 
#' would have recorded with zero offset (\eqn{x = 0}). We write the vertical
#' two-way traveltime at zero offset 
#' \deqn{t_0 = t_{TWT}(x = 0) = \frac{2z}{v}}
#' Therefore, the NMO-correction \eqn{\Delta_{NMO}} is
#' \deqn{\Delta_{NMO} = t_{TWT}(x) - t_0}  
#' \deqn{\Delta_{NMO} = t_0 (\sqrt{1 + \frac{x^2}{v^2 t_0^2}} - 1)}
#' @param x An object of the class \code{GPR}
#' @param v A length-one numeric vector defining the radar wave velocity in 
#'          the ground
#' @rdname NMOCor-methods
#' @aliases NMOCor,GPR-method
#' @export
#' @references
#' \itemize{
#'   \item{Tillard and Dubois (1995) Analysis of GPR data: wave propagation
#'         velocity determination. Journal of Applied Geophysics, 33:77-91}
#'   \item{Shatilo and Aminzadeh (2000) Constant normal-moveout (CNMO) 
#'         correction: a technique and test results. Geophysical Prospecting,
#'         473-488}
#' }
setMethod("NMOCor", "GPR", function(x, v = NULL){
  if(!isCMP(x)){
    stop("survey mode of 'x' is not multi-offset. ",
         "update survey mode:\n",
         "  surveymode(x) <- 'CMP'\n",
         " or\n",
         "  surveymode(x) <- 'WARR'\n")
  }
  if(is.null(v)){
    v <- x@vel[[1]]
  } 
  x <- .NMOCor(x, v = v, asep = x@antsep)
  proc(x) <- getArgs()
  return(x)
}
)

.NMOCor <- function(x, v = NULL, asep = NULL){
  if(is.null(asep)){
    if(length(x@rec) == 0 || length(x@trans) == 0){
      asep <- seq(x@antsep, by = x@dx, length.out = length(x))
    }else{
      asep <- sqrt(colSums((x@rec - x@trans)^2))
    }
  }
  t0 <- round(x@time0[1]/x@dz)*x@dz
  tx0 <- x@depth - t0   # t(x = 0)
  #x <- x[(t0/x@dz + 1):nrow(x),]
  #x@time0 <- 0
  print(v)
  x_nmoCor <- x
  x_nmoCor@data[] <- 0
  if(is.null(v)){
    v <- x@vel[[1]]
  }
  for(i in seq_along(x)){
    tt <- sqrt( tx0^2 + (asep[i]^2 )/v^2 )
    tt[tx0 < 0] <- NA
    valreg <- signal::interp1(x = tx0, y = x@data[, i], xi = tt, 
                              method = "spline", extrap = NA)
    # deltaT <- sqrt( x@depth^2 + (asep[i]^2 )/v^2 )
    # newT <- 2*x@depth - deltaT
    # test <- newT > 0
    # valreg <- signal::interp1(x = newT[test], y = x@data[, i], 
    #                           xi = x@depth[test], 
    #                           method = "cubic", extrap = NA)
    x_nmoCor@data[,i] <- valreg
  }
  x_nmoCor@data[is.na(x_nmoCor@data)] <- 0
  x_nmoCor@data[is.infinite(x_nmoCor@data)] <- 0
  x_nmoCor@pos <- asep
  return(x_nmoCor)
}
