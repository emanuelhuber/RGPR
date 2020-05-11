#' Normal Move-Out correction
#' 
#' Remove the Normal Move-Out (NMO) from the trace given a constant velocity: 
#' this is a non-linear 
#' correction of the time axis that requires interpolation. Note that
#' only the conventional NMO correction is currently implemented. The 
#' conventional NMO introduces a streching effect. A nonstretch NMO will
#' be implemented in a near future. The Normal Move-out is defined as the
#' difference between the two-way time at a given offset and the two-way 
#' zero-offset time.
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
#' @references
#' \itemize{
#'   \item{Tillard and Dubois (1995) Analysis of GPR data: wave propagation
#'         velocity determination. Journal of Applied Geophysics, 33:77-91}
#'   \item{Shatilo and Aminzadeh (2000) Constant normal-moveout (CNMO) 
#'         correction: a technique and test results. Geophysical Prospecting,
#'         473-488}
#' }
#' @name correctNMO
setGeneric("correctNMO", function(x, thrs = NULL, v = NULL, 
                              method = c("linear", "nearest", 
                                                      "pchip", "cubic", "spline")) 
  standardGeneric("correctNMO"))

#' @rdname correctNMO
#' @export
setMethod("correctNMO", "GPR", function(x, thrs = NULL, v = NULL, 
                                    method = c("linear", "nearest", "pchip",   
                                               "cubic", "spline")){
  method <- match.arg(method[1], c("spline", "linear", "nearest", "pchip", 
                                "cubic"))
  # method <- method[1]
  if(any(x@z0 > 0)){
    stop("You must first shift the traces to time-zero with\n",
         "'shiftToTime0()'")
  }
  if(!isZunitTime(x)){
    stop("The signal is a function of depth and not time. If you\n",
         "absolutely want to apply 'correctNMO()', change the unit with\n",
         "xunit(x) <- 'm', for example.")
  }
  if(anyNA(x@antsep)){
    stop("You must first set the antenna separation distances with\n",
         "'antsep(x) <- ...")
  }
  if(isCMP(x)){
    if(length(x@antsep) != ncol(x)){
      stop("The length of the antenna separation distances must equal",
           " to the number of columns of x. Use\n",
           "'antsep(x) <- ...")
    }
  }
  asep <- x@antsep
  if(length(asep) == 1){
    asep <- rep(asep, ncol(x))
  }
  
  if(is.null(v)){
    if(is.null(x@vel[["v"]])){
     x <- interpVel(x, type = "vrms", method = "pchip")
    }
    v <- x@vel[["v"]]
  } 
  
  if(!is.null(thrs)){
    # SEL <- NMOstreching(x)@data > thrs
    x[NMOstreching(x)@data > thrs] <- NA
  }
  
  x <- .NMOCor(x, v = v, asep = asep, method = method)
  proc(x) <- getArgs()
  return(x)
})

# c("linear", "nearest", "pchip",   "cubic", "spline")
.NMOCor <- function(x, v = NULL, asep = NULL, method = "pchip"){
  x_nmoCor <- x
  x_nmoCor@data[] <- 0
  # if(is.null(v)){
  #   v <- x@vel[[1]]
  # }
  # works when v is a vector.
  tt <- outer(x@z, x@antsep, .t_NMO, v = v)
  for(i in seq_along(x)){
    # tt <- sqrt( x@z^2 + (asep[i]^2 )/v^2 )
    valreg <- signal::interp1(x  = x@z, 
                              y  = x@data[, i],
                              xi = tt[, i], 
                              method = method, 
                              extrap = NA)
    
    x_nmoCor@data[,i] <- valreg
  }
  # x_nmoCor@data[is.na(x_nmoCor@data)] <- 0
  x_nmoCor@data[is.infinite(x_nmoCor@data)] <- NA
  x_nmoCor@x <- asep
  return(x_nmoCor)
}

.t_NMO <- function(t0, antsep, v){
  sqrt(t0^2 + (antsep/v)^2)
}