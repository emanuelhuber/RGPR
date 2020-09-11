#' Normal Move-Out
#' 
#' Compute the Normal Move-Out (NMO) for a data set given a constant velocity (?? FIXME): 
#' The Normal Move-out is defined as the
#' difference between the two-way time at a given offset and the two-way 
#' zero-offset time.
#'  We write the vertical
#' two-way traveltime at zero offset 
#' \deqn{t_0 = t_{TWT}(x = 0) = \frac{2z}{v}}
#' Therefore, the NMO-correction \eqn{\Delta_{NMO}} is
#' \deqn{\Delta_{NMO} = t_{TWT}(x) - t_0}  
#' \deqn{\Delta_{NMO} = t_0 (\sqrt{1 + \frac{x^2}{v^2 t_0^2}} - 1)}
#' @param x An object of the class \code{GPR}
#' @param v A length-one numeric vector defining the radar wave velocity in 
#'          the ground
#' @name NMO
setGeneric("NMO", function(x, v = NULL) 
  standardGeneric("NMO"))



#' @rdname NMO
#' @export
setMethod("NMO", "GPR", function(x, v = NULL){
  if(any(x@z0 > 0)){
    stop(msg_do_shiftToTime0)
  }
  if(!isZTime(x)){
    stop(msg_set_zunitToDepth)
  }
  if(is.null(v)){
    # if(length(x@vel) == 0){
    #   stop("You must assign a positiv numerical value to 'v'!")
    # }else{
    #   if(is.null(v)){
    #     if(is.null(x@vel[["v"]])){
    #       x <- interpVel(x, type = "vrms", method = "pchip")
    #     }
    #     v <- x@vel[["v"]]
    #   } 
    # }
    v <- .getVel(x)
  }
  if(anyNA(x@antsep)){
    stop(msg_set_antsep)
  }
  D_NMO <- x
  if(!isCMP(x)){
    D_NMO <- D_NMO[, 1]
    D_NMO@time <- numeric(0)
    D_NMO@marker <- ""
    D_NMO@ann <- ""
    D_NMO@mode <- "CMP"
    D_NMO@coord <- matrix(nrow = 0, ncol = 3)
    D_NMO@rec <- matrix(nrow = 0, ncol = 3)
    D_NMO@trans <- matrix(nrow = 0, ncol = 3)
    D_NMO@angles <- numeric(0)
  }else{
    if(length(x@antsep) != ncol(x)){
      stop("The length of the antenna separation distances must equal",
           " to the number of columns of x. Use\n",
           "'antsep(x) <- ...")
    }
  }
  D_NMO[] <- outer(x@z, x@antsep, .NMO, v = v)
  D_NMO@dlab <- "NMO"
  D_NMO@dunit <- D_NMO@zunit
  
  # not necessarily for CMP data but we do it to be safe.
  D_NMO@xlab <- "antenna separation"
  D_NMO@x <- x@antsep
  
  proc(D_NMO) <- getArgs()
  return(D_NMO)
})


.NMO <- function(t0, antsep, v){
  sqrt(t0^2 + (antsep/v)^2) - t0
}


# return either 1 value, a vector or FIXME: a matrix
.getVel <- function(x, type = c("vrms", "vint")){
  type <- match.arg(type, c("vrms", "vint"))
  if(length(x@vel) == 0){
    stop("You must assign a positiv velocity value!")
  }else{
    if(is.null(x@vel[[type]])){
      stop("You must first set this type of velocity: ", type)
    }else{
      if(!is.null(x@vel[["type"]][["intp"]])){
        v <- .interpVel(x, type = type, method = x@vel[[type]][["intp"]])
      }
      if(!is.null(x@vel[["type"]][["smooth"]])){
        v <-  mmand::gaussianSmooth(v, sigma = x@vel[["type"]][["smooth"]]) 
      }
    }
    v <- x@vel[["v"]]
    return(v)
  }
}


.interpVel <- function(x, 
                      type = c("vrms", "vint"),
                      method = c("stairs", "linear", "nearest", 
                                 "pchip", "cubic", "spline")){
  type <- match.arg(type, c("vrms", "vint"))
  method <- match.arg(method, c("stairs", "linear", "nearest", "pchip", "cubic", "spline"))
  if(method == "stairs"){
    v_stairs <- approxfun(x@vel[[type]][["t"]], x@vel[[type]][["v"]], 
                          rule = 2, method = "constant", f = 1)
    v <- v_stairs(x@z)
  }else{
   v <- signal::interp1(x = x@vel[[type]][["t"]], y = x@vel[[type]][["v"]],
                                    xi = x@z, method = method,
                                    extrap = TRUE)
  }
  return(v)
}