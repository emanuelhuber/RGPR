#' Normal Move-Out streching
#' 
#' Compute the Normal Move-Out (NMO) streching for a data set given a constant velocity.
#' The NMO streching is defined by \deqn{S_{NMO} = \frac{\Delta_{NMO}}{t_0}}, where
#' \deqn{t_0 = t_{TWT}(x = 0) = \frac{2z}{v}} is the vertical two-way traveltime at zero offset. 
#' @param x An object of the class \code{GPR}
#' @param v A length-one numeric vector defining the radar wave velocity in 
#'          the ground
#' @name NMOstreching
setGeneric("NMOstreching", function(x, v = NULL) 
  standardGeneric("NMOstreching"))

#' @rdname NMOstreching
#' @export
setMethod("NMOstreching", "GPR", function(x, v = NULL){
  # if(any(x@z0 > 0)){
  #   stop("You must first shift the traces to time-zero with\n",
  #        "'shiftToTime0()'")
  # }
  # if(is.null(v)){
  #   stop("You must assign a positiv numerical value to 'v'!")
  # }
  # S_NMO <- NMO(x, v) / x@z
  # S_NMO@data[is.infinite(S_NMO@data)] <- 0
  # S_NMO@dlab <- "NMO strech"
  # S_NMO@dunit <- ""
  # 
  # proc(S_NMO) <- getArgs()
  if(any(x@z0 > 0)){
    stop("You must first shift the traces to time-zero with\n",
         "'shiftToTime0()'")
  }
  if(!isZunitTime(x)){
    stop("The signal is a function of depth and not time. If you\n",
         "absolutely want to apply 'NMOstreching()', change the unit with\n",
         "xunit(x) <- 'm', for example.")
  }
  if(is.null(v)){
    if(length(x@vel) == 0){
      stop("You must assign a positiv numerical value to 'v'!")
    }else{
      if(is.null(x@vel[["v"]])){
        x <- interpVel(x, type = "vrms", method = "pchip")
      }
      v <- x@vel[["v"]]
    }
  }
  if(anyNA(x@antsep)){
    stop("You must first set the antenna separation distances with\n",
         "'antsep(x) <- ...")
  }
  S_NMO <- x
  if(!isCMP(x)){
    S_NMO <- S_NMO[, 1]
    S_NMO@time <- numeric(0)
    S_NMO@marker <- ""
    S_NMO@ann <- ""
    S_NMO@mode <- "CMP"
    S_NMO@coord <- matrix(nrow = 0, ncol = 3)
    S_NMO@rec <- matrix(nrow = 0, ncol = 3)
    S_NMO@trans <- matrix(nrow = 0, ncol = 3)
    S_NMO@angles <- numeric(0)
  }else{
    if(length(x@antsep) != ncol(x)){
      stop("The length of the antenna separation distances must equal",
           " to the number of columns of x. Use\n",
           "'antsep(x) <- ...")
    }
  }
  S_NMO[] <- outer(x@z, x@antsep, .NMOstreching, v = v)
  S_NMO@dlab <- "NMO streching"
  S_NMO@dunit <- ""
  
  # not necessarily for CMP data but we do it to be safe.
  S_NMO@xlab <- "antenna separation"
  S_NMO@x <- x@antsep
  
  proc(S_NMO) <- getArgs()
  return(S_NMO)
})

.NMOstreching <- function(t0, antsep, v){
  1 - t0/ (sqrt(t0^2 + (antsep/v)^2) ) 
}