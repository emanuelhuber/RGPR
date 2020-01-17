setGenericVerif("gain", function(x, 
                                 type=c("power", "exp", "agc"), 
                                 ..., 
                                 track = TRUE) 
  standardGeneric("gain"))



#----------------- 1D-SCALING (GAIN)
#' Gain compensation
#' 
#' @name gain
#' @rdname gain
#' @export
setMethod("gain", "GPR", function(x, 
                                  type = c("power", "exp", "agc"), 
                                  ..., 
                                  track = TRUE){
  type <- match.arg(type, c("power", "exp", "agc"))
  x@data[is.na(x@data)] <-0
  if(type == "power"){
    # alpha, dts, t0 = NULL, te = NULL, tcst = NULL
    x@data <- .gainPower(x@data, dts = x@dz, ...)
  }else if(type == "exp"){
    # alpha, dts, t0 = NULL, te = NULL
    x@data <- .gainExp(x@data, dts = x@dz, ...)
  }else if(type == "agc"){
    x@data <- .gainAgc(x@data, dts = x@dz, ...)
  }
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
} 
)

# dts = sampling time (e.g., 0.8 ns)
# t0 = starting time to apply the gain scaling
# te = ending time to apply the gain scaling
# tcst
# CF Yilmaz, p85
.gainPower <- function(A, alpha, dts, t0 = NULL, te = NULL,
                       tcst = NULL){
  g <- .gainPower0(A[,1], alpha, dts, t0, te, tcst)
  Anew <- A * g
  #s1 = ((max(A))-(min(A)));  # scale factor
  #s2 = ((max(Anew))-(min(Anew)));  # scale factor
  #return(Anew/s2*s1 )
  return( Anew / sd(Anew) * sd(A))
}

.gainPower0 <- function(d, alpha, dts, t0 = NULL, te = NULL, tcst = NULL){
  if(is.null(t0)) t0 <- 0
  if(is.null(te)) te <- (length(d) - 1) * dts
  if(!is.null(tcst) && !(tcst > t0 && tcst < te)){
    stop("you need tcst > t0 && tcst < te\n")
  }
  ## FIX ME > instead of "(seq_along(d) - 1) *dts" use directly
  ## x <- gpr@depth !!!  
  x <- (seq_along(d) - 1) *dts
  test <- x >= t0 & x <= te
  g <- rep(1, length(d))
  g[test] <- 1 + (seq_along(d[test])*dts )^alpha
  g[x > te] <- max(g)
  if(!is.null(tcst) && any(x < tcst)){
    g[x < tcst] <- g[1 + floor(tcst/dts)]
  }
  return( g)
}

.gainExp <- function(A, alpha, dts, t0 = NULL, te = NULL){
  g <- .gainExp0(A[,1], alpha, dts, t0, te)
  Anew <- A * g
  return( Anew / sd(Anew) * sd(A))
}

.gainExp0 <- function(d, alpha, dts, t0 = NULL, te = NULL){
  if(is.null(t0) || t0==0) t0 <-0
  if(is.null(te)) te <-(length(d)-1)*dts
  ## FIX ME > instead of "(seq_along(d) - 1) *dts" use directly
  ## x <- gpr@depth !!! 
  x <- (seq_along(d) - 1) * dts
  test <- (x >= t0 & x <= te)
  test_max <- x > te
  g <- rep(1L,length(d))
  g[test] <-  exp((x[test] - t0) * dts * alpha)
  g[test_max] <-  exp(max(x[test] - t0)*dts*alpha)
  return( g)
}


