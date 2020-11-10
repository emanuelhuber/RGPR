

#' Set velocities
#' 
#' set velocities
#'   @export
setVel <- function(x, v, twt, type = c("vrms", "vint")){
  type <- match.arg(type, c("vrms", "vint"))
  if(length(v) != length(twt)){
    stop("'v' and 'twt' must have the same length!")
  }
  i <- order(twt)
  v <- v[i]
  twt <- twt[i]
  if(type == "vrms"){
    v <- list("vrms" = list("t"    = twt,
                            "v"    = v,
                            "intp" = "stairs"),
              "vint" = c(dixVel(twt = twt, v = v),
                         "intp" = "stairs"))
  }else{
    v <- list("vint" = list("t"    = twt, 
                            "v"    = v,
                            "intp" = "stairs"),
    )
  }
  x@vel <- v
  return(x)
}

