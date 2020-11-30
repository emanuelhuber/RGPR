

#' Set velocities
#' 
#' set velocities
#' @param x [\code{GPR}] GPR object
#' @param v [\code{numeric(n)}] \code{n} velocity values
#' @param twt [\code{numeric(n)}] \code{n} two-way travel time values associated
#'            with the velocities \code{v}
#' @param type [\code{"vrms"}|\code{vint}] Kind of velocities \code{v}: 
#'             root-mean-square velocities \code{"vrms"} (genrally inferred
#'             from CMP data) or internal velocities \code{"vint"} (layer 
#'             velocities).
#' @return [\code{GPR}]
#' @name velSet
#' @rdname velSet
setGeneric("velSet", function(x, v, twt, type = c("vrms", "vint")) standardGeneric("velSet"))


#' @rdname velSet
#' @export
setMethod("velSet", "GPR", function(x, v, twt, type = c("vrms", "vint")){
  type <- match.arg(type, c("vrms", "vint"))
  if(length(v) != length(twt)){
    stop("'v' and 'twt' must have the same length!")
  }
  i <- order(twt)
  v <- v[i]
  twt <- twt[i]
  if(type == "vrms"){
    v_dix <- velDix(twt = twt, 
                    v = v)
    v <- list("vrms" = list("t"    = twt,
                            "v"    = v,
                            "intp" = "stairs"),
              "vint" = list("t"    = v_dix$t,
                            "v"    = v_dix$v,
                            "intp" = "stairs"))
  }else{
    v <- list("vint" = list("t"    = twt, 
                            "v"    = v,
                            "intp" = "stairs"))
  }
  x@vel <- v
  return(x)
})

