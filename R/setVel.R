#' Set velocities
#' 
#' set velocities.
#' @param x [\code{GPR}] GPR object
#' @param v [\code{numeric(n)}] \code{n} velocity values
#' @param twt [\code{numeric(n)}] \code{n} two-way travel time values associated
#'            with the velocities \code{v}. Optional.
#' @param type [\code{"vrms"}|\code{vint}] Kind of velocities \code{v}: 
#'             root-mean-square velocities \code{"vrms"} (genrally inferred
#'             from CMP data) or internal velocities \code{"vint"} (layer 
#'             velocities).
#' @return [\code{GPR}]
#' @name setVel
#' @rdname setVel
setGeneric("setVel", function(x, v, twt, type = c("vrms", "vint")) standardGeneric("setVel"))


#' @rdname setVel
#' @export
setMethod("setVel", "GPR", function(x, v, twt, type = c("vrms", "vint")){
  
  type <- match.arg(type, c("vrms", "vint"))
  # print(type)
  if(missing(twt)){
    x@vel <- list("v" = checkVelIntegrity(x, v))
  }else{
    if(length(v) != length(twt)){
      stop("'v' and 'twt' must have the same length!")
    }
    i <- order(twt)
    v <- v[i]
    twt <- twt[i]
    if(type == "vrms"){
      v_dix <- velDix(twt = twt, 
                      v = v)
      sel <- !is.na(v_dix$v)
      v <- list("vrms" = list("t"    = twt[sel],
                              "v"    = v[sel],
                              "intp" = "stairs"),
                "vint" = list("t"    = v_dix$t[sel],
                              "v"    = v_dix$v[sel],
                              "intp" = "stairs"))
      if(sum(!sel) > 0){
        warning("You have ", sum(!sel), " time-velocity pair(s) for which the internal velocity is non-real!\n",
                "Nothing dramatic, I just removed this/these pair(s). It can happens\n",
                "- if the travel time intervals are small or\n",
                "- if the NMO velocity change is large.\n")
        
      } 
    }else{
      v <- list("vint" = list("t"    = twt, 
                              "v"    = v,
                              "intp" = "stairs"))
    }
    x@vel <- v
  }
  return(x)
})