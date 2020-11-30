#--- internal velocities

#' Computes Dix velocities
#' 
#' Computes Dix velocities from root-mean-square velocity and corresponding
#' two-way travel times
#' 
#' @param twt [\code{numeric(n)}] $n$ two-way travel times ($n > 0$)
#' @param v   [\code{numeric(n)}] $n$ corresponding velocities
#' @return [\code{list}] List with two elements: \code{t} (time) and
#'         [\code{v}] (corresponding Dix velocities)
#' @export
velDix <- function(twt, v){
  if(twt[1] != 0){
    twt <- c(0, twt)
  }
  if(v[1] != 0){
    v <- c(0, v)
  }
  twt_v2 <- twt * v^2
  vint <- sqrt(diff(twt_v2)/diff(twt))
  sel <- is.na(vint)
  if(sum(sel) > 0){
    message(rep("*", sum(sel)))
  }
  return(list(t = twt[-1], v = vint))
}
