
#' Interpolate velocity model
#' 
#' Define the interpolation parameters that will be used when the velocities 
#' will be plotted or used in other functions.
#' To undo the interpolation, simply set `method = stairs`.
#' @param x (`GPR class`) An object of the class `GPR`
#' @param type (`character[1]`) Which type of velocity values has to be
#'             updated? The root-mean-square velocity (`vrms`) or the
#'             internal velocity (`vint`)?
#' @param method (`character[1]`) Interpolation method to be applied:
#'               one of `stairs`, `linear`, `nearest`, 
#'               `pchip`, `spline`, `cubic` 
#'               (see also [signal::interp1()]). 
#' @return (`GPR class`) An object of the class GPR.
#' @name velInterp
setGeneric("velInterp", function(x, 
                                 type = c("vrms", "vint"),
                                 method = c("stairs", "linear", "nearest", 
                                            "pchip", "cubic", "spline")) 
  standardGeneric("velInterp"))

#' @rdname velInterp
#' @export
setMethod("velInterp", "GPR", 
          function(x, 
                   type = c("vrms", "vint"),
                   method = c("stairs", "linear", "nearest",
                              "pchip", "cubic", "spline")){
  type <- match.arg(type, c("vrms", "vint"))
  method <- match.arg(method, c("stairs", "linear", "nearest", "pchip", "cubic", "spline"))
  x@vel[[type]][["intp"]] <- method
  return(x)
})



.intpSmoothVel <- function(x_vel_i, x_z){
  if(is.list(x_vel_i) && !is.null(x_vel_i$intp)){
    if(x_vel_i$intp == "stairs"){
      v_stairs <- approxfun(x_vel_i[["t"]], x_vel_i[["v"]], 
                            rule = 2, method = "constant", f = 1)
      x_vel_i[["v"]] <- v_stairs(x_z)
      x_vel_i[["t"]] <- x_z
      
    }else{
      print(x_vel_i$intp)
      x_vel_i[["v"]]  <- signal::interp1(x = x_vel_i[["t"]], y = x_vel_i[["v"]],
                                         xi = x_z, method = x_vel_i$intp,
                                         extrap = TRUE)
      x_vel_i[["t"]] <- x_z
      x_vel_i[["intp"]] <- "stairs"
    }
    if(!is.null(x_vel_i$smooth) && x_vel_i$smooth > 0){
      x_vel_i[["v"]]  <- mmand::gaussianSmooth(x_vel_i[["v"]], sigma = x_vel_i$smooth)
    }
  }
  return(x_vel_i)
}


.intpSmoothAllVel <- function(x_vel, x_z){
  if(length(x_vel) > 0){
    # interpolate velocities
    for(i in seq_along(x_vel)){
      # vi <- x_vel[[i]]
      x_vel[[i]] <- .intpSmoothVel(x_vel[[i]], x_z)
      
    }
  }
  return(x_vel)
}
