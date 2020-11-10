


#' Plot Velocities
#' 
#' Plot Velocities
#' @export
plotVel <- function(x){
  if(length(x@vel) > 0){
    x@vel <- .intpSmoothAllVel(x@vel, x@z)
    v_lim <- range(sapply(x@vel, .getAllVel))
    plot(0, type = "n", ylim = rev(range(x_tv@z)),  xlim = v_lim, yaxs = "i",
         xlab = RGPR:::.vlab(x),
         ylab = RGPR:::.zlab(x))
    if(!is.null(x@vel[["vrms"]])){
      # v_rms <- approxfun(x@vel[["vrms"]][["t"]], x@vel[["vrms"]][["v"]], rule = 2, method = "constant", f = 1)
      lines(x@vel[["vrms"]][["v"]], x@vel[["vrms"]][["t"]], type = "s", lty = 1)
    }
    if(!is.null(x@vel[["vint"]])){
      # v_int <- approxfun(x@vel[["vint"]][["t"]], x@vel[["vint"]][["v"]], rule = 2, method = "constant", f = 1)
      # lines(v_int(x_z), x_z, type = "s", lty = 3)
      lines(x@vel[["vint"]][["v"]], x@vel[["vint"]][["t"]], type = "s", lty = 3)
    }
    if(!is.null(x@vel[["v"]]) && is.numeric(x@vel[["v"]])){
      lines(x@vel[["v"]], x_z, type = "s", lty = 2, col = "red")
    }
  }else{
    if(isZDepth(x)){
      stop(msg_set_zunitToDepth)
    }else{
      stop("")
    }
  }
}

.getAllVel <- function(x){
  if(inherits(x, "list") && !is.null(x[["v"]])){
    return(x[["v"]])
  }else if(is.numeric(x)){
    return(x)
  }
}