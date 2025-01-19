


#' Plot Velocities
#' 
#' Plot Velocities (only 1D for now)
#' @param x (`GPR class`) An object of the class `GPR`
#' @name plotVel
#' @rdname plotVel
setGeneric("plotVel", function(x) standardGeneric("plotVel"))

#' @rdname plotVel
#' @export
#' @concept plot
setMethod("plotVel", "GPR", function(x){
  if(length(x@vel) > 0){
    x@vel <- .intpSmoothAllVel(x@vel, x@z)
    v_lim <- range(sapply(x@vel, .getAllVel))
    plot(0, type = "n", ylim = rev(range(x@z)),  xlim = v_lim, yaxs = "i",
         xlab = .vlab(x),
         ylab = .zlab(x))
    if(!is.null(x@vel[["vrms"]])){
      lines(x@vel[["vrms"]][["v"]], x@vel[["vrms"]][["t"]], type = "s", lty = 1)
    }
    if(!is.null(x@vel[["vint"]])){
      lines(x@vel[["vint"]][["v"]], x@vel[["vint"]][["t"]], type = "s", lty = 3)
    }
    if(!is.null(x@vel[["v"]]) && is.numeric(x@vel[["v"]])){
      lines(x@vel[["v"]], x@z, type = "s", lty = 2, col = "red")
    }
  }else{
    message(msg_no_vel)
    return(NULL)
    # if(isZDepth(x)){
    #   stop(msg_set_zunitToDepth)
    # }else{
    #   stop("")
    # }
  }
})

.getAllVel <- function(x){
  if(inherits(x, "list") && !is.null(x[["v"]])){
    return(x[["v"]])
  }else if(is.numeric(x)){
    return(x)
  }
}
