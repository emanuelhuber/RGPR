

#' Plot Velocities
#' 
#' Plot Velocities (only 1D for now)
#' @param x [\code{GPR class}] An object of the class \code{GPR}
#' @name plotVel
#' @rdname plotVel
setGeneric("plotVel", function(x) standardGeneric("plotVel"))

#' @rdname plotVel
#' @export
setMethod("plotVel", "GPR", function(x){
  if(length(x@vel) > 0){
    x@vel <- .intpSmoothAllVel(x@vel, x@depth)
    v_lim <- range(sapply(x@vel, .getAllVel))
    plot(0, type = "n", ylim = rev(range(x@depth)),  xlim = v_lim, yaxs = "i",
         xlab = paste0("velocity (", x@posunit, "/", x@depthunit, ")"),
         ylab = paste0("two-way travel time (", x@depthunit, ")"))
    if(!is.null(x@vel[["vrms"]])){
      lines(x@vel[["vrms"]][["v"]], x@vel[["vrms"]][["t"]], type = "s", lty = 1)
    }
    if(!is.null(x@vel[["vint"]])){
      lines(x@vel[["vint"]][["v"]], x@vel[["vint"]][["t"]], type = "s", lty = 3)
    }
    if(!is.null(x@vel[["v"]])){ 
      if(is.numeric(x@vel[["v"]])){
        lines(x@vel[["v"]], x@depth, type = "s", lty = 2, col = "red")
      }else{
        lines(x@vel[["v"]][["v"]], x@vel[["v"]][["t"]], type = "s", lty = 1)
      }
    }
    if(length(x@vel) == 1 && is.null(names(x@vel)) && !is.null(x@vel[[1]]) && is.numeric(x@vel[[1]])){
      lines(x@vel[[1]], x@depth, type = "s", lty = 1, col = "black")
    }
  }else{
    if(isDepthDepth(x)){
      stop(msg_set_zunitToDepth)
    }else{
      stop("")
    }
  }
})

.getAllVel <- function(x){
  if(inherits(x, "list") && !is.null(x[["v"]])){
    return(x[["v"]])
  }else if(is.numeric(x)){
    return(x)
  }
}