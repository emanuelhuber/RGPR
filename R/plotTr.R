#' Plot all the traces in one 1D plot
#' 
#' @param x Object of the class GPR
#' @param add [\code{logical(1)}] If \code{TRUE}, add to current plot.
#' @param ... Arguments to be passed to \code{plot}/\code{line}
#' @name plotTr
setGeneric("plotTr", function(x, add = FALSE,  ...)
  standardGeneric("plotTr"))


#' @rdname plotTr
#' @export
setMethod(
  f = "plotTr",
  signature = "GPRvirtual",
  definition = function(x, add = FALSE, ...){
    if(ncol(x) == 1){
      plot(x, add = add,  ...)
    }else{
      dots <- list(...)
      if(!is.null(dots[["log"]]) && dots[["log"]] == "y"){
        dots[["log"]] <- ""
        x <- log(x)
        # if(is.null(dots[["ylab"]])){
        #   #FIXME : use .ylab (with option for log)
        #   dots[["ylab"]] <- "amplitude envelope (log mV)"
        # }
      }
      #FIXME : use .ylab (with option for log)
      # if(is.null(dots[["ylab"]])) dots[["ylab"]] <- "amplitude envelope (mV)"
      if(is.null(dots[["ylab"]])) dots[["ylab"]] <- .dlab(x)
      
      dotsLine <- list()
      dotsLine[["X"]] <- x
      dotsLine[["FUN"]] <- lines
      dotsLine[["x"]] <- x@z
      
      dotsLine[["lty"]] <- dots[["lty"]]
      dotsLine[["lwd"]] <- dots[["lwd"]]
      dotsLine[["col"]] <- dots[["col"]]
      # be carefull (dots$type redefined below)
      dotsLine[["type"]] <- dots[["type"]]  
      dotsLine[["pch"]] <- dots[["pch"]]
      
      if(is.null(dotsLine[["col"]])){
        dotsLine[["col"]] <- rgb(0, 0, 0, 0.1)
      }
      
      if(is.null(dots[["ylim"]])){
        dots[["ylim"]] <- range(x@data, na.rm = TRUE)
        if(dots[["ylim"]][1] > 0){
          dots[["ylim"]][1] <- 0
        }else{
          dots[["ylim"]] <- max(abs(dots[["ylim"]])) * c(-1, 1)
        }
      }
      if(length(dim(x@data)) == 2){
        dotsLine[["MARGIN"]] <- 2
        dots[["x"]] <- x[, 1]
      }else{
        dotsLine[["MARGIN"]] <- c(2, 3)
        dots[["x"]] <- x[, , 1][,1]
      }
      dots[["y"]] <- NULL
      dots[["type"]] <- "n"
      if(isFALSE(add)){
        # dots[["add"]] <- NULL
        invisible( do.call(plot, dots) )
      }    
        # print("OK")
      invisible(do.call(apply, dotsLine))
    }
  }
)
