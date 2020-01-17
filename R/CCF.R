
setGeneric("CCF", function(x,
                                lag.max   = 100,
                                type      = c("correlation", "covariance", "partial"),
                                na.action = na.fail, 
                                demean    = TRUE, 
                                ...) 
standardGeneric("CCF"))


#' Auto-correlation function 
#' 
#' @name CCF
#' @rdname ACF
#' @export
setMethod("CCF", "GPR", function(x, 
                lag.max   = 100,
                type      = c("correlation", "covariance", "partial"),
                na.action = na.fail, 
                demean    = TRUE, 
                ...){
  Xout <- as.GPRset.GPR(x)
  Xout_data <- acf(x@data,  #, 2, .acf, 
                lag.max = lag.max, 
                type = type,
                plot = FALSE,
                na.action = na.action,
                demean = demean,
                ...)
  Xout@data <- aperm(Xout_data$acf, c(3, 2, 1))
  Xout@sets <-  seq_len(dim(Xout)[3])
  Xout@setnames <- paste0("lag", seq_len(dim(Xout)[3]))
  Xout@setunit <- "lag"
  Xout@depth <- x@pos
  Xout@depthunit <- Xout@posunit
  Xout@time0 <- rep(0, ncol(x))
  proc(Xout) <- getArgs()
  return(Xout)
})

