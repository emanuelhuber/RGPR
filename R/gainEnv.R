setGeneric("gainEnv", function(x, global = FALSE, FUN = mean, track = TRUE) 
  standardGeneric("gainEnv"))



#' Gain compensation based on envelop 
#' 
#' @name gainEnv
#' @rdname gainEnv
#' @export
setMethod("gainEnv", "GPR", function(x, global = TRUE, FUN = mean, track = TRUE){
    if(isTRUE(global)){
      x_mean <- traceStat(envelope(x), FUN = FUN)
      # plot(x[, 10])
      # lines(x_env[, 10], col="red")
      # lines(x4_env_mean, col="green")
      # plot(max(x4_env_mean) - x4_env_mean)
      g0 <- as.vector(x_mean/max(x_mean))
    }else{
      xenv <- envelope(x)
      xenvmax <- apply(xenv, 2, max, na.rm = TRUE)
      g0 <- xenv@data / xenvmax
    }
    g <- 1/g0
    g[!is.finite(g)] <- 1
    return(x * g)
    # sel <- seq_along(g0) > which.max(g0)
    # g0[!sel] <- 1
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
} 
)
