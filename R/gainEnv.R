#' Gain compensation based on envelop 
#' 
#' @name gainEnv
#' @rdname gainEnv
#' @export
setGeneric("gainEnv", function(x, FUN = mean, track = TRUE) 
  standardGeneric("gainEnv"))




#' @rdname gainEnv
#' @export
setMethod("gainEnv", "GPR", function(x, FUN = mean, track = TRUE){
  x_mean <- apply(amplEnv(x), 1, FUN = FUN)
  # plot(x[, 10])
  # lines(x_env[, 10], col="red")
  # lines(x4_env_mean, col="green")
  # plot(max(x4_env_mean) - x4_env_mean)
  g0 <- as.vector(x_mean/max(x_mean))
  sel <- seq_along(g0) > which.max(g0)
  g0[!sel] <- 1
  g <- 1/g0
  g[!is.finite(g)] <- 1
  return(x * g)
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
} 
)