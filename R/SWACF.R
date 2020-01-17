# use function ".acf" from "trACF()"


setGeneric("SWACF", function(x, w = 100) 
  standardGeneric("SWACF"))


#' @export
setMethod("SWACF", "GPR", function(x, w = 100){
  w <- round(w / x@dz)
  X <- wapplyRow(x = as.matrix(x), width = w, by = 1, FUN = .applyACF, 
                 lag.max = w, plot = FALSE)
  X <- aperm(X, perm = c(3, 2, 1))
  dim(X)
  xs <- as.GPRset.GPR(x[seq_len(dim(X)[1]), ])
  xs@data <- X
  
  
  xs@sets <-  seq_len(w)
  xs@setnames <- paste0("lag", seq_len(w))
  xs@setunit <- "lag"
  proc(xs) <- getArgs()
  return(xs)
})


.applyACF <- function(x, lag.max = NULL,
                      type = c("correlation", "covariance", "partial"),
                      plot = TRUE, na.action = na.fail, demean = TRUE, ...){
  apply(x, 2, .acf, lag.max = lag.max,
        type = type,
        plot = plot, na.action = na.action, demean = demean, ...)
}
