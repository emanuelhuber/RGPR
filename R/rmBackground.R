setGeneric("rmBackground", function(x, w = NULL, FUN = mean, ...,
                                    track = TRUE) 
  standardGeneric("rmBackground"))

#' Remove background
#'
#' \code{rmBackground} is a generic function used to produce results defined
#' by an user function. The user function is applied accross traces (horizontal) 
#' using a moving window. Note that if the moving window length is not defined, 
#' all traces are averaged into one single trace (the results is similar to
#' \code{apply(x, 1, FUN, ...)}. \code{rmBackground} is a wrapper for
#' \code{x - traceStat(...)}
#' 
#' @param x An object of the class GPR
#' @param w A length-one integer vector equal to the window length of the 
#'          average window. If \code{w = NULL} a single trace corresponding to
#'          the average trace of the whole profile is returned.
#' @param FUN A function to compute the average (default is \code{mean})
#' @param ... Additional parameters for the FUN functions
#' @return An object of the class GPR. When \code{w = NULL}, this function 
#'         returns a GPR object with a single trace corresponding to the 
#'         average trace of the whole radargram. When \code{w} is equal to a
#'         strictly positive interger this function returns a GPR object with
#'         a size identical to x where each trace corresponds to the average
#'         of the \code{w} neighbouring traces centered on the considered trace.
#' @examples
#' data("frenkeLine00")
#' 
#' f0 <- frenkeLine00
#' 
#' # substract the average trace
#' f1 <- rmBackground(f0)
#' 
#' f2 <- rmBackground(f0, w = 20)
#' plot(f2)
#' 
#' f3 <- rmBackground(f0, w = 20, FUN = median)
#' plot(f3)
#' @name rmBackground
#' @rdname rmBackground
#' @export
setMethod("rmBackground", "GPR", function(x, w = NULL, FUN = mean, ...,
                                       track = TRUE){
  x <- x - traceStat(x, w = w, FUN = FUN, ...,
                       track = FALSE)
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
})