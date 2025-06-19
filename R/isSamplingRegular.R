
#' Is the sampling along all axis regular
#' 
#' Is the sampling along all axis regular
#' @param obj (`GPR*` object)
#' @param axes (`numeric[n]`) Along x-axis = 1, along y-axis = 2
#' @return (`logical[1]`)
#' @name isSamplingRegular
#' @rdname isSamplingRegular
#' @export
setGeneric("isSamplingRegular", function(x, axes = NULL) 
  standardGeneric("isSamplingRegular"))



#' @rdname isSamplingRegular
#' @export
setMethod("isSamplingRegular", "GPR", function(x, axes = NULL){
  tst <- TRUE
  if(is.null(axes)) axes <- c(1, 2)
  if(1 %in% axes){
   tst <- tst & isTRUE(all.equal(sd(diff(x@x)), 0))
  }
  if(2 %in% axes){
    tst <- tst & isTRUE(all.equal(sd(diff(x@z)), 0))
  }
  return(tst)
})