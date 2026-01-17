#' Switch polarity 
#' 
#' 
#' @param obj    (`GPR`) An object of the class GPR.
#' @param track (`logical[1]`) Should the processing step be tracked?
#' @return (`GPR`) An object of the class GPR whose traces are dewowed.
#' @name switchPolarity
#' @rdname switchPolarity
#' @export
#' @concept processing
setGeneric("switchPolarity", 
           function(obj, ..., track = TRUE)
             standardGeneric("switchPolarity"))

#' @rdname switchPolarity
#' @export
setMethod("switchPolarity", "GPR", function(obj, track = TRUE){
  x@data <- -x@data
  if(isTRUE(track)) proc(obj) <- getArgs()
  return(obj) 
})

#' @rdname switchPolarity
#' @export
setMethod("switchPolarity", "GPRsurvey", function(obj, sel, track = TRUE){
  if(!all(sel %in% seq_along(obj))) stop("'sel' must range between 1 and ", length(obj), "!")
  for(i in sel){
    y <- verboseF( obj[[i]], verbose = FALSE )
    y@data <- -y@data
    obj@paths[i] <- .saveTempFile(y)
  }
  return(obj) 
})