#' @name firstBreakToTime0
#' @rdname firstBreakToTime0
#' @export
setGeneric("firstBreakToTime0",
           function(x, fb, c0 = 0.299)
             standardGeneric("firstBreakToTime0"))

#' Convert first wave break time to time-zero
#'
#' Account for the delay time between time of wave emission and time of first
#' wave break recording due to the antenna separation (offset).
#' 
#' @param x (`GPR`)
#' @param fb (`numeric[1|n]`) Time of first break (`n = ncol(x)`)
#' @param c0 (`numeric[1]`)  Propagation speed of the GPR wave 
#'               through air in unit of space per unit of time 
#'               (generally in m/ns).
#' @rdname firstBreakToTime0
#' @export
setMethod("firstBreakToTime0", "GPR",  function(x, fb, c0 = 0.299){
  if(length(x@antsep) == 0 || (!is.numeric(x@antsep))){
    stop("You must first define the antenna separation",
         "with `antsep(x)<-...`!")
  }
  fb - x@antsep/c0
})
