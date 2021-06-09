setGenericVerif("crop", function(x, xlim = NULL, ylim = NULL, track = TRUE) 
  standardGeneric("crop"))

#' Crop GPR data
#' 
#' Crop GPR data based on trace position (\code{xlim}) and/or 
#' two-way travel time/depth (\code{ylim}). One of the arguments \code{xlim} and
#' \code{ylim} can be set equal to \code{NULL}. In this case, the whole data
#' range wi
#' 
#' @param x GPR data
#' @param xlim [\code{numeric(2)|NULL}] Trace position range. If equal to 
#'             \code{NULL} the range is equal to the data range.
#' @param ylim [\code{numeric(2)|NULL}] two-way travel time/depth range. 
#'             If equal to \code{NULL} the range is equal to the data range.
#' @name crop
#' @rdname crop
#' @export
setMethod("crop", "GPR", function(x, xlim = NULL, ylim = NULL,
                                  track = TRUE){
  if(!is.null(xlim)){
    xlim <- sort(xlim)
    xsel <- x@pos >= xlim[1] & x@pos <= xlim[2]
  }else{
    xsel <- seq_len(ncol(x))
  }
  if(!is.null(ylim)){
    xylim <- sort(ylim)
    ysel <- x@depth >= ylim[1] & x@depth <= ylim[2]
  }else{
    ysel <- seq_len(nrow(x))
  }
  
  x <- x[ysel, xsel]
  if(isTRUE(track)) proc(x) <- getArgs()
  #   x@proc <- c(x@proc, proc)
  return(x)
} 
)