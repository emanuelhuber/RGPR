#' Two-dimensional filters
#' 
#' Two-dimensional filters
#' @param obj (`GPR* object`)
#' @param type (`character[1]`) Filter method.
#' @param ... (`*`) Additional parameters
#' @param track (`logical[1]`) Should the processing step be tracked? 
#' @name filter2D
#' @rdname filter2D
#' @export
#' @concept processing
setGeneric("filter2D", function(obj, type=c("median3x3", "adimpro", "gaussian"), 
                                     ..., track = TRUE) 
  standardGeneric("filter2D"))


#' @rdname filter2D
#' @export
setMethod("filter2D", "GPR", function(obj, 
                                      type = c("median3x3", "adimpro", "gaussian"), 
                                      ...,
                                      track = TRUE){
  type <- match.arg(type, c("median3x3", "adimpro", "gaussian"))
  if(type == "median3x3"){
    obj@data <-  .medianFilter3x3(obj@data)
  }else if( type == "adimpro"){
    IMG <- obj@data
    IMG <- (IMG-min(IMG))/(max(IMG)-min(IMG))
    adimg <- adimpro::make.image(IMG)
    # img.smooth <- adimpro::awsimage(adimg, hmax = 2)
    # img.smooth <- adimpro::awsimage(adimg, hmax = 2)
    # img.smooth <- adimpro::awsaniso(adimg, hmax = 2,...)
    img.smooth <- adimpro::awsaniso(adimg, ...)
    AA <- adimpro::extract.image(img.smooth)
    AAA <- ( (AA - mean(AA))/sd(AA) ) * sd(obj@data)
    obj@data <- AAA
  }else if(type == "gaussian"){
    obj@data <- mmand::gaussianSmooth(obj@data, ...)
  }
  if(isTRUE(track)) proc(obj) <- getArgs()
  #     obj@proc <- c(obj@proc, proc)
  return(obj)
} 
)

.medianFilter3x3 <- function(A){
  B <- A  # <- matrix(0, 364,364)
  for(i in 1:(nrow(A)-2)) {
    for(j in 1:(ncol(A)-2) ) {
      xm <- A[i+0:2, j+0:2]
      B[i+1, j+1] <- xm[order(xm)[5]]
    }
  }  
  return(B)
}