#' @name filter2D
#' @rdname filter2D
#' @export
setGenericVerif("filter2D", function(x, type=c("median3x3", "adimpro"), 
                                     ..., track = TRUE) 
  standardGeneric("filter2D"))


#' Two-dimensional filters
#' 
#' @name filter2D
#' @rdname filter2D
#' @export
setMethod("filter2D", "GPR", function(x, 
                                      type = c("median3x3", "adimpro"), 
                                      ...,
                                      track = TRUE){
  type <- match.arg(type, c("median3x3", "adimpro"))
  if(type == "median3x3"){
    x@data <-  .medianFilter3x3(x@data)
  }else if( type == "adimpro"){
    IMG <- x@data
    IMG <- (IMG-min(IMG))/(max(IMG)-min(IMG))
    adimg <- adimpro::make.image(IMG)
    # img.smooth <- adimpro::awsimage(adimg, hmax = 2)
    # img.smooth <- adimpro::awsimage(adimg, hmax = 2)
    # img.smooth <- adimpro::awsaniso(adimg, hmax = 2,...)
    img.smooth <- adimpro::awspimage(adimg,...)
    AA <- adimpro::extract.image(img.smooth)
    AAA <- ( (AA - mean(AA))/sd(AA) ) * sd(x@data)
    x@data <- AAA
  }
  if(isTRUE(track)) proc(x) <- getArgs()
  #     x@proc <- c(x@proc, proc)
  return(x)
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