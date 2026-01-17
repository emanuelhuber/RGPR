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
setMethod("filter2D", "GPRvirtual", function(obj, 
                                      type = c("median3x3", "adimpro", "gaussian"), 
                                      ...,
                                      track = TRUE){
  type <- match.arg(type, c("median3x3", "adimpro", "gaussian"))
  obj@data <- .filter2D(obj@data, type = type, ...)
  if(isTRUE(track)) proc(obj) <- getArgs()
  #     obj@proc <- c(obj@proc, proc)
  return(obj)
} 
)

#' @rdname filter2D
#' @export
setMethod("filter2D", "GPRslice", function(obj, 
                                      type = c("median3x3", "adimpro", "gaussian"), 
                                      ...,
                                      track = TRUE){
  type <- match.arg(type, c("median3x3", "adimpro", "gaussian"))
  obj@data <- .filter2D(obj@data, type = type, ...)
  if(isTRUE(track)) proc(obj) <- getArgs()
  #     obj@proc <- c(obj@proc, proc)
  return(obj)
} 
)

#' @rdname filter2D
#' @export
setMethod("filter2D", "GPRcube", function(obj, 
                                           type = c("median3x3", "adimpro", "gaussian"), 
                                           ...,
                                           track = TRUE){
  type <- match.arg(type, c("median3x3", "adimpro", "gaussian"))
  for(i in 1:dim(obj)[3]){
    obj@data[,,i] <- .filter2D(obj@data[,,i], type = type, ...)
  }
  if(isTRUE(track)) proc(obj) <- getArgs()
  #     obj@proc <- c(obj@proc, proc)
  return(obj)
} 
)



.filter2D <- function(A, 
                      type = c("median3x3", "adimpro", "gaussian"), 
                      ...,
                      track = TRUE){
  if(type == "median3x3"){
    return( .medianFilter3x3(A) )
  }else if( type == "adimpro"){
    IMG <- (A-min(A))/(max(A)-min(A))
    adimg <- adimpro::make.image(IMG)
    # img.smooth <- adimpro::awsimage(adimg, hmax = 2)
    # img.smooth <- adimpro::awsimage(adimg, hmax = 2)
    # img.smooth <- adimpro::awsaniso(adimg, hmax = 2,...)
    img.smooth <- adimpro::awsaniso(adimg, ...)
    AA <- adimpro::extract.image(img.smooth)
    return( ( (AA - mean(AA))/sd(AA) ) * sd(A) )
  }else if(type == "gaussian"){
    return( mmand::gaussianSmooth(A, ...) )
  }
}

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