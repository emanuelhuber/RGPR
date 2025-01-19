
#' Adapative weights 2D smoothing
#' 
#' A wrapper for the following `adimpro` functions: 
#' [adimpro::awsaniso()], [adimpro::awsimage()], and
#'            [adimpro::awsimage()].
#' @param x (`GPR`) 
#' @param method (`character[1]`) `awsaniso` stand for anisotropic
#'               adaptive weights smoothing 
#'               (call the function [adimpro::awsaniso()]),
#'               `aws` stand for adaptive weights smoothing using
#'               a local constant model
#'               (call the function [adimpro::awsimage()]), and
#'               `awsp` stand for adaptive weights smoothing using a
#'                local polynomial models up to a degree of 2.
#'               (call the function [adimpro::awsimage()]).
#' @param ... additional parameters to be passed to the `adimpro` 
#'            functions: [adimpro::awsaniso()],
#'            [adimpro::awsimage()], and
#'            [adimpro::awsimage()].
#' @return (`GPR`)
#' @name smooth2D
setGeneric("smooth2D", function(x, method = c("awsaniso", "aws", "awsp"), ...) 
  standardGeneric("smooth2D"))

#' @rdname smooth2D
#' @export
setMethod("smooth2D", "GPR", 
          function(x, method = c("awsaniso", "aws", "awsp"), ...) {

    method <- match.arg(method, c("awsaniso", "aws", "awsp"))
    
    IMG <- x@data
    IMG <- (IMG-min(IMG))/(max(IMG)-min(IMG))
    adimg <- adimpro::make.image(IMG)
    # img.smooth <- adimpro::awsimage(adimg, hmax = 2)
    # img.smooth <- adimpro::awspimage(adimg, hmax = 2)
    # img.smooth <- adimpro::awsaniso(adimg, hmax = 2,...)
    if(method == "awsaniso"){
      img.smooth <- adimpro::awsaniso(adimg, ...)
    }else  if(method == "aws"){
      img.smooth <- adimpro::awsimage(adimg, ...)
    }else  if(method == "awsp"){
      img.smooth <- adimpro::awspimage(adimg, ...)
    }else{
      stop("An error that should never be raised...")
    }
    AA <- adimpro::extract.image(img.smooth)
    AAA <- ( (AA - mean(AA))/sd(AA) ) * sd(x@data)
    x@data <- AAA
    
    return(x)
})
