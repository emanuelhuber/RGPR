setGenericVerif("plot2PDF", function(x, fPath = NULL, addTopo = FALSE, 
                                      clip = NULL, normalize = NULL,  ...) 
  standardGeneric("plot2PDF"))

#' Export a PDF showing the GPR profile.
#'
#' @name plot2PDF
#' @rdname plot2PDF
#' @export
setMethod("plot2PDF", "GPR", 
          function(x, fPath = NULL, addTopo = FALSE, clip = NULL, normalize = NULL, 
                    type = "raster",...){
            if(is.null(fPath)){
              stop("fPath must be given\n")
            }
            if(any(dim(x) == 1)){
              stop("no export because dim = 1\n")
            }
            plot(x, clip = clip, addTopo = addTopo, type = type, 
                 pdfName = fPath, normalize = normalize,  ...)
          }
)