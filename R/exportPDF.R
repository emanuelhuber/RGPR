setGenericVerif("exportPDF", function(x, fPath = NULL, addTopo = FALSE, 
                                      clip = NULL, normalize = NULL, nupspl = NULL, ...) 
  standardGeneric("exportPDF"))

#' Export a PDF showing the GPR profile.
#'
#' @name exportPDF
#' @rdname exportPDF
#' @export
setMethod("exportPDF", "GPR", 
          function(x, fPath = NULL, addTopo = FALSE, clip = NULL, normalize = NULL, 
                   nupspl = NULL,  type = "wiggles",...){
            if(is.null(fPath)){
              stop("fPath must be given\n")
            }
            if(any(dim(x) == 1)){
              stop("no export because dim = 1\n")
            }
            plot(x, clip = clip, addTopo = addTopo, type = type, 
                 pdfName = fPath, normalize = normalize, nupspl = nupspl, ...)
          }
)