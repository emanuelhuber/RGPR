setGenericVerif("plot2PNG", function(x, fPath = NULL, pngFac = pngFac, addTopo = FALSE, 
                                      clip = NULL, normalize = NULL, nupspl = NULL, ...) 
  standardGeneric("plot2PNG"))

#' Export a PNG showing the GPR profile.
#'
#' @name plot2PNG
#' @rdname plot2PNG
#' @export
setMethod("plot2PNG", "GPR", 
          function(x, fPath = NULL, pngFac = 20, addTopo = FALSE, clip = NULL, normalize = NULL, 
                   nupspl = NULL,  type = "raster",...){
            if(is.null(fPath)){
              stop("fPath must be given\n")
            }
            if(any(dim(x) == 1)){
              stop("no export because dim = 1\n")
            }
            plot(x, clip = clip, addTopo = addTopo, type = type, pngFac = pngFac,
                 pngName = fPath, normalize = normalize, nupspl = nupspl, ...)
          }
)