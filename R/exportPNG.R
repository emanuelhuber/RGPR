


setGeneric("exportPNG", function(x, target = raw(), markers = NULL, ...) 
  standardGeneric("exportPNG"))


#' Export GPR data as PNG file
#' 
#' Export GPR data as black-and-white PNG file with pixel dimension equal to 
#' the sample dimension of the GPR data. There is the possibility to add
#' white pixels to identify the fiducial markers on the PNG file.
#' 
#' @param x [\code{GPR}]
#' @param target Either name of the file to write, a binary connection or a 
#'               raw vector (see \link{\code{[png]{writePNG}}})
#' @param markers [\code{integer(n)}] Pixel index set to white at marker
#'                                    position (e.g., \code{markers = 1:5} to
#'                                    set the first 5 pixels white at marker
#'                                    position).
#' @param ... See additional arguments to be passed to
#'             \link{\code{[png]{writePNG}}})
#' @export
setMethod("exportPNG", "GPR", function(x, target = raw(), markers = NULL, ...){
  # scale data to [0, 1]
  x01 <- scaleTo01(x)
  if(!is.null(markers) && all(markers > 0) && all(markers <= nrow(x))){
    # scan(s) with fiducial markers
    i <- which(fid(x) != "")
    # set the first 5 pixels equal to 1 (= white) at trace i
    x01[markers, i] <- 1L
  }
  #--- EXPORT PNG
  # the function scaleTo01() convert the GPR object to a matrix with values
  # ranging between 0 and 1
  png::writePNG(x01, target = target, ...)
})