#' Return length of a GPRsurvey object (number of GPR lines)
#' @param x (`GPRsurvey`)
#' @aliases length,GPRsurvey-method
#' @export
setMethod("length", "GPRsurvey", function(x) length(x@names))
