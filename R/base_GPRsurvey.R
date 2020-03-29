#' Return length of a GPRsurvey object (number of GPR lines)
#' @param x [\code{GPRsurvey}]
#' @aliases length,GPRsurvey-method
#' @export
setMethod("length", "GPRsurvey", function(x) length(x@names))