#' Meta-data extracted from the GPR data
#' 
#' Meta-data extracted from the GPR data that depends on the manufacturer
#' file format. All the information that are not used to construct the RGPR
#' objects is still stored as meta-data and can be retrieved with \code{md()}.
#' 
#' The element names of the list are given below
#' \describe{
#'   \item{hd}{Here comes all the information from the raw GPR data file that
#'             is not used to construct a RGPR object.
#'             It can be retrieved with \code{metadata(x)$hd}.}
#'   \item{GPS}{When reading GPR data, the optional GPS data file is red if 
#'              available and red even if \code{interpGPS = FALSE} 
#'              (see \code{\link{readGPR}}). If \code{interpGPS = FALSE}, 
#'              the (formated) content of the GPS data file is then stored as 
#'              meta-data and can be retrieved with \code{metadata(x)$GPS}.}
#'   \item{clipData}{When reading GPR data, the clipDataped signal values are directly
#'               estimated from the bit values and stored as metadata.
#'               They can be retrieved with \code{metadata(x)$clipData}.
#'               These clipDataped values can be used directly
#  FIXME             by \code{link{clipDatapedValues}} and \code{\link{declipData}}
#'               .}
#' }
#' 
#' @param x [\code{GPR}] An object of the class GPR.
# @param value [\code{character}]
#' @return [\code{list}] 
#' @seealso \code{\link{readGPR}} 
#, FIXME \code{\link{clipDatapedValues}}, and \code{\link{declipData}}
#' @name metadata
#' @rdname metadata
setGeneric("metadata", function(x) standardGeneric("metadata"))

# @name md<-
# @rdname md
# setGeneric("md<-", function(x, value) standardGeneric("md<-"))

#' @rdname metadata
#' @export
setMethod("metadata", "GPR", function(x){
  return(x@md)
})

# @rdname md
# @export
# setReplaceMethod("md", "GPR", function(x, value){
#   ...
#   x@proc <- c(x@proc, "md<-")
#   return(x)
# })
