#' @name exportProc
#' @rdname exportProc
#' @export
setGenericVerif("exportProc",  function(x,fPath=NULL,sep="\t", row.names=FALSE,
                                        col.names=FALSE, ...) standardGeneric("exportProc"))


#' Export the process steps.
#'
#' @name exportProc
#' @rdname exportProc
#' @export
setMethod("exportProc", "GPR", function(x,fPath=NULL,sep="\t", row.names=FALSE,
                                        col.names=FALSE, ...){
  write.table(x@proc, file = fPath, row.names = row.names,
              col.names = col.names,...)
})

