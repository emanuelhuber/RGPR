
#' Print the list of delineation of the GPR data
#' 
#' Print and invisible returns the GPR data delineations.
#' @param obj (`GPR* object`) An object of the class `GPR`
#' @param name (`character[n]`) Names of the delineations.
#' @name delineations
#' @rdname delineation
#' @export
setGeneric("delineations", function(obj, name = NULL) 
  standardGeneric("delineations"))


#' @rdname delineation
#' @export
setMethod("delineations", "GPR", function(obj, name = NULL){
  if(length(obj@delineations) > 0){
    message("*** delineated lines ****")
    xyzrel <- .getXYZrel(obj)
    if(!is.null(name)){
      xyzrel <- xyzrel[names(xyzrel) %in% name]
    }
    m <- unlist( lapply(xyzrel, .printdelineations) )
    m <- Map(c, paste0(names(m), "\n"), paste0(seq_along(m), m))
    message(unlist(m), appendLF = FALSE)
    message("- - - - - - - - - - -")
    invisible(xyzrel)
  }else{
    message("There is no delineations. Use 'delineate()' ",
            "to delineate reflectors/structures on your data.")
  }
})

# xyz0 <- xyz
.printdelineations <- function(xyz){
  m <- paste0( ". length = ", round(diff(range(xyz[, 4])), 2), 
               ";  mean depth = ", round(diff(range(xyz[, 5], na.rm = TRUE)), 2),
               ";  ", nrow(xyz), " pts\n")
  return(m)
}

