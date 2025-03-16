
#' Print the list of delineation of the GPR data
#' 
#' Print and invisible returns the GPR data delineations.
#' @param obj (`GPR* object`) An object of the class `GPR`
#' @param name (`character[n]`) Names of the delineations.
#' @name velLayers
#' @rdname velLayers
#' @export
setGeneric("velLayers", function(obj, name = NULL) 
  standardGeneric("velLayers"))


#' @rdname velLayers
#' @export
setMethod("velLayers", "GPR", function(obj, name = NULL){
  
  obj@delineations <- obj@md[["velocity_interfaces"]] 
  delineations(obj, name = name)
  
  # if(length(obj@delineations) > 0){
  #   message("*** delineated lines ****")
  #   xyzrel <- .getXYZrel(obj)
  #   if(!is.null(name)){
  #     xyzrel <- xyzrel[names(xyzrel) %in% name]
  #   }
  #   m <- unlist( lapply(xyzrel, .printdelineations) )
  #   m <- Map(c, paste0(names(m), "\n"), paste0(seq_along(m), m))
  #   message(unlist(m), appendLF = FALSE)
  #   message("- - - - - - - - - - -")
  #   invisible(xyzrel)
  # }else{
  #   message("There is no delineations. Use 'delineate()' ",
  #           "to delineate reflectors/structures on your data.")
  # }
})