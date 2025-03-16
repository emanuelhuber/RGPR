
#' Export the coordinates of the delineations
#'
#' @param obj (`GPR* object`) An object of the class `GPR`
#' @param dsn (`character(1)|connection object`) data source name: 
#'            either the filepath to the GPR data (character),
#'            or an open file connection.
#' @name exportDelineations
#' @rdname delineation
#' @export
setGeneric("exportDelineations", function(obj, dsn = ".") 
  standardGeneric("exportDelineations"))


#' @rdname delineation
#' @export
setMethod("exportDelineations", "GPR", function(obj, dsn = "."){
  if(length(obj@delineations) > 0){
    xyzrel <- .getXYZrel(obj)
    for(i in seq_along(xyzrel)){
      table_path_name <- paste0(dsn, obj@name, "_del", i, "_", 
                                names(xyzrel[i]), ".txt")
      write.table(xyzrel[[i]], 
                  file = table_path_name, 
                  sep = ";", 
                  row.names = FALSE, 
                  col.names =  TRUE)
    }
  }else{
    message("There is no delineations. Use 'delineate()' ",
            "to delineate reflectors/structures on your data.")
  }
})

#' @export
setMethod("exportDelineations", "GPRsurvey", function(obj, dsn = "."){
  for(i in seq_along(obj)){
    exportDelineations(verboseF(obj[[i]], verbose = FALSE),  
                       dsn = dsn) 
  }
})
