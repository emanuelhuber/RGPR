

# FIXME: type = "xta" not yet implemented 


#' Write the GPR object in a file.
#'
#' @param obj Object of the class `GPR` or `GPRsurvey`
#' @param dsn Filepath (Length-one character vector). If `dsn = NULL`,
#'              the file will be save in the current working directory with
#'              the name of obj (`name(obj)`) with the extension depending 
#'              of `format`.
#' @param format Format type. See Details.
#' @param overwrite Boolean. If `TRUE` existing files will be overwritten,
#'                  if `FALSE` an error will be thrown if the file(s) 
#'                  already exist(s).
#' @param ... additional parameters to be passed to [write.table()]
#'            when `format = "ASCII"` or `format = "xyza"`.
#' @seealso [readGPR()]
#' @name writeGPR
setGeneric("writeGPR", function(obj, dsn = NULL, 
                                format = c("rds", "dt1", "ascii", "xta", "xyza", "vtk"),
                                overwrite = FALSE, ...){ standardGeneric("writeGPR")})



#' @rdname writeGPR
#' @export
setMethod("writeGPR", "GPR", function(obj, dsn = NULL, 
                                      format = c("rds", "dt1", "ascii", "xta", "xyza", "vtk"),
                                      overwrite = FALSE, ...){
  format <- match.arg(tolower(format), c("rds", "dt1", "ascii", "xta", "xyza", "vtk"))
  dsn <- ifelse(is.null(dsn), obj@name, 
                  file.path(dirname(dsn), .fNameWExt(dsn)))
  ext <- switch(format,
                "dt1" = ".dt1",
                "rds" = ".rds",
                "ascii" = ".txt",
                "xta" = ".txt",
                "xyza" = ".txt",
                "vtk" = ".vtk")
  dsn <- paste0(dsn, ext)
  testFile <- file.exists(dsn)
  if(isTRUE(overwrite)){
    if(testFile) message("File overwritten\n")
  }else if(testFile){
    stop("File already exists. Cannot overwrite!\n")
  }
  obj@path <- dsn
  obj@data[is.na(obj@data) | is.infinite(obj@data)] <- 0
  switch(format,
         "dt1" = {.writeDT1(obj, dsn)},
         "rds" = {namesSlot <- slotNames(obj)
                   xList <- list()
                   # xList[["version"]] <- "0.1"
                   for(i in seq_along(namesSlot)){
                     xList[[namesSlot[i]]] <- slot(obj, namesSlot[i])
                   }
                   saveRDS(xList, dsn)},
         # idea: add header data
         "ascii" = {write.table(as.matrix(obj), file = dsn, 
                                quote = FALSE, col.names = obj@x, 
                                row.names = obj@z,
                                ...)},
         "xyza" = {if(length(obj@coord) == 0){
                   stop("This data has no coordinates!")
                   }
                   xyzv <- matrix(nrow=prod(dim(obj)), ncol = 4)
                   colnames(xyzv) <- c("x", "y", "z", "a")
                   xyzv[, 4]  <- as.vector(as.matrix(obj))
                   xyzv[,1:3] <-  kronecker(obj@coord, matrix(1,nrow(obj),1))
                   xyzv[,3]   <- rep(max(xyzv[,3]), ncol(obj)) - 
                     rep(obj@z, times = ncol(obj))
                   write.table(xyzv, file = dsn, quote = FALSE, 
                               col.names = TRUE, row.names = FALSE, ...)},
         "vtk" = {if(length(obj@coord) == 0){
                    stop("This data has no coordinates!")
                  }
                  writeVTK(obj, dsn = dsn)
         }
  )
  # invisible(return(obj))
})



#' @rdname writeGPR
#' @export
setMethod("writeGPR", "GPRsurvey", 
  function(obj, dsn = NULL, 
           format = c("DT1", "rds", "ASCII", "xta", "xyzv", "vtk"),
           overwrite = FALSE, ...){
    #setMethod("writeGPR", "GPRsurvey", 
    #    function(obj,dsn, format=c("DT1","rds"), overwrite=FALSE){
    format <- match.arg(tolower(format), c("dt1", "rds", "ascii", "xta", "xyza", "vtk"))
    if(format == "vtk"){
      writeVTK(obj, dsn)
    }else{
      mainDir <- dirname(dsn)
      if(mainDir =="." || mainDir =="/" ){
        mainDir <- ""
      }
      subDir <- basename(dsn)
      if ( !dir.exists( file.path(mainDir, subDir) )) {
        warning("Create new director ", subDir, " in ", mainDir, "\n")
        dir.create(file.path(mainDir, subDir))
      }
      
      for(i in seq_along(obj)){
        z <- obj[[i]]
        dsn <- file.path(mainDir, subDir, z@name)
        obj@paths[[i]] <- paste0(dsn, ".", tolower(format))
        writeGPR(z, dsn = dsn, format = format , overwrite = overwrite)
        message("Saved: ", obj@paths[[i]] )
        # gpr <- verboseF( obj[[i]] , verbose = FALSE)
        # #if(length(obj@coords[[i]]) > 0){
        # gpr@coord <- obj@coords[[i]]
        # #}
        # if(length(obj@intersections[[i]])>0){
        #   #ann(gpr) <- obj@intersections[[gpr@name]][,3:4]
        #   ann(gpr) <- cbind(obj@intersections[[i]]$trace,
        #                     obj@intersections[[i]]$name)
        # }
        # dsn <- file.path(mainDir, subDir, gpr@name)
        # obj@paths[[i]] <- paste0(dsn, ".", tolower(format))
        # writeGPR(gpr, dsn = dsn, format = format , overwrite = overwrite, ...)
        # message("Saved: ", dsn )
      } 
      # invisible(obj)
    }
  }
)



