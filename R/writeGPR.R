#' @name writeGPR
#' @rdname writeGPR
#' @export
setGeneric("writeGPR", function(x, fPath = NULL, 
                                type = c("rds", "DT1", "ASCII", "xta", "xyza"),
                                overwrite = FALSE, ...){ standardGeneric("writeGPR")})


#----------------------- SAVE/EXPORT ------------------------#
#' Write the GPR object in a file.
#'
#' @param x Object of the class \code{GPR} or \code{GPRsurvey}
#' @param fPath Filepath (Length-one character vector). If \code{fPath = NULL},
#'              the file will be save in the current working directory with
#'              the name of x (\code{name(x)}) with the extension depending 
#'              of \code{type}.
#' @param type Format type. See Details.
#' @param overwrite Boolean. If \code{TRUE} existing files will be overwritten,
#'                  if \code{FALSE} an error will be thrown if the file(s) 
#'                  already exist(s).
#' @name writeGPR
#' @rdname writeGPR
#' @seealso \code{\link{readGPR}}
#' @export
setMethod("writeGPR", "GPR", function(x, fPath = NULL, 
                                      type = c("rds", "DT1", "ASCII", "xta", "xyza"),
                                      overwrite = FALSE, ...){
  type <- match.arg(tolower(type), c("dt1", "rds", "ascii", "xta", "xyza"))
  fPath <- ifelse(is.null(fPath), x@name, 
                  file.path(dirname(fPath), .fNameWExt(fPath)))
  ext <- switch(type,
                "dt1" = ".dt1",
                "rds" = ".rds",
                "ascii" = ".txt",
                "xta" = ".txt",
                "xyza" = ".txt")
  fPath <- paste0(fPath, ext)
  testFile <- file.exists(fPath)
  if(isTRUE(overwrite)){
    if(testFile) message("File overwritten\n")
  }else if(testFile){
    stop("File already exists. Cannot overwrite!\n")
  }
  x@filepath <- fPath
  x@data[is.na(x@data) | is.infinite(x@data)] <- 0
  switch(type,
         "dt1" = {.writeDT1(x, fPath)},
         "rds" = {namesSlot <- slotNames(x)
         xList <- list()
         # xList[["version"]] <- "0.1"
         for(i in seq_along(namesSlot)){
           xList[[namesSlot[i]]] <- slot(x, namesSlot[i])
         }
         saveRDS(xList, fPath)},
         # idea: add header data
         "ascii" = {write.table(as.matrix(x), file = fPath, 
                                quote = FALSE, col.names = x@pos, 
                                row.names = x@depth,
                                ...)},
         "xyza" = {if(length(x@coord) == 0){
           stop("This data has no coordinates!")
         }
           xyzv <- matrix(nrow=prod(dim(x)), ncol = 4)
           colnames(xyzv) <- c("x", "y", "z", "a")
           xyzv[, 4]  <- as.vector(as.matrix(x))
           xyzv[,1:3] <-  kronecker(x@coord, matrix(1,nrow(x),1))
           xyzv[,3]   <- rep(max(xyzv[,3]), ncol(x)) - 
             rep(x@depth, times = ncol(x))
           write.table(xyzv, file = fPath, quote = FALSE, 
                       col.names = TRUE, row.names = FALSE, ...)}
  )
  invisible(return(x))
} 
)



#' @export
setMethod("writeGPR", "GPRsurvey", 
          function(x, fPath = NULL, 
                   type = c("DT1", "rds", "ASCII", "xta", "xyzv"),
                   overwrite = FALSE, ...){
            #setMethod("writeGPR", "GPRsurvey", 
            lst <- list(...)
            if(!is.null(lst$format)){
              if(tolower(lst$format) %in% c("dt1", "rds", "ascii", "xta", "xyza")){
                type <- lst$format
              }else{
                stop("use 'type' in instead of format.\n",
                     "'type' must be one of the following: 'DT1', 'rds', 'ASCII', 'xta', 'xyzv'. ")
              }
            }
            #    function(x,fPath, format=c("DT1","rds"), overwrite=FALSE){
            type <- match.arg(tolower(type), c("dt1", "rds", "ascii", "xta", "xyza"))
            mainDir <- dirname(fPath)
            if(mainDir =="." || mainDir =="/" ){
              mainDir <- ""
            }
            subDir <- basename(fPath)
            if ( !dir.exists( file.path(mainDir, subDir) )) {
              warning("Create new director ", subDir, " in ", mainDir, "\n")
              dir.create(file.path(mainDir, subDir))
            }
            for(i in seq_along(x)){
              gpr <- verboseF( x[[i]] , verbose = FALSE)
              if(length(x@coords[[gpr@name]])>0){
                gpr@coord <- x@coords[[gpr@name]]
              }
              if(length(x@intersections[[gpr@name]])>0){
                #ann(gpr) <- x@intersections[[gpr@name]][,3:4]
                ann(gpr) <- cbind(x@intersections[[gpr@name]]$trace,
                                  x@intersections[[gpr@name]]$name)
              }
              fPath <- file.path(mainDir, subDir, gpr@name)
              x@filepaths[[i]] <- paste0(fPath, ".", tolower(type))
              writeGPR(gpr, fPath = fPath, type = type , overwrite = overwrite, ...)
              message("Saved: ", fPath )
            } 
            invisible(return(x))
          }
)