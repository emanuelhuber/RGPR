`#' @name writeGPR
#' @rdname writeGPR
#' @export
setGeneric("writeGPR", function(x, fPath = NULL, 
                                type = c("rds", "DT1", "SGY", "ASCII", "xta", "xyza"),
                                overwrite = FALSE, endian = .Platform$endian, ...){ standardGeneric("writeGPR")})


#----------------------- SAVE/EXPORT ------------------------#
#' Write GPR data
#' 
#' Write GPR data to a specified file format (see Details). Add automatically
#' the file extension if missing.
#'
#' Modified slots
#' \itemize{
#'   \item \code{rds}: Internal R format (useful if you aim to save your data
#'          for later processing in R).
#'   \item \code{DT1}: Sensors & Software file format
#'   \item \code{ASCII}: ".txt" format; write the GPR data as matrix like format with 
#'         column names equal to trace position and row name equal to depth/time
#'   \item \code{xta}: write the GPR data as a 3-column ".txt" file. The columns
#'         correspond to 1) trace position, 2) time/depth, and 3) amplitude.
#'   \item \code{xyza}: write the GPR data as a 4-column ".txt" file.
#'         The columns correspond to 1) trace x-position, 2) trace y-position, 
#'         3) time/depth, and 4) amplitude.
#' }
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
                                      type = c("rds", "DT1", "SGY", "ASCII", "xta", "xyza"),
                                      overwrite = FALSE, endian = .Platform$endian, ...){
  lst <- list(...)
  if(!is.null(lst$format)){
    if(tolower(lst$format) %in% c("dt1", 'sgy', "rds", "ascii", "xta", "xyza")){
      type <- lst$format
    }else{
      stop("use 'type' in instead of format.\n",
           "'type' must be one of the following: 'DT1', 'SGY', 'rds', 'ASCII', 'xta', 'xyzv'. ")
    }
  }
  type <- match.arg(tolower(type[1]), c("rds", "dt1", "sgy", "ascii", "xta", "xyza"))
  fPath <- ifelse(is.null(fPath), x@name, 
                  file.path(dirname(fPath), .fNameWExt(fPath)))
  ext <- switch(type,
                "dt1" = ".dt1",
                # "segy" = ".sgy",
                "sgy" = ".sgy",
                "rds" = ".rds",
                "ascii" = ".txt",
                "xta" = ".txt",
                "xyza" = ".txt")

   if(grepl("(\\.s(e)*gy)$", fPath, ignore.case = TRUE) || 
      grepl(paste0("(\\", ext, ")$"), fPath, ignore.case = TRUE)){
     ext <- ""
   }

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
         "dt1" = {.writeDT1(x, fPath, endian = endian)},
         "sgy" = {.writeSGY(x, fPath, endian = endian)},
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
                   xyzv[,1:3] <-  kronecker(x@coord, matrix(1, nrow(x) , 1))
                   xyzv[,3]   <- rep(max(xyzv[,3]), ncol(x)) - 
                     rep(x@depth, times = ncol(x))
                   write.table(xyzv, file = fPath, quote = FALSE, 
                               col.names = TRUE, row.names = FALSE, ...)},
         "xta" = {xta <- matrix(nrow=prod(dim(x)), ncol = 3)
                  colnames(xta) <- c("x", "t", "a")
                  xta[,1] <-  rep(pos(x), times = nrow(x))
                  xta[,2] <-  rep(depth(x), each = ncol(x))
                  xta[, 3]  <- as.vector(as.matrix(x))
                  write.table(xta, file = fPath, quote = FALSE, 
                              col.names = TRUE, row.names = FALSE, ...)
         
                 }
  )
  message(paste0("File saved: ", fPath))
  #invisible(return(x))
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
                type <- "rds"
                # stop("use 'type' in instead of format.\n",
                     # "'type' must be one of the following: 'DT1', 'rds', 'ASCII', 'xta', 'xyzv'. ")
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
)`