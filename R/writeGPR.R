

# FIXME: type = "xta" not yet implemented 


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
#' @param ... additional parameters to be passed to \code{\link{write.table}}
#'            when \code{type = "ASCII"} or \code{type = "xyza"}.
#' @seealso \code{\link{readGPR}}
#' @name writeGPR
setGeneric("writeGPR", function(x, fPath = NULL, 
                                type = c("DT1", "rds", "ASCII", "xta", "xyza"),
                                overwrite = FALSE, ...){ standardGeneric("writeGPR")})



#' @rdname writeGPR
#' @export
setMethod("writeGPR", "GPR", function(x, fPath = NULL, 
                                      type = c("DT1", "rds", "ASCII", "xta", "xyza"),
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
  x@path <- fPath
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
  # invisible(return(x))
})



#' @rdname writeGPR
#' @export
setMethod("writeGPR", "GPRsurvey", 
  function(x, fPath = NULL, 
           type = c("DT1", "rds", "ASCII", "xta", "xyzv"),
           overwrite = FALSE, ...){
    #setMethod("writeGPR", "GPRsurvey", 
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
      z <- x[[i]]
      fPath <- file.path(mainDir, subDir, z@name)
      x@paths[[i]] <- paste0(fPath, ".", tolower(type))
      writeGPR(z, fPath = fPath, type = type , overwrite = overwrite)
      message("Saved: ", x@paths[[i]] )
      # gpr <- verboseF( x[[i]] , verbose = FALSE)
      # #if(length(x@coords[[i]]) > 0){
      # gpr@coord <- x@coords[[i]]
      # #}
      # if(length(x@intersections[[i]])>0){
      #   #ann(gpr) <- x@intersections[[gpr@name]][,3:4]
      #   ann(gpr) <- cbind(x@intersections[[i]]$trace,
      #                     x@intersections[[i]]$name)
      # }
      # fPath <- file.path(mainDir, subDir, gpr@name)
      # x@paths[[i]] <- paste0(fPath, ".", tolower(type))
      # writeGPR(gpr, fPath = fPath, type = type , overwrite = overwrite, ...)
      # message("Saved: ", fPath )
    } 
    # invisible(x)
  }
)




# -------------------------------------------
# ------------writeDT1--------------------------
# -------------------------------------------
# @name  writeDT1 
# @description This function writes *.HD and associated *.DT1
# files  (Sensors & Software)

# @date 07.11.2012 08:33
# @auteur Emanuel Huber
# @param [text]    fileNameHD       (file path of *.hd file)
# @param [text]    fileNameDT1       (file path of *.dt1 file)


# @return list((hd = headerHD, dt1hd = headerDT1, data=myData))
# -------------------------------------------
.writeDT1 <- function(x, fPath){
  #-------------------------
  # DT1 FILE: traces
  traceData <- x@data  # should ranges between -32768 and 32767
  if(max(traceData)/max(abs(traceData))*32768 <= 32767){
    traceData <- round(traceData/max(abs(traceData))*32768)
  }else{
    traceData <- round(traceData/max(abs(traceData))*32767)
  }
  #   cat(range(traceData),"\n")
  if(min(traceData)< -32768 || max(traceData) > 32768){
    stop("problem real > integer conversion")
  }
  #   A
  #   traceData <- A$data
  storage.mode(traceData) <- "integer"
  
  # DT1 FILE: header
  indexDT1Header=c("traces", "position", "samples","topo", "NA0", "bytes","dz", 
                   "stacks","NA1","NA2", "NA3", "NA4", "NA5", "NA6", "recx","recy",
                   "recz","transx","transy","transz","time0","zeroflag", "NA7", "time",
                   "x8", "com","com1","com2","com3","com4","com5","com6")
  traces_hd <- list()
  traces_hd$traces <- x@traces
  traces_hd$position <- x@pos
  traces_hd$samples <- rep(nrow(x@data),ncol(x@data))
  if(length(x@coord) > 0 && sum(is.na(x@coord)) > 0){
    traces_hd$topo <- x@coord[,3]
  }else{
    traces_hd$topo <- rep.int(0L,ncol(x@data))
  }
  traces_hd$NA0 <- rep.int(0L,ncol(x@data))
  #   traces_hd$NA0 <- A$dt1hd$NA1
  traces_hd$bytes <- rep.int(2L,ncol(x@data))
  traces_hd$dz <- rep(x@dz*1000,ncol(x@data)) # time window 
  traces_hd$stacks <- rep(as.numeric(x@hd$NUMBER_OF_STACKS),ncol(x@data))
  traces_hd$NA1 <- rep.int(0L,ncol(x@data))
  traces_hd$NA2 <- traces_hd$NA1 
  traces_hd$NA3 <- traces_hd$NA1 
  traces_hd$NA4 <- traces_hd$NA1 
  traces_hd$NA5 <- traces_hd$NA1 
  traces_hd$NA6 <- traces_hd$NA1 
  if(length(x@rec) > 0 && sum(is.na(x@rec)) > 0){
    traces_hd$recx <- x@rec[,1]
    traces_hd$recy <- x@rec[,2]
    traces_hd$recz <- x@rec[,3]
  }else{
    traces_hd$recx <- rep.int(0L,ncol(x@data))
    traces_hd$recy <- rep.int(0L,ncol(x@data))
    traces_hd$recz <- rep.int(0L,ncol(x@data))
  }
  if(length(x@trans) > 0 && sum(is.na(x@trans)) > 0){
    traces_hd$transx <- x@trans[,1]
    traces_hd$transy <- x@trans[,2]
    traces_hd$transz <- x@trans[,3]
  }else{
    traces_hd$transx <- rep.int(0L,ncol(x@data))
    traces_hd$transy <- rep.int(0L,ncol(x@data))
    traces_hd$transz <- rep.int(0L,ncol(x@data))
  }
  # traces_hd$time0 <- x@time0 
  traces_hd$time0 <- 1L + round(x@time0/x@dz,2)
  traces_hd$time0 <- traces_hd$NA1 
  traces_hd$zeroflag <- rep.int(0L,ncol(x@data)) 
  traces_hd$NA7 <- traces_hd$NA1 
  aa <-as.POSIXct(x@time[1], origin = "1970-01-01")
  bb <- format(aa, format = "%Y-%m-%d")
  myDay <- as.double(as.POSIXct(as.Date(bb), origin="1970-01-01"))
  traces_hd$time <- x@time - myDay
  traces_hd$x8 <- rep.int(0L, ncol(x@data)) 
  if(length(x@fid) == 0){
    traces_hd$com <- rep("", ncol(x@data))
  }else{
    traces_hd$x8[trimStr(x@fid) != ""] <- 1L
    traces_hd$com <- x@fid 
  }
  
  # FILE NAMES
  dirName   <- dirname(fPath)
  splitBaseName <- unlist(strsplit(basename(fPath), '[.]'), use.names = FALSE)
  baseName   <- paste0(splitBaseName[1:(length(splitBaseName)-1)])
  if(dirName == '.'){
    fPath <- baseName
  }else{
    fPath <- paste0(dirName, '/', baseName)
  }
  # if(isTRUE(overwrite)){
  # cat("file may be overwritten\n")
  # }else{
  # fPath_orgi <- fPath
  # k <- 0
  # while(file.exists(paste(fPath,".DT1",sep="")) || 
  # file.exists(paste(fPath,".HD",sep=""))){
  # fPath <- paste(fPath_orgi,"_",k,sep="")
  # k <- k+1
  # }
  # }
  
  
  # WRITE DT1 FILE
  dt1_file <- file(paste0(fPath, ".DT1") , "wb")
  for(i in 1:ncol(x@data)){
    for(j in 1:25){
      realData4 <- traces_hd[[j]][i]
      #       realData4 <- A$dt1hd[[j]][i]
      storage.mode(realData4) <- "double"
      writeBin(realData4, dt1_file, size = 4, endian = "little")
    }
    comment28 <- as.character(traces_hd$com[i])
    # nnchar <- 28-nchar(comment28)
    com_add <- paste(c(rep(" ", 28-nchar(comment28)), comment28), 
                     sep = "", collapse = "")
    writeChar(com_add, dt1_file,nchars =28,eos = NULL)
    # suppressWarnings( writeChar(comment28, dt1_file,nchars = 28,eos = NULL))
    # for(k in 1:nnchar){
    # writeChar("^@", dt1_file)
    # }
    # two-byte integers
    writeBin(traceData[,i], dt1_file, size = 2, endian = "little")
  }
  close(dt1_file)
  
  # j<-0
  
  # j<-j+1
  # traces_hd[[indexDT1Header[j]]][i]
  
  #-------------------------
  # HD FILE: traces
  hd_file <- file(paste0(fPath, ".HD") , "w+")
  writeLines("1234", con = hd_file, sep = "\r\n")
  if(!is.null(x@hd$gprdevice)){
    writeLines(as.character(x@hd$gprdevice), con = hd_file, sep = "\r\n")
  }else{
    writeLines("Data from RGPR", con = hd_file, sep = "\r\n")
  }
  writeLines(as.character(x@date), con = hd_file, sep = "\r\n")
  writeLines(paste0("NUMBER OF TRACES"," = ", as.character(ncol(x@data))), 
             con = hd_file, sep = "\r\n")
  writeLines(paste0("NUMBER OF PTS/TRC"," = ", as.character(nrow(x@data))), 
             con = hd_file, sep = "\r\n")
  writeLines(paste0("TIMEZERO AT POINT", " = ",
                    as.character(1+round(mean(x@time0)/x@dz,2))), 
             con = hd_file, sep = "\r\n")
  writeLines(paste0("TOTAL TIME WINDOW", " = ", 
                    as.character(x@dz*(nrow(x@data)))), 
             con = hd_file, sep = "\r\n")
  startpos <- 0
  if(!is.null(x@hd$startpos)){
    startpos <- x@hd$startpos
  }
  writeLines(paste0("STARTING POSITION", " = ", as.character(startpos)), 
             con = hd_file, sep = "\r\n")
  endpos <- (ncol(x@data)-1)*x@dx
  if(!is.null(x@hd$endpos)){
    endpos <- x@hd$endpos
  }
  writeLines(paste0("FINAL POSITION"," = ", as.character(endpos)), 
             con = hd_file, sep = "\r\n")
  writeLines(paste0("STEP SIZE USED"," = ",as.character(x@dx)),
             con = hd_file, sep = "\r\n")
  writeLines(paste0("POSITION UNITS", " = ", "m"), 
             con = hd_file, sep = "\r\n")
  if(x@posunit != "m"){
    warning('Position units were defined as "metres"!\n')
  }
  x_freq <- ifelse(is.na(x@freq), 0, x@freq)
  writeLines(paste0("NOMINAL FREQUENCY"," = ", as.character(x_freq)), 
             con = hd_file, sep = "\r\n")
  x_antsep <- ifelse(is.na(x@antsep), 0, x@antsep)
  writeLines(paste0("ANTENNA SEPARATION"," = ", as.character(x_antsep)), 
             con = hd_file, sep = "\r\n")
  pulservoltage <- 0
  if(!is.null(x@hd$PULSER_VOLTAGE_V)){
    pulservoltage <- x@hd$PULSER_VOLTAGE_V
  }
  writeLines(paste0("PULSER VOLTAGE (V)", "=", as.character(pulservoltage)), 
             con = hd_file, sep = "\r\n")
  nstacks <- 1
  if(!is.null(x@hd$NUMBER_OF_STACKS)){
    nstacks <- x@hd$NUMBER_OF_STACKS
  }
  writeLines(paste0("NUMBER OF STACKS", " = ", as.character(nstacks)), 
             con = hd_file, sep = "\r\n")
  writeLines(paste0("SURVEY MODE", " = ", as.character(x@mode)), 
             con = hd_file, sep = "\r\n")
  
  if(length(x@hd) > 0){
    hdNames <- names(x@hd)
    hdNames <- hdNames[!(hdNames %in% c("startpos", "endpos",
                                        "NUMBER_OF_STACKS",
                                        "PULSER_VOLTAGE_V", "gprdevice"))]
    for(i in seq_along(hdNames)){
      hdName <- gsub("_", replacement = " ", as.character(hdNames[i]))
      hdName <- gsub("Serial", replacement = "Serial#", hdName)
      hdName <- gsub("CAL tm", replacement = "CAL (t/m)", hdName)
      
      writeLines(paste0(as.character(hdName), " = ", 
                        as.character(x@hd[[hdNames[i]]])), 
                 con = hd_file, sep = "\r\n")
    }
  }
  close(hd_file)
  return(fPath)
}