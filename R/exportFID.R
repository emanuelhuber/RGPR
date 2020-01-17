#' @name exportFid
#' @rdname exportFid
#' @export
setGenericVerif("exportFid", function(x, fPath = NULL, sep = " ") 
  standardGeneric("exportFid"))


#' Export fiducial markers
#' 
#' @name exportFid
#' @rdname exportFid
#' @export
setMethod("exportFid", "GPR", function(x, fPath = NULL, sep = " "){
  # Trace  Position  Comment  PNAME
  if(length(x@fid) > 0){
    tr_start <- 1
    tr_end <- length(x)
    tr <- which(fid(x) != "" & fid(x) != "skip")
    trfid <- fid(x)[tr]
    if(!(tr_start %in% tr)){  
      tr <- c(tr_start,tr)
      trfid <- c("START",trfid)
    }
    if(!(tr_end %in% tr)){
      tr <- c(tr,tr_end)
      #         lastF <-regmatches(trfid[length(trfid)], 
      #                         regexpr(pattern="[[:digit:]]+", 
      #                         trfid[length(trfid)]))
      #         if(length(lastF)>0){
      #           trfid <- c(trfid, paste0("F", as.numeric(lastF)+1))
      #         }else{
      trfid <- c(trfid,"END")
      #         }
    }
    trpos <- x@pos[tr]
    FID <- data.frame("TRACE" = tr,"POSITION" = trpos, "COMMENT" = trfid)
    if(length(x@coord) > 0 && ncol(x@coord) == 3){
      FID$E <- x@coord[tr,1]
      FID$N <- x@coord[tr,2]
      FID$Z <- x@coord[tr,3]
    }
    if(is.null(fPath)){
      return(FID)
    }else{
      write.table(x = FID, file = fPath, sep = sep, row.names = FALSE, 
                  col.names = TRUE, quote = FALSE)
    }
  }else{
    if(length(x@name)>0){
      message("No fiducials for ", x@name, "\n")
    }else{
      message("No fiducials\n")
    }
    return(NULL)
  }
} 
)


#' @export
setMethod("exportFid", "GPRsurvey", function(x, fPath = NULL, sep = " "){
  for(i in seq_along(x)){
    gpr <- readGPR(x@filepaths[[i]])
    file_name <- file.path(fPath, paste0(gpr@name, ".txt"))
    exportFid(x = gpr, fPath = file_name, sep = sep)
    message('File "', file_name, '" created!')
    # x@coords[[gpr@name]] <- gpr@coord
  }
}
)