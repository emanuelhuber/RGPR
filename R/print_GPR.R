.GPR.print   <-  function(x){
  toprint <- character(13 + length(x@proc))
  toprint[1] <- paste0(class(x)[1], " object with ", ncol(x@data), 
                       " traces and ", nrow(x@data), " samples per traces")
  if(inherits(x, "GPRset")){
    toprint[2] <- paste0(length(x@y), " sets:         ", x@ylab)
  }
  
  toprint[3] <- paste0("name:           ", x@name)
  if(x@desc != "") toprint[4] <- paste0("description:    ", x@desc)
  if(length(x@date) > 0) toprint[5] <- paste0("survey date:    ", x@date)
  x_mode <- x@mode
  if(x_mode == "CO") x_mode <- "common-offset"
  toprint[6] <- paste0("survey mode:    ", x@mode)
  toprint[7] <- paste0("line length:    ", diff(range(x@x)), " ", x@xunit)
  toprint[8] <- paste0("window length:  ", diff(range(x@z)), " ", x@zunit)
  if(length(x@freq) == 1){
    x_freq <- ifelse(is.na(x@freq), "-", paste0(x@freq, " MHz"))
    toprint[9] <- paste0("frequency:      ", x_freq)
  }else{
    toprint[9] <- paste0("frequencies:    ", paste0(x@freq, collapse = ", "), " MHz")
  }
  nbfid <- sum(trimStr(x@markers)!= "")
  toprint[10] <- paste0("markers:        ", nbfid)
  if(length(x@coord) > 0){
    toprint[11] <- paste0("coordinates:    YES")
  }else{
    toprint[11] <- paste0("coordinates:    -")
  } 
  if(length(x@md[["clipData"]]) > 0){
    toprint[12] <- paste0("clipDataped values: YES (", sum(unlist(x@md[["clipData"]])), ")")
  }else{
    toprint[12] <- paste0("clipDataped values: -")
    
  }
  if(is.na(x@crs)){
    x_crs <- "-"
  }else{
    if(!is.null(a <- sf::st_crs(x@crs)$epsg)){
      x_crs <- paste0(sf::st_crs(x@crs)$Name, " (EPSG:", a, ")")
    }else{
      x_crs <- x@crs
    }
  }
  # x_crs <- ifelse(is.na(x@crs), "-", x@crs)
  toprint[13] <- paste0("CRS:            ", x_crs)

  i <- 0
  if(length(x@proc)>0){
    toprint[14] <- "> PROCESSING"
    for(i in seq_along(x@proc)){
      toprint[14 + i] <- paste0("  ", i, ". ", x@proc[i])
    }      
  }
  # toprint[9 + i + 1] <- "***********************"
  return(toprint[toprint != ""])      
}   



#' Print GPR
#' @param x (`GPR`) 
#' @param ... Not used. 
#' @export
print.GPR <- function(x, ...){
  jj <- .GPR.print(x, ...)
  cat(jj, sep = "\n")
  return(invisible(jj))
}


#' Show some information on the GPR object
#'
#' Identical to print().
#' @param object (`GPR`) 
#' @export
setMethod("show", "GPR", function(object){
  print.GPR(object)
}) 
