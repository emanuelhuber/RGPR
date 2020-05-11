
  
#' Print GPRsurvey
#' @param x [\code{GPRsurvey object}] 
#' @param ... Not used. 
#' @export
print.GPRsurvey <- function(x, ...){
  cat("*** Class GPRsurvey ***\n")
  n <- length(x@paths)
  dirNames <- dirname(x@paths)
  if(length(unique(dirNames)) == 1){
    cat("Temp. directory: ", dirNames[1], "\n")
  }else{
    cat("Directories: ", dirNames,"\n")
  }
  testCoords <- sapply(x@coords, function(x) length(x) > 0)
  testCoords <- as.integer(testCoords) + 1
  testInt    <- sapply(x@intersections, 
                       function(x) ifelse(!is.null(x), nrow(x), 0))
  
  cat("- - - - - - - - - - - - - - -\n")
  is_test <- c("-","YES")
  overview <- data.frame(
    "name"      = x@names,
    "date"      = x@dates,
    "dim"       = paste0(x@nz, " x ", x@nx),
    "length"    = formatC(signif(x@xlengths, digits = 4), 
                          digits = 2, format = "fg", 
                          flag = "#"),
    "units"     = ifelse(x@spunit == "", "-", x@spunit),
    "freq"      = x@freqs,
    "coord"     = is_test[testCoords],
    "inters."   = testInt
  )
  print(overview)
  if(length(x@coords)>0 ){
    cat("- - - - - - - - - - - - - - -\n")
    if(length(x@crs) > 0 ){
      if(length(unique(x@crs)) == 1){
        cat("Coordinate system: ", x@crs, "\n")
      }else{
        cat("Coordinate systems: ", paste0(x@crs, collapse = " "), "\n")
      }
    }else{
      cat("Coordinate system: undefined", "\n")
    }
    
  }
  cat("****************\n")
}

#' Show some information on the GPR object
#'
#' Identical to print().
#' @param object [\code{GPRsurvey object}] 
#' @export
setMethod("show", "GPRsurvey", function(object){print.GPRsurvey(object)}) 
