
  
#' Print GPRsurvey
#' @param x [\code{GPRsurvey object}] 
#' @param ... Not used. 
#' @export
print.GPRsurvey <- function(x, ...){
  cat("*** Class GPRsurvey ***\n")
  # n <- length(x)
  n <- length(x@paths)
  dirNames <- dirname(x@paths)
  if(length(unique(dirNames)) == 1){
    cat("Temp. directory: ", dirNames[1], "\n")
  }else{
    cat("Directories: ", dirNames,"\n")
    # cat("One directory among others:", dirNames[1],"\n")
  }
  testCoords <- sapply(x@coords, function(x) length(x) > 0)
  testCoords <- as.integer(testCoords) + 1
  # testCoords <- rep(0, n)
  # names(testCoords) <- x@names
  # if(length(x@coords) > 0){
  #   testLength <- sapply(x@coords, length)
  #   testCoords[names(testLength)] <- testLength
  # }
  # testCoords <- as.numeric(testCoords > 0) + 1
  testInt <- sapply(x@intersections, function(x) if(!is.null(x)){nrow(x)}else{0})
  # testInt <- as.integer(testInt) + 1
  
  # testIntersecs <- rep(0,n)
  # names(testIntersecs) <- x@names
  # if(length(x@intersections) > 0){
  #   testLength <- sapply(x@intersections,length)
  #   testIntersecs[names(testLength)] <- testLength
  # }
  # testIntersecs <- as.numeric(testIntersecs > 0)+1
  
  cat("- - - - - - - - - - - - - - -\n")
  is_test <- c("-","YES")
  #overview <- data.frame("name" = .fNameWExt(x@filepaths),
  overview <- data.frame(
    "name"      = x@names,
    "date"      = x@dates,
    #"name"     = x@names,
    # "length"    = round(x@lengths,2),
    "dim"       = paste0(x@nz, " x ", x@nx),
    "length"    = formatC(signif(x@xlengths, digits = 4), 
                          digits = 2, format = "fg", 
                          flag = "#"),
    "units"     = ifelse(x@spunit == "", "-", x@spunit),
    "freq"      = x@freqs,
    "coord"     = is_test[testCoords],
    # "epsg"      = sf::st_crs(x@crs)$epsg,
    "inters."   = testInt
    #"int"      = is_test[testIntersecs],
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
