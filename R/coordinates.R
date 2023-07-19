

#' Trace coordinates
#' 
#' Return or update the trace coordinates (x, y, z). Not that you cannot 
#' change the number of coordinates with \code{coord}.
#' 
#' Modified slots class GPR
#' \itemize{
#'   \item \code{coord} the trace coordinates
#'   \item \code{x} the local trace position (along profile)
#' }
#' Modified slots class GPRsurvey
#' \itemize{
#'   \item \code{coords} the trace coordinates
#'   \item \code{xlengths} the local trace position (along profile)
#'   \item \code{intersections} the local trace position (along profile)
#' }
#' @param x      [\code{GPR class}] An object of the class \code{GPR}
#' @param value  [\code{matrix(n,3)|list}] coordinates (x, y, z)
#' @return [\code{GPR class}] An object of the class \code{GPR}
#' @name coordinates
#' @concept getters/setters
setGeneric("coordinates", function(x) 
  standardGeneric("coordinates"))


#' @rdname coordinates
#' @aliases coordinates<-,GPR-method
setGeneric("coordinates<-",function(x,value){standardGeneric("coordinates<-")})

 
#' @rdname coordinates   
#' @export
setMethod("coordinates", "GPR", function(x){
  return(x@coord)
})

#' @rdname coordinates
#' @export
setReplaceMethod("coordinates", signature="GPR", function(x, value){
    value <- as.matrix(value)
    
    #---- check some stuff
    msg <- c()
    if(ncol(value) != 3){
      msg <- c(msg, paste0("'value' must have 3 columns."))
    }
    if(nrow(value) != ncol(x)){
      msg <- c(msg, paste0("'value' must have same row number as x."))
    }
    if(length(msg) > 0){
      stop(paste0(paste0(seq_along(msg), ". "), msg, sep = "\n"))
    } 
    #------------------------ -
    
    x@coord  <- value
    
    x <- dropDuplicatedCoords(x, verbose = FALSE)
    x <- .updateXpos(x)
    
    x@proc   <- c(x@proc, "coordinates<-")
    return(x)
})



#' @rdname coordinates   
#' @export
setMethod("coordinates", "GPRsurvey", function(x){
  return(x@coords)
})


#' @rdname coordinates
#' @export
setReplaceMethod("coordinates", signature="GPRsurvey", function(x, value){
  
  # check length
  if(length(value) != length(x@coords)){
    stop("'value' must have the same length as 'coordinates(x)', ", 
         length(x@coords), ".")
  }
  
  test_value_len <- sapply(value, length) > 0 
  # check number of columns
  tst <- sapply(value, ncol) != 3 & test_value_len 
  if(any(tst)){
    stop("All elements must have 3 columns! ",
         "Check element(s) ", paste0(which(tst), collapse = ", "), "...")
  } 
  
  # check number of row
  tst2 <- sapply(value, nrow) != x@nx & test_value_len 
  if(any(tst2)){
    stop("The following elements must have the correct number of rows: ",
         paste("elt.", paste(which(tst2), x@nx[tst2], sep = "->"),
               collapse = ",  "))
  }
  x@coords  <- value
  
  x_lengths <- sapply(x@coords[test_value_len], pathLength, USE.NAMES = FALSE)
  if(length(x_lengths) > 0){
    x@xlengths[test_value_len] <- x_lengths
    
  }
  # print(x_lengths)
  
  x <- intersect(x)
  
  return(x)
})


.updateXpos <- function(x){
  xpos <- relPos(x)
  if(length(xpos) > 0){
    x@x <- xpos
  }
  return(x)
}

# # x = GPR
# .updateXpos <- function(x){
#   if(length(x@coord) > 0){
#     if(isCRSGeographic(x)){ 
#       dx <- verboseF(geodist::geodist(x@coord[,1:2], paired = FALSE, 
#                              sequential = TRUE, pad = FALSE, 
#                              measure = "geodesic"),
#                      verbose = FALSE)
#       x@x <- c(0, cumsum(dx))
#     }else{
#       x@x <- pathRelPos(x@coord[,1:2])
#     }
#   }
#   return(x)
# }
