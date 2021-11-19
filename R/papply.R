#####
setGeneric("papply", function(x, prc = NULL) standardGeneric("papply"))
##########



#' Apply processing to GPRsurvey object
#' 
#' @name papply
#' @rdname papply
#' @export
setMethod("papply", "GPRsurvey", function(x, prc = NULL){
  if(typeof(prc) != "list") stop("'prc' must be a list")
  for(i in seq_along(x)){
    y <- verboseF( x[[i]], verbose = FALSE )
    message('Processing ', y@name, '...', appendLF = FALSE)
    for(k in seq_along(prc)){
      y <- do.call(names(prc[k]), c(x = y,  prc[[k]]))
    }
    x@names[[i]]        <- y@name
    x@descriptions[[i]] <- y@description
    x@lengths[[i]]      <- y@dx * ncol(y@data)
    x@surveymodes[[i]]  <- y@surveymode
    x@posunits[[i]]     <- y@posunit
    if(length(x@crs) == 1){
      x@crs <- y@crs
    }else{
      x@crs[[i]]          <- y@crs
    }
    x@fids[[i]]         <- y@fid
    x@ntraces[[i]]      <- ncol(y)
    x@nz[[i]]           <- nrow(y)
    x@dz[[i]]           <- mean(diff(y@depth))
    x@zunits[[i]]       <- y@depthunit
    if(length(y@date) == 0){
      x@dates[[i]]        <- NA
    }else{
      x@dates[[i]]        <- y@date
    }
    if(length(y@freq) == 0){
      x@freqs[[i]]        <- NA
    }else{
      x@freqs[[i]]        <- y@freq
    }
    if(length(y@antsep) == 0){
      x@antseps[[i]]        <- NA
    }else{
      x@antseps[[i]]      <- y@antsep
    }
    if(length(y@coord) > 0){
      x@coords[[y@name]] <- y@coord
      x@lengths[[i]]      <- posLine(y[,1:2],last=TRUE)
    } 
    x@filepaths[[i]] <- .saveTempFile(y)
    message(' done!', appendLF = TRUE)
  }
  x@intersections <- list()
  x <- setCoordref(x)
  return(x)
} 
)

#' @name papply
#' @rdname papply
#' @export
setMethod("papply", "GPR", function(x, prc = NULL){
  if(typeof(prc) != "list") stop("'prc' must be a list")
    for(k in seq_along(prc)){
      x <- do.call(names(prc[k]), c(x = x,  prc[[k]]))
      message("*", appendLF = FALSE)
    }
    message("")
  return(x)
} 
)

