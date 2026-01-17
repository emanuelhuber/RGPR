
#' Apply bacth processing to GPRsurvey object
#' 
#' @param obj     (`GPR`) An object of the class GPR.
#' @param prc     (`list`) A list of processing funtions with their arguments
#' @returns (`GPRsurvey`)
#' @name papply
#' @rdname papply
#' @export
#' @concept processing
setGeneric("papply", function(obj, prc = NULL) standardGeneric("papply"))


#' @rdname papply
#' @export
setMethod("papply", "GPRsurvey", function(obj, prc = NULL){
  if(typeof(prc) != "list") stop("'prc' must be a list")
  # zunits <- vector(mode = "character", length = length(obj))
  for(i in seq_along(obj)){
    y <- verboseF( obj[[i]], verbose = FALSE )
    message('Processing ', y@name, '...', appendLF = FALSE)
    for(k in seq_along(prc)){
      y <- do.call(names(prc[k]), c(obj = y,  prc[[k]]))
    }
    obj@paths[i] <- .saveTempFile(y)
    obj@names[i]        <- y@name
    obj@descs[i]        <- y@desc
    obj@modes[i]        <- y@mode
    
    obj@dates[i]        <- y@date
    
    obj@freqs[i]        <- y@freq
    obj@antseps[i]      <- y@antsep
    
    # obj@coordref
    if(length(y@coord) > 0){
      obj@coords[[i]] <- y@coord
    } 
    
    # obj@intersections
    obj@markers[[i]]  <- trimStr(y@markers)
    
    obj@nz[i]         <- nrow(y@data)
    obj@zlengths[i]   <- diff(range(y@z, nr.rm = TRUE))
    obj@zunits[i]     <- y@zunit
    obj@nx[i]         <- ncol(y@data)
    obj@xlengths[i]   <-ifelse(length(y@coord) > 0,
                               tail(relPos(y[,1:2]), 1),
                               diff(range(x@x, na.rm = TRUE)))
    message(' done!', appendLF = TRUE)
  }
  # obj@intersections <- list()
  # obj <- setCoordref(obj)
  return(obj)
} 
)

#' @name papply
#' @rdname papply
#' @export
setMethod("papply", "GPR", function(obj, prc = NULL){
  if(typeof(prc) != "list") stop("'prc' must be a list")
  for(k in seq_along(prc)){
    obj <- do.call(names(prc[k]), c(obj = obj,  prc[[k]]))
    message("*", appendLF = FALSE)
  }
  message("")
  return(obj)
} 
)