#' Extract GPR object from GPRsurvey object
#' 
#' Extract GPR object from GPRsurvey object
#' @param x [class \code{GPRsurvey}]
#' @param id [\code{integer(1)|character(1)}] Indice or name of the GPR line to
#'                                            extract.
#' @param verbose [\code{logical(1)}] If \code{TRUE} the function prints some
#'                                    information.
#' @return [\code{GPR class}] An object of the class GPR.
#' @name getGPR
#' @export
setGeneric("getGPR", function(x, id, verbose = FALSE) 
  standardGeneric("getGPR"))

#' @rdname getGPR
#' @export
setMethod("getGPR", "GPRsurvey", function(x, id, verbose = FALSE){
  # if(length(id)>1){
  #   warning("Length of id > 1, I take only the first element!\n")
  #   id <- id[1]
  # }
  # if(is.numeric(id)){
  #   no <- as.integer(id)
  #   if(!(no %in% seq_along(x@paths))){
  #     stop("'id' not in x!")
  #   }
  #   gpr <- verboseF(readGPR(x@paths[[no]]), verbose = verbose)
  # }else if(is.character(id)){
  #   no <- which(x@names == trimStr(id))
  #   if(length(no > 0)){
  #     gpr <- verboseF(readGPR(x@paths[[no]]), verbose = verbose)
  #   }else{
  #     stop("There is no GPR data with the name '", trimStr(id),"'\n")
  #   }
  # }
  # # FIXME : check if nrow(x@coord)
  # if(length(x@coords[[gpr@name]])>0){
  #   gpr@coord <- x@coords[[gpr@name]]
  # }
  # # if(length(x@intersects[[gpr@name]])>0){
  # #   ann(gpr) <- cbind(x@intersects[[gpr@name]]$trace,
  # #                     x@intersects[[gpr@name]]$name)
  # # }
  # # FIXME
  # if(length(x@crs) == 1){
  #   gpr@crs <- x@crs
  # }else{
  #   gpr@crs <- x@crs[no]
  # }
  # # if(length(x@coordref)>0){
  # #   gpr@coordref <- x@coordref
  # # }
  # return(gpr)
  if(length(id)>1){
    warning("Length of id > 1, I take only the first element!\n")
    id <- id[1]
  }
  if(is.numeric(id)){
    no <- id
    gpr <- readGPR(x@paths[[id]])
  }else if(is.character(id)){
    no <- which(x@names == trimStr(id))
    if(length(no > 0)){
      id <- no
      gpr <- readGPR(x@paths[[id]])
    }else{
      stop("There is no GPR data with the name '", trimStr(id),"'\n")
    }
  }
  if(length(x@coords[[id]]) > 0 ){
     # FIXME -> check that nrow(x@coords[[id]]) is correct!!!!
    if(nrow(x@coords[[id]]) != ncol(gpr)){
      stop("nrow(x@coords[[id]]) != ncol(gpr)")
    }
    gpr@coord <- unname(x@coords[[id]] )
    gpr@x <- relPos(gpr)
  }
  if(length(x@intersections[[id]]) > 0 ){
    
    FUN <- function(y, x){
      findClosestCoord(x, y = y)
    }
    x_tr <- apply(x@intersections[[id]], 1, FUN, gpr)
    ann(gpr) <- cbind(x_tr, as.character(x@intersections[[id]]$name))
    
  }
  # FIXME
  if(length(x@crs) == 1){
    gpr@crs <- x@crs
  }else{
    gpr@crs <- x@crs[id]
  }
  
  gpr@spunit <- x@spunit
  if(any(x@zunits != "")){
    gpr@zunit <- x@zunits[x@zunits != ""][1]
  }else{
    gpr@zunit <- ""
    warning("No z-units!!")
  }
  gpr@xunit <- x@spunit
  # what about gpr@xunit ??
  # if(length(x@coordref)>0){
  #   gpr@coordref <- x@coordref
  # }
  return(gpr)
})
