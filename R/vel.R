


# return either 1 value, a vector or FIXME: a matrix
# @param strict [logical(1)] If TRUE, .getVel raises an error if the velocity
#               type does not exist. If FALSE, it will check if a velocity "v"
#               exists and return it
.getVel <- function(x, type = c("vrms", "vint"), strict = TRUE){
  type <- match.arg(type, c("vrms", "vint"))
  if(length(x@vel) == 0){
    stop("You must first assign a positiv velocity value!")
  }else{
    if(is.null(x@vel[[type]])){
      if(strict){
        stop("You must first set this type of velocity: ", type)
      }else{
        if(is.null(x@vel[["v"]])){
          stop("You must first set at least on of this type of velocity: ", 
               type, ", v!")
        }else{
          v <- .intpSmoothVel(x@vel[["v"]], x_z = x@z)
        }
      }
    }else{
      v <- .intpSmoothVel(x@vel[[type]], x_z = x@z)
      # if(!is.null(x@vel[["type"]][["intp"]])){
      #   v <- .interpVel(x, type = type, method = x@vel[[type]][["intp"]])
      # }
      # if(!is.null(x@vel[["type"]][["smooth"]])){
      #   v <-  mmand::gaussianSmooth(v, sigma = x@vel[["type"]][["smooth"]]) 
      # }
    }
    # v <- x@vel[["v"]]
    if(is.list(v)){
      return(v[["v"]])
    }else{
      return(v)
    }
  }
}


