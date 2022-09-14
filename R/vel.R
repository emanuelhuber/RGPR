
#' Set and get velocity
#' 
#' Set and get velocity model
#' 
#' The argument \code{value} can be
#' \itemize{
#'   \item a scalar (length-one vector) for uniform velocity
#'   \item a vector of length equal to the sample number (row number) of x (\code{m}).
#'   \item a matrix of dimension equal to the sample and trace numner 
#'          (row and column number) of x (\code{m} \eqn{\times} \code{n}).
#'   \item a list with elements \code{t} and \code{v} having the same length.
#'         \code{t} defines the lower time boundaries of the velocities
#'         \code{v}. FIXME: "intp", "smooth"
#' }
#' 
#' @param x [\code{GPR class}] An object of the class \code{GPR}
#' @param value [numeric(1)|numeric(m)|matrix(m,n)|list] The velocity model, see section 
#' Details
#' @return [\code{list}|\code{numeric}|\code{matrix}] The velocities as they
#'         are stored in \code{x}.
#' @name vel
#' @rdname vel
setGeneric("vel", function(x) standardGeneric("vel"))

#' @rdname vel
#' @export
setMethod("vel", "GPR", function(x){
  return(x@vel)
} 
)


#' @name vel<-
#' @rdname vel
setGeneric("vel<-", function(x, value) standardGeneric("vel<-"))

#' @rdname vel
#' @export
setReplaceMethod("vel", "GPR", function(x, value){
  if(is.list(value)){
    # FIXME
    print(names(value))
    if(all(names(value) %in% c("t", "v"))){
      x@vel <- list("v" = checkVelIntegrity(x, value))
    }else{
      # print("===========================")
      sel <- names(value) %in% c("vrms", "vint", "v")
      print(sel)
      if(!all(sel)){
        print(!all(sel))
        warning("velocity type/s '", names(value)[!sel], "' is/are not supported.\n",
                "Please use one of the following: 'vrms', 'vint', 'v'")
      }
      value <- value[sel]
      for(i in seq_along(value)){
        # print("- - - - - - - -")
        # print(paste0("name = ", names(value)[i]))
        # val <- value[[i]]
        # vector or matrix
        # if(is.list(val)){
        #   print("list")
        #   print(val)
        #   # print(val[[1]])
        #   
        # # list
        # }else{
        #   print("vector/matrix")
        #   print(val)
        #   
        #   # print(val[[1]])
        # }
        value[[i]] <- checkVelIntegrity(x, value[[i]])
        # x@vel[["vrms"]]
      }
      # print("******************")
      # print(value)
      x@vel <- value
    }
  }else{
    x@vel <- list("v" = checkVelIntegrity(x, value))
  }
  return(x)
})

checkVelIntegrity <- function(x, value){
  if(is.list(value)){
    valnames <- names(value)
    tst <- valnames %in% c("t", "v", "intp", "smooth")
    if(!all(tst)){
      stop("only 't', 'v', 'intp', 'smooth'")
    }else{
      if(length(value[["t"]]) != length(value[["v"]])){
        stop("'v' and 't' must have the same length")
      }
      if(!is.null(value[["intp"]])){
        if(!is.character(value[["intp"]]) || length(value[["intp"]]) != 1 ){
          stop("'intp' must be a character and have length one")
        }
      }
      if(!is.null(value[["smooth"]])){
        if(!is.logical(value[["smooth"]]) || length(value[["smooth"]]) != 1 ){
          stop("'smooth' must be a logical and have length one")
        }
      }
    }
    return(value)
  }else if(is.matrix(value)){
    if(any(value <= 0)){
      stop("Velocity must be strictly positive")
    }
    # check dimension
    if(ncol(value) > 1){
      if(all(dim(value) == dim(x))){
        return(value)
      }else{
        stop("dim(x) != dim(value)")
      }
    }else{
      if(all(nrow(value) == nrow(x))){
        return(as.vector(value))
      }else{
        stop("nrow(x) != nrow(value)")
      }
    }
  }else if(is.null(dim(value))){
    if(any(value <= 0)){
      stop("Velocity must be strictly positive")
    }
    if(length(value) > 1){
      if(length(value) == nrow(x)){
        return( unname(value))
      }else{
        stop("nrow(x) != length(value)")
      }
    }else{
      return(unname(value))
    }
  }else{
    stop("structure not implemented. Please contact me:\n",
         "emanuel.huber@pm.me")
  }
}


# return either 1 value, a vector or FIXME: a matrix
# @param strict [logical(1)] If TRUE, .getVel2 raises an error if the velocity
#               type does not exist. If FALSE, it will check if a velocity "v"
#               exists and return it
.getVel2 <- function(x, type = c("vrms", "vint", "v"), strict = TRUE){
  type <- match.arg(type, c("vrms", "vint", "v"))
  if(length(x@vel) == 0){
    stop("You must first assign a positiv velocity value!")
  }else{
    if(is.null(x@vel[[type]])){
      if(strict){
        stop("You must first set this type of velocity: ", type)
      }else{
        if(is.null(x@vel[["v"]]) && strict){
          stop("You must first set at least one of these types of velocity: ", 
               type, ", v!")
        }else if(length(x@vel) == 1){
          v <- .intpSmoothVel(x@vel[[1]], x_z = x@depth)
        }else{
          v <- .intpSmoothVel(x@vel[["v"]], x_z = x@depth)
        }
      }
    }else{
      v <- .intpSmoothVel(x@vel[[type]], x_z = x@depth)
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

#' @name setVel
#' @rdname vel
#' @export
setGenericVerif("setVel", function(x, v) standardGeneric("setVel"))

#' @name setVel
#' @rdname vel
#' @export
setMethod("setVel", "GPR", function(x, v){
  vel(x) <- v 
  return(x)
})