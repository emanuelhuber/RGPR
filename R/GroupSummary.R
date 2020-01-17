
#' @export
setMethod(
  f="min", 
  signature="GPR", 
  definition=function(x,...,na.rm=FALSE){
    min(x@data,na.rm=na.rm)
  }
)
#' @export
setMethod(
  f="max", 
  signature="GPR", 
  definition=function(x,...,na.rm=FALSE){
    max(x@data,na.rm=na.rm)
  }
)
#' @export
setMethod(
  f="range", 
  signature="GPR", 
  definition=function(x,...,na.rm=FALSE){
    range(x@data,na.rm=na.rm)
  }
)

#' @export
setMethod(   
  # can make the sum of a list of object of class GPR
  f = "sum", 
  signature = "GPR", 
  definition = function(x, ..., na.rm = FALSE){
    dots <- list(...)
    if(length(dots) == 0){
      sum(as.matrix(x), na.rm = na.rm)
    }else{
      z <- lapply(dots, function(x){ sum(x@data, na.rm)})
      Reduce("+", c(z, sum(as.matrix(x), na.rm = na.rm)))
    }
  }
)

#' @export
setMethod(   
  # can make the prod of a list of object of class GPR
  f = "prod", 
  signature = "GPR", 
  definition = function(x, ..., na.rm = FALSE){
    dots <- list(...)
    if(length(dots) == 0){
      prod(as.matrix(x), na.rm = na.rm)
    }else{
      z <- lapply(dots, function(x){ prod(x@data, na.rm)})
      # Reduce("*", z) * prod(as.matrix(x), na.rm = na.rm)
      Reduce("*", c(z, prod(as.matrix(x), na.rm = na.rm)))
    }
  }
)

#' @export
setMethod(   
  # can make the all of a list of object of class GPR
  f = "all", 
  signature = "GPR", 
  definition = function(x, ..., na.rm = FALSE){
    dots <- list(...)
    if(length(dots) == 0){
      all(as.matrix(x), na.rm = na.rm)
    }else{
      z <- lapply(dots, function(x){ all(x@data, na.rm)})
      Reduce(all, c(z, all(as.matrix(x), na.rm = na.rm)))
    }
  }
)

#' @export
setMethod(   
  # can make the all of a list of object of class GPR
  f = "any", 
  signature = "GPR", 
  definition = function(x, ..., na.rm = FALSE){
    dots <- list(...)
    if(length(dots) == 0){
      any(as.matrix(x), na.rm = na.rm)
    }else{
      z <- lapply(dots, function(x){ any(x@data, na.rm)})
      Reduce(any, c(z, any(as.matrix(x), na.rm = na.rm)))
    }
  }
)