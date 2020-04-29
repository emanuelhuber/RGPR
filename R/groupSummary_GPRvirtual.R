

#' Basic summary functions
#'
#' Methods for the base Summary methods \link[methods]{S4groupGeneric}
#' @param x An object of the class GPRvirtual
#' @param ... further arguments 
#' @param na.rm [\code{logical(1)}] should missing values be removed?
#' @details Currently implemented methods include:
#' \itemize{
#'  \item{all, any, sum, prod, min, max, range}
#'  }
#' @rdname Summary-methods
#' @aliases Summary-GPRvirtual-method
setMethod(
  f = "Summary",
  signature = "GPRvirtual",
  definition = function(x, ..., na.rm = FALSE){
    # x <- list(...)[[1]]
    x_summary <- switch(.Generic,
                     max = .GPR.max(x, ..., na.rm = na.rm),
                     min = .GPR.min(x, ..., na.rm = na.rm),
                     range = .GPR.range(x, ..., na.rm = na.rm),
                     prod = .GPR.prod(x, ..., na.rm = na.rm),
                     sum = .GPR.sum(x, ..., na.rm = na.rm),
                     any = .GPR.any(x, ..., na.rm = na.rm),
                     all = .GPR.all(x, ..., na.rm = na.rm),
                     stop(paste(.Generic, "not yet allowed on GPR objects"))
    )
    # proc(x) <- getArgs()
    return(x_summary)
  }
)

.GPR.max <- function(x,...,na.rm = FALSE){
  dots <- list(...)
  if(length(dots) == 0){
    max(x@data, na.rm = na.rm)
  }else{
    z <- sapply(dots, function(x){ max (x, na.rm = na.rm)})
    return(max(max(x@data), z))
  }
}

.GPR.min <- function(x,...,na.rm = FALSE){
  dots <- list(...)
  if(length(dots) == 0){
    min(x@data, na.rm = na.rm)
  }else{
    z <- sapply(dots, function(x){ min (x, na.rm = na.rm)})
    return(min(min(x@data), z))
  }
}


.GPR.range <- function(x,...,na.rm = FALSE){
  dots <- list(...)
  if(length(dots) == 0){
    range(x@data, na.rm = na.rm)
  }else{
    z <- sapply(dots, function(x){ range (x, na.rm = na.rm)})
    return(range(range(x@data), z))
  }
}


.GPR.prod <- function(x,...,na.rm = FALSE){
  dots <- list(...)
  if(length(dots) == 0){
    prod(x@data, na.rm = na.rm)
  }else{
    z <- sapply(dots, function(x){ prod (x, na.rm = na.rm)})
    Reduce("*", c(z, prod(x@data, na.rm = na.rm)))
  }
}

.GPR.sum <- function(x,...,na.rm = FALSE){
  dots <- list(...)
  if(length(dots) == 0){
    sum(x@data, na.rm = na.rm)
  }else{
    z <- sapply(dots, function(x){ sum (x, na.rm = na.rm)})
    Reduce("+", c(z, sum(x@data, na.rm = na.rm)))
  }
}

.GPR.any <- function(x,...,na.rm = FALSE){
  dots <- list(...)
  if(length(dots) == 0){
    any(x@data, na.rm = na.rm)
  }else{
    z <- lapply(dots, function(x){ any (x, na.rm = na.rm)})
    Reduce("any", c(z, any(x@data, na.rm = na.rm)))
  }
}

.GPR.all <- function(x,...,na.rm = FALSE){
  dots <- list(...)
  if(length(dots) == 0){
    all(x@data, na.rm = na.rm)
  }else{
    z <- lapply(dots, function(x){ all (x, na.rm = na.rm)})
    Reduce("all", c(z, all(x@data, na.rm = na.rm)))
  }
}

