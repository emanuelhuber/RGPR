
#------------------------------------------------------------------------------#
setAs(from = "GPRvirtual", to = "matrix", def = function(from){ from@data } )

#' Coercion to matrix, vector or numeri
#' @param x [\code{GPR}]
#' @param ... Not used
#' @param mode Not used
#' @name as.matrix
#' @rdname GPRcoercion
setGeneric("as.matrix", function(x) standardGeneric("as.matrix")) 

#' @rdname GPRcoercion
#' @export
setMethod("as.matrix", signature("GPRvirtual"), function(x){as(x, "matrix")})

#------------------------------------------------------------------------------#

#' @aliases as.array,GPRvirtual-method
#' @name as.array
#' @rdname GPRcoercion
#' @export
setMethod("as.array", "GPRvirtual",  function(x, ...) as.numeric(x@data))

#------------------------------------------------------------------------------#
setAs(from = "GPRvirtual", to = "vector", def = function(from){ from@data})

#' @aliases as.vector,GPRvirtual-method
#' @rdname GPRcoercion
#' @export
setMethod("as.vector", signature("GPRvirtual"), 
          function(x, mode = "any"){as.vector(x@data)})


#------------------------------------------------------------------------------#

#' @aliases as.numeric,GPRvirtual-method
#' @name as.numeric
#' @rdname GPRcoercion
#' @export
setMethod("as.numeric", "GPRvirtual",  function(x, ...) as.numeric(x@data))


#' @aliases as.integer,GPRvirtual-method
#' @name as.integer
#' @rdname GPRcoercion
#' @export
setMethod("as.integer", "GPRvirtual",  function(x, ...) as.integer(as.numeric(x@data)))



setMethod("as.double", "GPRvirtual",  function(x, ...) as.double(x@data))
