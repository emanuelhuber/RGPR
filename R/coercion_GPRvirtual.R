
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




###--- Coercion from ... to GPR
setAs(from = "matrix", to = "GPR", def = function (from) as.GPR.matrix(from))

setAs(from = "data.frame", to = "GPR", def = function (from) as.GPR.data.frame(from))


#' Coercion from data.frame to GPR
#'
#' @name as.GPR.data.frame
#' @rdname GPRcoercion
#' @export
as.GPR.data.frame <- function(x, ...){
  as.GPR.matrix(as.matrix(x))
}


#' Coercion from matrix to GPR
#'
#' @name as.GPR.matrix
#' @rdname GPRcoercion
#' @export
as.GPR.matrix <- function (x, ...){
  myArg <- as.list(match.call(definition = sys.function(-2),
                              call = sys.call(-2),
                              expand.dots = FALSE ))
  d_name <- paste(eval(myArg[2]))
  new("GPR", 
      #--- class GPRvirtual
      version      = "0.3",  
      name         = d_name,
      path         = "",
      desc         = paste0("coercion of ", as.character(d_name), 
                            " (",typeof(x), ") into GPR"),
      mode         = "CO",
      date         = Sys.Date(),
      freq         = NA_real_, 
      
      data         = x,     
      dunit        = "mV",  
      dlab         = "amplitude", 
      
      spunit       = "",  
      crs          = NA_character_,  
      
      xunit        = "m",  
      xlab         = "position",
      
      zunit        = "ns",  
      zlab         = "two-way travel time",
      
      vel          = list(0.1),   
      
      # proc         = "list",
      # delineations = "list",
      #md           = list(),  
      
      #--- class GPR
      z0           = rep(0, ncol(x)),    
      time         = rep(0, ncol(x)),    
      antsep       = NA_real_,    
      markers      = rep("", ncol(x)), 
      # ann          = "character", 
      
      coord        = matrix(nrow = 0, ncol = 3),     
      rec          = matrix(nrow = 0, ncol = 3),     
      trans        = matrix(nrow = 0, ncol = 3),     
      
      x            = 1:ncol(x) - 1,     
      z            = seq(0, by = 1, length.out = nrow(x))  
  )
}

setAs(from = "list", to = "GPR", def = function (from) as.GPR.list(from))

#' Coercion from list to GPR
#'
#' @name as.GPR.list
#' @rdname GPRcoercion
#' @export
as.GPR.list <- function (x, ...){
  # prefix: "d_" for default
  if(all("data" != tolower(names(x)))){
    stop("The list must have a 'data' index name")
  }
  x[["data"]] <- as.matrix(x[["data"]])
  if(!is.matrix(x[["data"]])){
    stop("The element 'data' must be a matrix!")
  }
  if(is.null(x[["x"]])){
    x[["x"]] <- 1:ncol(x[["data"]]) -1
  }
  if(is.null(x[["z"]]) ){
    x[["z"]] <- 1:nrow(x[["z"]]) - 1
  }
  if(!is.null(x[["vel"]]) && !is.list(x[["vel"]])){
    x[["vel"]] <- list(x[["vel"]])
  }
  # FIXME -> check values for all slots!!!!
  if(!is.null(x[["mode"]])){
    if(x[["mode"]] == "CMP"){
      if(is.null(x[["antsep"]])){
        x[["antsep"]] <- x[["x"]]
      }      
      if(is.null(x[["xlab"]])){
        x[["xlab"]] <- "antenna separation"
      }
    }
  }
  myArg <- as.list(match.call(definition = sys.function(-2),
                              call = sys.call(-2),
                              expand.dots = FALSE )
  )
  d_name <- paste(eval(myArg[2]))
  y <- new("GPR", 
           version      = "0.3",  
           name         = d_name,
           path         = "",
           desc         = paste0("coercion of ", as.character(d_name), 
                                 " (",typeof(x), ") into GPR"),
           mode         = "CO",
           date         = Sys.Date(),
           freq         = NA_real_, 
           
           data         = x[["data"]],     
           dunit        = "mV",  
           dlab         = "amplitude", 
           
           spunit       = "",  
           crs          = NA_character_,  
           
           xunit        = "m",  
           xlab         = "position",
           
           zunit        = "ns",  
           zlab         = "two-way travel time",
           
           vel          = list(0.1),   
           
           # proc         = "list",
           # delineations = "list",
           #md           = list(),  
           
           #--- class GPR
           z0           = rep(0, ncol(x[["data"]])),    
           time         = rep(0, ncol(x[["data"]])),    
           antsep       = NA_real_,    
           markers      = rep("", ncol(x[["data"]])), 
           # ann          = "character", 
           
           coord        = matrix(nrow = 0, ncol = 3),     
           rec          = matrix(nrow = 0, ncol = 3),     
           trans        = matrix(nrow = 0, ncol = 3),     
           
           x            = x[["x"]],     
           z            = x[["z"]]  
  )
  sNames <- slotNames(y)
  sNames <- sNames[ !(sNames %in% c("data", "x", "z"))]
  for(i in seq_along(sNames)){
    if(!is.null(x[[sNames[i]]])){
      slot(y, sNames[i], check = TRUE) <- x[[sNames[i]]]
    }
  }
  return(y)
}    

