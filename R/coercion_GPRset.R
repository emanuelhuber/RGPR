

###--- Coercion from ... to GPRset
setAs(from = "matrix", to = "GPRset", def = function (from) as.GPRset.matrix(from))
setAs(from = "array", to = "GPRset", def = function (from) as.GPRset.array(from))
setAs(from = "data.frame", to = "GPRset", def = function (from) as.GPRset.data.frame(from))
setAs(from = "GPR", to = "GPRset", def = function (from) as.GPRset.GPR(from))


#' Coercion from data.frame to GPRset
#'
#' @name as.GPRset.data.frame
#' @rdname GPRcoercion
#' @export
#' @concept coercion
as.GPRset.data.frame <- function(x, ...){
  as.GPRset.matrix(as.matrix(x))
}


#' Coercion from matrix to GPRset
#'
#' @name as.GPRset.matrix
#' @rdname GPRcoercion
#' @export
#' @concept coercion
as.GPRset.matrix <- function (x, ...){
  # myArg <- as.list(match.call(definition = sys.function(-2),
  #                             call = sys.call(-2),
  #                             expand.dots = FALSE ))
  # d_name <- paste(eval(myArg[2]))
  d_name <- ""
  new("GPRset", 
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
      
      vel          = list(v = 0.1),   
      
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
      z            = seq(0, by = 1, length.out = nrow(x)),  
      
      #--- class GPRset
      y            = 0,
      yunit        = "",
      ylab         = ""
  )
}


#' Coercion from array to GPRset
#'
#' @name as.GPRset.array
#' @rdname GPRcoercion
#' @export
#' @concept coercion
as.GPRset.array <- function (x, ...){
  # myArg <- as.list(match.call(definition = sys.function(-2),
  #                             call = sys.call(-2),
  #                             expand.dots = FALSE ))
  # d_name <- paste(eval(myArg[2]))
  d_name <- ""
  new("GPRset", 
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
      
      vel          = list(v = 0.1),   
      
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
      z            = seq(0, by = 1, length.out = nrow(x)),  
      
      #--- class GPRset
      y            = seq(from = 1, to = dim(x)[3]),
      yunit        = rep("", dim(x)[3]),
      ylab         = rep("", dim(x)[3])
  )
}


#' Coercion from GPR to GPRset
#'
#' @name as.GPRset.GPR
#' @rdname GPRcoercion
#' @export
#' @concept coercion
as.GPRset.GPR <- function (x, ...){
  # myArg <- as.list(match.call(definition = sys.function(-2),
  #                             call = sys.call(-2),
  #                             expand.dots = FALSE ))
  # d_name <- paste(eval(myArg[2]))
  d_name <- ""
  new("GPRset", 
      #--- class GPRvirtual
      version      = x@version,  
      name         = x@name,
      path         = x@path,
      desc         = x@desc,
      mode         = x@mode,
      date         = x@date,
      freq         = x@freq, 
      
      data         = array(x@data, dim = c(dim(x), 1)),     
      dunit        = x@dunit,  
      dlab         = x@dlab, 
      
      spunit       = x@spunit, 
      crs          = x@crs,  
      
      xunit        = x@xunit,  
      xlab         = x@xlab,
      
      zunit        = x@zunit,  
      zlab         = x@zlab,
      
      vel          = x@vel,   
      
      # proc         = "list",
      # delineations = "list",
      #md           = list(),  
      
      #--- class GPR
      z0           = x@z0,    
      time         = x@time,    
      antsep       = x@antsep,    
      markers      = x@markers, 
      # ann          = "character", 
      
      coord        = x@coord,     
      rec          = x@rec,     
      trans        = x@trans,     
      
      x            = x@x,     
      z            = x@z,  
      
      #--- class GPRset
      y            = 0,
      yunit        = "",
      ylab         = ""
  )
}
