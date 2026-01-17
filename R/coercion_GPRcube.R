
setAs(from = "array", to = "GPRcube", def = function (from) as.GPRcube.array(from))

#' Coercion from matrix to GPR
#'
#' @name as.GPR.matrix
#' @rdname GPRcoercion
#' @export
#' @concept coercion
as.GPRcube.array <- function (x, ...){
  myArg <- as.list(match.call(definition = sys.function(-2),
                              call = sys.call(-2),
                              expand.dots = FALSE ))
  d_name <- paste(eval(myArg[2]))
  new("GPRcube", 
      #--- class GPRvirtual
      version      = "0.3",  
      name         = d_name,
      path         = "",
      desc         = paste0("coercion of ", as.character(d_name), 
                            " (",typeof(x), ") into GPRcube"),
      mode         = "CO",
      date         = Sys.Date(),
      freq         = NA_real_, 
      
      data         = x,     
      dunit        = "mV",  
      dlab         = "amplitude", 
      
      spunit       = "",  
      crs          = NA_character_,  
      
      xunit        = "m",  
      xlab         = "x-position",
      
      zunit        = "m",  
      zlab         = "depth",
      
      vel          = list(),   
      
      # proc         = "list",
      # delineations = "list",
      #md           = list(),  
      
      #--- class GPRcube
      dx     = 1,
      dy     = 1,
      dz     = 1,
      ylab   = "y-position",  # set names, length = 1|p
      
      center = c(0,0,0),    # coordinates grid corner bottom left (0, 0, 0)
      rot    = numeric(0)     # affine transformation
  )
}

setAs(from = "list", to = "GPRcube", def = function (from) as.GPRcube.list(from))

#' Coercion from list to GPR
#'
#' @name as.GPR.list
#' @rdname GPRcoercion
#' @export
#' @concept coercion
as.GPRcube.list <- function (x, ...){
  # prefix: "d_" for default
  if(all("data" != tolower(names(x)))){
    stop("The list must have a 'data' index name")
  }
  # x[["data"]] <- as.matrix(x[["data"]])
  if(!is.array(x[["data"]])){
    stop("The element 'data' must be an array!")
  }
  if(is.null(x[["dx"]])){
    x[["dx"]] <- 1
  }
  if(is.null(x[["dy"]]) ){
    x[["dy"]] <- 1
  }
  if(is.null(x[["dz"]]) ){
    x[["dz"]] <- 1
  }
  if(is.null(x[["ylab"]]) ){
    x[["ylab"]] <- "y-position"
  }
  if(is.null(x[["xlab"]]) ){
    x[["xlab"]] <- "x-position"
  }
  if(is.null(x[["center"]]) ){
    x[["center"]] <- c(0,0,0)
  }
  if(is.null(x[["rot"]]) ){
    x[["rot"]] <- numeric(0)
  }
  
  if(!is.null(x[["vel"]]) && !is.list(x[["vel"]])){
    x[["vel"]] <- list(v = x[["vel"]])
  }else{
    x[["vel"]] <- list()
  }
  # FIXME -> check values for all slots!!!!
  # if(!is.null(x[["mode"]])){
  #   if(x[["mode"]] == "CMP"){
  #     if(is.null(x[["antsep"]])){
  #       x[["antsep"]] <- x[["x"]]
  #     }      
  #     if(is.null(x[["xlab"]])){
  #       x[["xlab"]] <- "antenna separation"
  #     }
  #   }
  # }
  myArg <- as.list(match.call(definition = sys.function(-2),
                              call = sys.call(-2),
                              expand.dots = FALSE )
  )
  d_name <- paste(eval(myArg[2]))
  y <- new("GPRcube", 
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
           
           vel          = x[["vel"]],   
           
           # proc         = "list",
           # delineations = "list",
           #md           = list(),  
           
           #--- class GPR
           dx     = x[["dx"]],
           dy     = x[["dy"]],
           dz     = x[["dz"]],
           ylab   = x[["ylab"]],  # set names, length = 1|p
           
           center = x[["center"]],    # coordinates grid corner bottom left (0, 0, 0)
           rot    = x[["rot"]]     # affine transformation
           
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
