
#' @name name
#' @rdname name
#' @export
setGenericVerif("name", function(x) standardGeneric("name"))

#' Name of the GPR data
#' 
#' @name name
#' @rdname name
#' @export
setMethod("name", "GPR", function(x){
  return(x@name)
} 
)

#' @name name<-
#' @rdname name
#' @export
setGenericVerif("name<-",function(x,value){standardGeneric("name<-")})

#' @name name<-
#' @rdname name
#' @export
setReplaceMethod(
  f="name",
  signature="GPR",
  definition=function(x,value){
    value <- as.character(value)[1]
    x@name <- value
    x@proc <- c(x@proc, "name<-")
    return(x)
  }
)

#' @name setName
#' @rdname name
#' @export
setGenericVerif("setName", function(x, value) standardGeneric("setName"))


#' @name setName
#' @rdname name
#' @export
setMethod("setName", "GPR", function(x, value){
  name(x) <- value
  return(x)
} 
)

#------------------------------------------------------------------------------#

#' Depth unit of the GPR data
#' 
#' @name depthunit
#' @rdname depthunit
#' @export
setMethod("depthunit", "GPR", function(x){
  return(x@depthunit)
} 
)

#' @name depthunit<-
#' @rdname depthunit
#' @export
setReplaceMethod(
  f="depthunit",
  signature="GPR",
  definition=function(x,value){
    value <- as.character(value)[1]
    x@depthunit <- value
    x@proc <- c(x@proc, "depthunit<-")
    return(x)
  }
)

#------------------------------------------------------------------------------#

#' Position unit of the GPR data
#' 
#' @name posunit
#' @rdname posunit
#' @export
setMethod("posunit", "GPR", function(x){
  return(x@posunit)
} 
)

#' @name posunit<-
#' @rdname posunit
#' @export
setReplaceMethod(
  f="posunit",
  signature="GPR",
  definition=function(x,value){
    value <- as.character(value)[1]
    x@posunit <- value
    x@proc <- c(x@proc, "posunit<-")
    return(x)
  }
)

#------------------------------------------------------------------------------#

#' Description of the GPR data
#' 
#' @name description
#' @rdname description
#' @export
setMethod("description", "GPR", function(x){
  return(x@description)
} 
)

#' @name description<-
#' @rdname description
#' @export
setReplaceMethod(
  f="description",
  signature="GPR",
  definition=function(x, value){
    x@description <- as.character(value)[1]
    x@proc <- c(x@proc, "description<-")
    return(x)
  }
)

#------------------------------------------------------------------------------#


#' Filepath of the GPR data
#' 
#' @rdname filepath-methods
#' @aliases filepath,GPR-method
setMethod("filepath", "GPR", function(x){
  return(x@filepath)
} 
)

#' @rdname filepath-methods
#' @aliases filepath<-,GPR-method
setReplaceMethod(
  f="filepath",
  signature="GPR",
  definition=function(x,value){
    x@filepath <- as.character(value)[1]
    x@proc <- c(x@proc, "filepath<-")
    return(x)
  }
)

#------------------------------------------------------------------------------#

#' Annotations of the GPR data
#' 
#' @name ann
#' @rdname ann
#' @export
setMethod("ann", "GPR", function(x){
  return(x@ann)
} 
)

#' @name ann<-
#' @rdname ann
#' @export
setReplaceMethod(
  f="ann",
  signature="GPR",
  definition=function(x,value){
    vals <- value
    if(!is.matrix(value)){
      vals <- matrix(value,nrow=1,ncol=length(value))
    }
    traces <- (value[,1])
    annnames <- as.character(value[,2])
    valuesList <- (tapply(annnames, traces, identity))
    test <- unlist(lapply(valuesList, paste, sep = "", collapse = "#"), 
                   use.names = TRUE)
    if(length(x@ann) != length(x)) x@ann <- character(length(x))
    x@ann[as.numeric(names(test))] <- test
    x@proc <- c(x@proc, "ann<-")
    # FIXME > sapply(test, trimStr)
    return(x)
  }
)

#------------------------------------------------------------------------------#


#' Coordinates of the GPR data
#' 
#' @rdname coord-methods
#' @aliases coord,GPR-method
setMethod("coord", "GPR", function(x, i, ...){
  #     if(is.integer(i) && i > 0 && i < 4)
  if(length(x@coord) == 0){
    return(x@coord)
  }
  if(missing(i)){
    i <- 1:3
  }
  return(x@coord[, i, ...])
} 
)


#' @rdname coord-methods
#' @aliases coord<-,GPR-method
setReplaceMethod(
  f = "coord",
  signature = "GPR",
  definition = function(x, value){
    if(is.null(value)){
      x@coord <- matrix(nrow = 0, ncol = 3)
      x@proc <- c(x@proc, "coord<-//NULL")
    }else{
      value <- as.matrix(value)
      if(ncol(x@data) == nrow(value) && ncol(value) == 3){
        x@coord <- as.matrix(value)
        x <- trRmDuplicates(x, verbose = FALSE)
        x@proc <- c(x@proc, "coord<-//")
      }else if(length(value) == 0){
        x@coord <- matrix(nrow = 0, ncol = 3)
        x@proc <- c(x@proc, "coord<-//NULL")
      }else{
        stop("Dimension should be ", nrow(value), "x", ncol(value), "!!")
      }
    }
    return(x)
  }
)




#------------------------------------------------------------------------------#

#' Survey date
#' 
#' Return NULL if no date exists, else an object of the class 'Date'
#' @name svDate
#' @rdname svDate
#' @export
setMethod("svDate", "GPR", function(x){
  if(length(x@date) > 0){
    return(as.Date(x@date))
  }else{
    return(NULL)
  }
} 
)

#' @param x An object of the class 'GPR'
#' @param value An object of the class 'Date'
#' @name svDate<-
#' @rdname svDate
#' @export
setReplaceMethod(
  f="svDate",
  signature="GPR",
  definition=function(x,value){
    value <- value[1]
    if(class(value) == "Date"){
      x@date <- as.character(value)
      x@proc <- c(x@proc, "svDate<-")
      return(x)
    }else{
      stop("'value' must be from the class 'Date'!")
    }
  }
)

#------------------------------------------------------------------------------#


#' Coordinate reference system (CRS) of the GPR data
#' 
#' @name crs
#' @rdname crs
#' @export
setMethod("crs", "GPR", function(x){
  return(x@crs)
} 
)

#' @name crs<-
#' @rdname crs
#' @export
setReplaceMethod(
  f="crs",
  signature="GPR",
  definition=function(x,value){
    value <- as.character(value)[1]
    x@crs <- value
    x@proc <- c(x@proc, "crs<-")
    return(x)
  }
)


#------------------------------------------------------------------------------#

#' @name vel
#' @rdname vel
#' @export
setGenericVerif("vel", function(x) standardGeneric("vel"))

#' Velocity model of the GPR data
#' 
#' @name vel
#' @rdname vel
#' @export
setMethod("vel", "GPR", function(x){
  if(length(x@vel) == 1){
    return(x@vel[[1]])
  }else{
    return(x@vel)
  }
})

#' @name vel<-
#' @rdname vel
#' @export
setGenericVerif("vel<-",function(x,value){standardGeneric("vel<-")})

#' @name vel<-
#' @rdname vel
#' @export
setReplaceMethod(
  f="vel",
  signature="GPR",
  definition=function(x, value){
    if(typeof(value) != "list"){
      value <- list(as.numeric(value))
    }
    x@vel <- value
    x@proc <- c(x@proc, "vel<-")
    return(x)
})

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

#------------------------------------------------------------------------------#

setMethod("trTime", "GPR", function(x){
  dorigin <- x@date
  if(length(dorigin) == 0){
    dorigin <- "1970-01-01"
  }
  traceTime <- as.double(as.POSIXct(x@time, origin = as.Date(dorigin)))
  return(traceTime)
} 
)


#' 'time-zero' of every traces
#' 
#' \code{time0} returns the 'time-zero' of every traces. Generally, 
#' 'time-zero' corresponds to the first wave arrival (also called first wave
#' break).
#' 
#' @param x An object of the class GPR.
#' @return A vector containing the time-zero values of each traces.
#' @examples
#' data(frenkeLine00)
#' time0(frenkeLine00)
#' @seealso \code{\link{firstBreak}} to estimate the first wave break.
#' @name time0
#' @rdname time0
#' @export
# @aliases time0-methods time0<- time0<--methods
setMethod("time0", "GPR", function(x){
  return(x@time0)
} 
)


#' @name time0<-
#' @rdname time0
#' @export
setReplaceMethod(
  f="time0",
  signature="GPR",
  definition=function(x, value){
    
    value <- as.numeric(value)
    
    #------------------- check arguments
    msg <- checkArgInit()
    msg <- checkArg(value,     msg, "NUMERIC_LEN", c(1, ncol(x)))
    checkArgStop(msg)
    #------------------- check arguments
    
    if(length(value) == 1) value <- rep(value, ncol(x))
    x@time0 <- value
    
    x@proc <- c(x@proc, "time0<-")
    return(x)
  }
)

#' Wrapper for \code{time0()<-}
#'
#' @name setTime0
#' @rdname time0
#' @export
setMethod("setTime0", "GPR", function(x, t0, track = TRUE){
  
  t0 <- as.numeric(t0)
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(t0,     msg, "NUMERIC_LEN", c(1, ncol(x)))
  msg <- checkArg(track,   msg, "LOGICAL_LEN", 1)
  checkArgStop(msg)

  if(length(t0) == 1) t0 <- rep(t0, ncol(x))
  x@time0 <- t0
  #------------------- check arguments
  
  if(isTRUE(track)) proc(x) <- getArgs()
  return(x)
})





#' Depth/time of the GPR data
#' 
#' @name depth
#' @rdname depth
#' @export
setMethod("depth", "GPR", function(x){
  return(x@depth)
})

#' @name depth<-
#' @rdname depth
#' @export
setReplaceMethod(
  f = "depth",
  signature = "GPR",
  definition = function(x, value){
    if(length(value) == length(x@depth)){
      x@depth <- value
      x@dz <- mean(abs(diff(value)))
    }else{
      stop("length(value) != length(x@depth)")
    }
    x@proc <- c(x@proc, "depth<-")
    return(x)
  }
)

#' Position of the GPR traces
#' 
#' @name pos
#' @rdname pos
#' @export
setMethod("pos", "GPR", function(x){
  return(x@pos)
} 
)
#' @name pos<-
#' @rdname pos
#' @export
setReplaceMethod(
  f = "pos",
  signature = "GPR",
  definition = function(x, value){
    if( length(x@coord) > 0 ){
      warning(c("'pos' will not be used for display because ",
                "'coord' already exists"))
    }
    if(length(value) == length(x@pos)){
      x@pos <- value
      x@dx <- mean(abs(diff(value)))
    }else{
      stop("length(value) != length(x@depth)")
    }
    x@proc <- c(x@proc, "pos<-")
    return(x)
  }
)

#------------------------------------------------------------------------------#
# FIXME - START
#' Antenna separation
#' 
#' For common-offset GPR data, the antenna separation is constant.
#' For multi-offset GPR data (e.g., CMP, WARR), the antenna separation increase
#' from trace to trace.
#' @name antsep
#' @rdname antsep
#' @export
setMethod("antsep", "GPR", function(x){
  return(x@antsep)
} 
)
#' @name antsep<-
#' @rdname antsep
#' @export
setReplaceMethod(
  f = "antsep",
  signature = "GPR",
  definition = function(x, value){
    if(isCMP(x)){
      if(length(value) != ncol(x)){
        stop("length(value) != ncol(x)")
      }else{
        if(any(diff(value) < 0 )){
          warning("The antenna separations do not increase!",
                  " Increasing values are expected.")
        }
        x@antsep <- value
        x <- trRmDuplicates(x)
      }
    }else{
      # not CMP, not WARR => common-offset
      if(length(value) > 1) warning("Only first element is used!")
      x@antsep <- value[1]
    }
    x@proc <- c(x@proc, "antsep<-")
    return(x)
  }
)


#' Antenna frequency in MHz
#' 
#' Antenna frequency of the antennas in MHz
#' @name antfreq
#' @rdname antfreq
#' @export
setMethod("antfreq", "GPR", function(x){
  return(x@freq)
} 
)
#' @name antfreq<-
#' @rdname antfreq
#' @export
setReplaceMethod(
  f = "antfreq",
  signature = "GPR",
  definition = function(x, value){
    # not CMP, not WARR => common-offset
    if(length(value) > 1) warning("Only first element is used!")
    x@freq <- as.numeric(value[1])
    x@proc <- c(x@proc, "antfreq<-")
    return(x)
  })

#' Survey mode of the GPR data
#' 
#' @name surveymode
#' @rdname surveymode
#' @export
setMethod("surveymode", "GPR", function(x){
  return(x@surveymode)
} 
)

#' @name surveymode<-
#' @rdname surveymode
#' @export
setReplaceMethod(
  f="surveymode",
  signature="GPR",
  definition=function(x, value){
    value <- as.character(value)[1]
    # define a function surveymodeValues() that return a character vector
    if(toupper(value) %in% c("CMP", "REFLECTION", "WARR", "CMPANALYSIS", 
                             "CMP/WARR")){
      x@surveymode <- value
    }else{
      stop("value must be either 'CMP', 'reflection', 'WARR', 'CMPanalysis',
           or 'CMP/WARR'")
    }
    x@proc <- c(x@proc, "surveymode<-")
    return(x)
    })


#' Fiducial markers of the GPR data
#' 
#' @name fid<-
#' @rdname fid
#' @export
setReplaceMethod(
  f = "fid",
  signature = "GPR",
  definition = function(x, value){
    value  <- as.character(value)
    x@fid  <- value
    x@proc <- c(x@proc, "fid<-")
    return(x)
  }
)

# CHECK ME : COMPARE FID<- with ANN<- (!!!!!)
#' @name fid
#' @rdname fid
#' @export
setMethod("fid", "GPR", function(x){
  return(x@fid)
} 
)

#' Values of the GPR data
#' 
#' @name values
#' @rdname values
#' @export
setMethod("values", "GPR", function(x){
  return(x@data)
} 
)

#' @name values<-
#' @rdname values
#' @export
setReplaceMethod(
  f="values",
  signature="GPR",
  definition=function(x,value){
    if(all(dim(value)==dim(x))){
      x@data <- value
      x@proc <- c(x@proc, "values<-")
      return(x)
    }else{
      stop("x [",nrow(x),"x",ncol(x),"] and A [",nrow(value),"x",ncol(value),
           "] should have the same size\n")
    }
  } 
)


#' Processing steps applied to the data
#' 
#' \code{processing} returns all the processing steps applied to the data.
#' 
#' @param x An object of the class GPR.
#' @return A character vector whose elements contain the name of the 
#' processing functions with their arguments applied previously on the
#' GPR data.
#' @examples
#' data(frenkeLine00)
#' A <- dewow(frenkeLine00, type = "Gaussian")
#' proc(A)
#' @name proc
#' @rdname proc
#' @export
setMethod("proc", "GPR", function(x){
  return(x@proc)
} 
)

#' Add a processing step
#' 
#' @name proc
#' @rdname proc
#' @export
setReplaceMethod(
  f = "proc",
  signature = "GPR",
  definition = function(x,value){
    value <- as.character(value)
    x@proc <- c(x@proc, value)
    return(x)
  }
)





#------------------------------------------------------------------------------#
#' Number of traces
#' 
#' Return the number of traces per data
#' @name ntraces
#' @rdname ntraces
#' @export
setMethod("ntraces", "GPRsurvey", function(x){
  return(x@ntraces)
})