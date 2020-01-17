
#' Class GPRset
#' 
#' @name GPRset-class
#' @rdname GPRset-class
#' @export
setClass(
  Class = "GPRset",
  contains = "GPR",
  slots = c(
    setnames  = "character",   # names of the sets
    sets      = "numeric",
    setunit   = "character",
    formula     = "expression", # or "function" GPR = a*GPR1 + b*GPR2 + c*GPR3 + ....
    extraData   = "list"
  )
)


###--- Coercion from GPR to GPRset ...
setAs(from = "GPR", to = "GPRset", def = function(from) as.GPRset.GPR(from))
  
  # setAs(from = "matrix", to = "GPR", def = function (from) as.GPR.matrix(from))

#' Coercion from matrix to GPR
#'
#' @name as.GPRset.GPR
#' @rdname GPRcoercion
#' @export
as.GPRset.GPR <- function (x, ...){
  new("GPRset", 
      setnames  = x@name,
      sets      = 1,
      version     = "0.2",
      data        = array(x@data, dim = c(dim(x), 1)),
      traces      = x@traces,           # x$dt1$traces
      pos         = x@pos,    # x$dt1$position  of the traces
      depth       = x@depth,
      time0       = x@time0,       # x$dt1$time0
      time        = x@time,        # x$dt1$time
      proc        = x@proc,          # processing steps
      vel         = x@vel,              # m/ns
      name        = x@name,
      description = x@description,
      filepath    = x@filepath,
      dz          = x@dz, 
      dx          = x@dx, 
      depthunit   = x@depthunit,
      posunit     = x@posunit,
      freq        = x@freq, 
      antsep      = x@antsep, 
      surveymode  = x@surveymode,
      date        = x@date,
      crs         = x@crs,
      hd          = x@hd                   # header
  )
  } 

# Print methods
# setMethod("print", "GPR", function(x) print.GPR(x))
# > 1. helper function:
.GPRset.print   <-  function(x, digits=5){
  topaste <- c(paste("***","Class GPRset", "***\n"))
  topaste <- c(topaste,   paste0("name        = ", x@name, "\n"))
  topaste <- c(topaste,   paste0("n sets    = ", dim(x)[3], "\n"))
  if(length(x@filepath) > 0){
    topaste <- c(topaste, paste0("filepath    = ", x@filepath, "\n"))
  }
  nbfid <- sum(trimStr(x@fid)!= "")
  if(nbfid > 0){
    topaste <- c(topaste, paste0(nbfid, " fiducial(s)\n"))
  }
  if(length(x@description) > 0){
    topaste <- c(topaste, paste0("description = ", x@description, "\n"))
  }
  if(length(x@date) > 0){
    topaste <- c(topaste, paste0("survey date = ", x@date,"\n"))
  }
  topaste <- c(topaste, paste0(x@surveymode,", ",x@freq, " MHz, ", 
                               "Window length = ",(nrow(x@data)-1)*x@dz, " ", x@depthunit,
                               ", dz = ",x@dz, " ", x@depthunit, "\n"))
  topaste <- c(topaste, paste0(ncol(x@data), " traces, ", 
                               diff(range(x@pos))," ",x@posunit,"\n"))
  if(length(x@proc)>0){
    topaste <- c(topaste, paste("> PROCESSING\n"))
    for(i in seq_along(x@proc)){
      topaste <- c(topaste, paste0("  ",i,". ", x@proc[i],"\n"))
    }      
  }
  topaste <- c(topaste, paste("****************\n"))
  return(topaste)      
}    

# #' @rdname show
#' Print GPR
#'
#' @method print GPRset 
#' @name print
#' @rdname show
# > 2. S3 function:
print.GPRset <- function(x, ...){
  jj <- .GPRset.print(x, ...)
  cat(jj)
  return(invisible(jj))
}
# > 3. And finally a call to setMethod():
# #' @rdname show
#' Show some information on the GPR object
#'
#' Identical to print().
#' @name show
#' @aliases show-method
setMethod("show", "GPRset", function(object){
  print.GPRset(object)
}) 

# "["
#' extract parts of GPRset
#'
#' Return an object of class GPR slice
#' @name GPRset-subset
#' @docType methods
#' @rdname GPRset-subset
setMethod(
  f = "[",
  signature = "GPRset",
  definition = function(x, i, j, k, drop = TRUE){
    if(missing(i) || length(i) == 0){
      i <- 1:dim(x@data)[1]
    } 
    if(missing(j) || length(j) == 0){
      j <- 1:dim(x@data)[2]
    }
    # dots <- list(...)
    # if(length(dots) > 0){
    #   k <- as.integer(dots[[1]])
    # }
    # print(dots)
    if(missing(k) || length(k) == 0){
      k <- 1:dim(x@data)[3]
    }
    # print(i)
    # print(j)
    # print(k)
    # extract set k
    if(length(k) == 1){
      # print("case 1")
      y <- new("GPR",
               version     = "0.2",
               data        = x@data[i, j, k],
               traces      = x@traces[j],           # x$dt1$traces
               pos         = x@pos[j],    # x$dt1$position  of the traces
               depth       = x@depth[i],
               time0       = x@time0[j],       # x$dt1$time0
               time        = x@time[j],        # x$dt1$time
               proc        = x@proc,          # processing steps
               vel         = x@vel,              # m/ns
               name        = paste0(x@name, " - ", x@setnames[k]),
               description = x@description,
               filepath    = x@filepath,
               dz          = x@dz, 
               dx          = x@dx, 
               depthunit   = x@depthunit,
               posunit     = x@posunit,
               freq        = x@freq, 
               antsep      = x@antsep, 
               surveymode  = x@surveymode,
               date        = x@date,
               crs         = x@crs,
               hd          = x@hd      
      )
      # extract a depth slice of GPR-set 
      # depth (y) is now set values
    }else if(length(i) == 1){
      # print("case 2")
      y <- new("GPR",
               version     = "0.2",
               data        = t(x@data[i, j, k]),
               traces      = x@traces[j],           # x$dt1$traces
               pos         = x@pos[j],    # x$dt1$position  of the traces
               posunit     = x@posunit,
               dx          = x@dx, 
               depth       = x@sets[k],
               depthunit   = x@setunit,
               dz          = mean(diff(x@sets[k])), 
               time0       = rep(0, length(j)),       # x$dt1$time0
               time        = x@time[j],        # x$dt1$time
               proc        = x@proc,          # processing steps
               vel         = x@vel,              # m/ns
               name        = paste0(x@name, " @", x@depth[i], " ", x@depthunit),
               description = x@description,
               filepath    = x@filepath,
               freq        = x@freq, 
               antsep      = x@antsep, 
               surveymode  = x@surveymode,
               date        = x@date,
               crs         = x@crs,
               hd          = x@hd      
      )
    # extract slice vertical for a trace j
    }else if( length(j) == 1){
      # print("case 3")
      y <- new("GPR",
               version     = "0.2",
               data        = x@data[i,j,k],
               pos         = x@sets[k],
               posunit     = x@setunit,
               dx          = mean(diff(x@sets[k])), 
               traces      = seq_along(k),
               time0       = rep(x@time0[j], length(k)),
               time        = rep(x@time[j], length(k)),
               depth       = x@depth[i],
               depthunit   = x@depthunit,
               dz          = x@dz, 
               proc        = x@proc,
               vel         = x@vel,
               name        = paste0(x@name, " @", x@pos[j], " ", x@posunit),
               description = x@description,
               filepath    = x@filepath,
               freq        = x@freq, 
               antsep      = x@antsep, 
               surveymode  = x@surveymode,
               date        = x@date,
               crs         = x@crs,
               hd          = x@hd      
      )
      # extract sub-GPRsetcube
    }else{
      x@data   <- x@data[i,j,k]
      x@traces <- x@traces[j]
      x@pos    <- x@pos[j]
      x@depth  <- x@depth[i]
      x@time0  <- x@time0[j]
      x@time   <- x@time[j]
      return(x)
    }
    
    return(y)
  }
)


#' @method plot GPRset 
#' @name plot
#' @rdname plot
#' @export
#Default 1D: "black". 2D: \code{palGPR(n = 101)}
# ##' @param y \code{NULL,} not used
# options: type=c(raster,wiggles), addTopo, clip, normalize
plot.GPRset <- function(x, 
                     #y = NULL, 
                     add = FALSE, 
                     relTime0 = FALSE,
                     note = NULL, 
                     addFid = TRUE,
                     addAnn = TRUE,
                     addTime0 = TRUE,
                     addDepth0 = TRUE,
                     addAmpl0 = TRUE,
                     addTopo = FALSE,
                     clip = NULL,
                     ratio = 1,
                     barscale = TRUE, 
                     wsize = 1,   # wiggles
                     wside = 1,   # wiggles
                     pdfName = NULL,
                     ...){
  
}
