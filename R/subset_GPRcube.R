
#' Extract and replace parts of a GPRcube object
#' 
#' Extract parts of a GPR object
#' @param x [class \code{GPRcube}]
#' @param i [\code{integer}] Indices specifying elements to extract or replace.
#' @param j [\code{integer}] Indices specifying elements to extract or replace.
#' @param k [\code{integer}] Indices specifying elements to extract or replace.
# @param ... Not used.
#' @param drop Not used.
# #' @param value [\code{numeric}] Value to set.
#' @return [\code{GPR}|\code{numeric}] Returns a numeric vector
#'        only if \code{x[]}.
#' @aliases [,GPRcube-method
#' @rdname subset-GPRcube
#' @export
setMethod(
  f = "[",
  signature = "GPRcube",
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
    vx <- x@center[1] + seq(0, by = x@dx, length.out = dim(x@data)[1])
    vy <- x@center[2] + seq(0, by = x@dy, length.out = dim(x@data)[2])
    vz <- x@center[3] + seq(0, by = x@dz, length.out = dim(x@data)[3])
    # extract slice k
    if(length(k) == 1){
      new_center <- sapply(list(vx[i], vy[j], vz[k]), min)
      y <- new("GPRslice",
               # version      = "0.1",
               # date         = x@date,  
               # freq         = x@freq,
               # x            = x@x[i],
               # y            = x@y[j],
               # data         = x@data[i, j, k, drop = TRUE],
               # coord        = x@coord,
               # posunit      = x@posunit,
               # crs          = x@crs,
               # depth        = x@depth[k],
               # depthunit    = x@depthunit,
               
               #---------- GPRvirtual ------------------#
               version      = "0.3",  # class version
               name         = x@name,
               path         = x@path,
               desc         = x@desc,
               mode         = x@mode,
               date         = x@date,
               freq         = x@freq,
               
               data         = x@data[i, j, k, drop = TRUE],
               dunit        = x@dunit,
               dlab         = x@dlab,
               
               spunit       = x@spunit,
               crs          = x@crs,
               # ? coordref = "numeric",    # coordinates references or "center" or "centroid"
               
               xunit        =  x@xunit,
               xlab         =  x@xlab,
               
               zunit        =  x@zunit,
               zlab         =  x@zlab,
               
               vel          =  x@vel,
               
               proc         =  x@proc,
               delineations =  x@delineations,
               md           =  x@md,
               
               #---------- GPRcube ------------------#
               # x     = x@x[i],
               # y     = x@y[j],
               # z     = x@z[k],
               dx     = x@dx,
               dy     = x@dy,
               dz     = x@dz,
               ylab   = x@ylab,  # set names, length = 1|p
               
               center = new_center,    # coordinates grid corner bottom left (0, 0, 0)
               rot    = x@rot,
               #---------- GPRslice ------------------#
               z = vz[k]
      )
      # extract GPR alons x or y axis
    }else if(length(i) == 1 || length(j) == 1){
      u <- which(c(length(i), length(j)) == 1)[1]
      if(u == 1){
        # dx <- mean(abs(diff(x@y)))
        # xpos <- x@y[j]
        xpos <- vy[j]
      }else{
        # dx <- x@dx #mean(abs(diff(x@x)))
        # xpos <- x@x[i]
        xpos <- vx[i]
      }
      xdata <- x@data[i, j, k]
      if(is.null(dim(xdata))){
        n <- 1L
        dim(xdata) <- c(length(xdata), 1)
      }else{
        xdata <- t(xdata)
        n <- ncol(xdata)
      }
      y <- new("GPR",  
              #--- class GPRvirtual
              version      = "0.3",  
              name         = x@name,
              path         = x@path,
              desc         = x@desc,
              mode         = x@mode,
              date         = x@date,
              freq         = x@freq, 
              
              data         = xdata,     
              dunit        = x@dunit,  
              dlab         = x@dlab, 
              
              spunit       = x@spunit,  
              crs          = x@crs,  
              #crs          = ",  
              
              xunit        = x@xunit,  
              xlab         = x@xlab,
              
              zunit        = x@zunit,  
              zlab         = x@zlab,
              
              vel          = ifelse(is.null(x@vel) || length(x@vel) == 0, list(), x@vel),   
              
              # proc         = "list",
              # delineations = "list",
              md           = x@md,  
              
              #--- class GPR
              z0           = rep(0, n),    
              time         = numeric(0),    
              antsep       = 0,    
              markers      = rep("", n), 
              # ann          = "character", 
              
              # coord        = coord,      # FIXME!
              # rec          = coord_rec,     
              # trans        = coord_trans,     
              
              x            = xpos,    
              z            = vz[k]  #x@z[k]
      )
     
    }else{
      y <- new("GPRcube",
           #----------------- GPRvirtual --------------------------------------#
               version      = x@version,
               name         = x@name,
               path         = x@path,
               desc         = x@desc,
               mode         = x@mode,
               date         = x@date,  
               freq         = x@freq,
               
               data         = x@data[i, j, k, drop = FALSE],
               dunit         = x@dunit,
               dlab         = x@dlab,
               
               spunit      = x@spunit,
               crs          = x@crs,

               xunit            = x@xunit,
               xlab            = x@xlab,
               
               zunit        = x@zunit,
               zlab        = x@zlab,
 
           #----------------- GPRcube -----------------------------------------#
                dx     = x@dx,   # xpos,
                dy     = x@dy,   # ypos,
                dz     = x@dz,   # SXY$vz,
                ylab   = x@ylab,   #,  # set names, length = 1|p
                
                center = c(vx[i][1], vy[j][1], vz[k][1]),
                rot    = x@rot
      )

      
      
     
     
    }
    
    return(y)
  }
)
