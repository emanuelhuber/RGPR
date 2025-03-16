#' Delineate structure on GPR data
#'
#' @param x (`GPR`)
#' @param name (`character[1]`) Names of the delineated line
#' @param values (`list`) list of x and y coordinates (optional). If `NULL`, 
#'               `locator()` function is run.
#' @param n (`numeric[1]`) the maximum number of points to locate. Valid values start at 1.
#' @param plotDel (`logical[1]`) If `TRUE`plot delineation.
#' @param ... Additional arguments for `plot`/`line` function
#' @param col (`character[n]`) `n` colors for the `n` delineations
#' @param method (`character[1]`) Interpolation method
#' @name delineate
#' @rdname delineation
#' @export
setGeneric("delineate", function(x, 
                                 name     = NULL,
                                 values   = NULL,
                                 n        = 10000, 
                                 plotDel = NULL, 
                                 ...) 
  standardGeneric("delineate"))

#' @rdname delineation
#' @export
setMethod("delineate", "GPR", function(x, 
                                       name     = NULL,
                                       values   = NULL, 
                                       n        = 10000, 
                                       plotDel = NULL, 
                                       ...){
  # plotDelineations(x)
  if(is.null(values)){
    plot(x, ...)
    if(is.null(plotDel)) plotDel <- list()
    plotDel[["x"]] <- x
    if(length(x@delineations) > 0 ) do.call(plotDelineations, plotDel)
    itp <- locator(type = "l", n = n)
  }else{
    itp <- values
  }
  if(length(itp) > 0){
    if(length(x@coord) == 0){
      x@coord <- matrix(0, nrow = ncol(x), ncol = 3)
      x@coord[,1] <- x@x
    }
    # xvalues <- posLine(x@coord)
    xval <- relPos(x)
    i <- sapply(itp$x, .whichMin, xval)
    j <- sapply(itp$y, .whichMin, x@z)
    # the indices should range from 1 to ncol/nrow
    test <- ( i >= 1 & i <= length(x) & 
                j >= 1 & j <= nrow(x) )
    if(any(!test)) warning("there is a problem")
    # remove duplicated indices
    ijDupl <- duplicated(i) & duplicated(j)
    i <- i[!ijDupl]
    j <- j[!ijDupl]
  }
  if(is.null(name)){
    x@delineations <- c(x@delineations, list(cbind(i, j)))
  }else{
    name <- as.character(name)
    if(length(x@delineations[[name]]) > 0){
      x@delineations[[name]] <- c(x@delineations[[name]], 
                                  list(cbind(i, j)))
    }else{
      x@delineations[[name]] <- list(cbind(i, j))
    }
  }
  return(x)
})  


#' Plot the delineation on a 2D plot
#'
#' @name plotDelineations
#' @rdname delineation
#' @export
setGeneric("plotDelineations", 
           function(x, 
                    method = c("linear", "nearest", "pchip", 
                               "cubic", "spline", "none"), 
                    col = NULL, ...) 
             standardGeneric("plotDelineations"))


#' @rdname delineation
#' @export
setMethod("plotDelineations", "GPR", 
          function(x, 
                   method = c("linear", "nearest", "pchip", 
                              "cubic", "spline", "none"), 
                   col = NULL, ...){
            if(is.null(dev.list())){
              stop("You must first plot the GPR profile with the function \"plot\"!\n")
            }
            if(length(x@delineations) > 0){
              method <- match.arg(method, c("linear", "nearest", "pchip", 
                                            "cubic", "spline", "none"))
              # print(method)
              if(method == "none"){
                xyzrel <- .getXYZrel(x)
                # print("NULL")
              }else{
                xyzrel <- .getXYZrelIntp(x, method)
              }
              nd <- length(xyzrel)
              if(is.null(col))     col <- 1
              if(length(col) <= nd) col <- rep(col, nd)
              for(i in 1:nd){
                lines(xyzrel[[i]][, 4], 
                      xyzrel[[i]][, 5], 
                      col = col[i],
                      ...)
              }
            }else{
              message("There is no delineations. Use 'delineate()' ",
                      "to delineate reflectors/structures on your data.")
            }
          })



# use flattenlist (global.R)
.getXYZrel <- function(x){
  u <- flattenlist(x@delineations)
  x_relPos <- relPos(x)
  if(length(x@coord) == 0){
    x@coord <- matrix(0, nrow = ncol(x), ncol = 3)
    x@coord[, 1] <- x_relPos
  }
  lapply(u, .getXYZrel0, x@coord, x_relPos, x@z)
}
.getXYZrel0 <- function(x, xyz, xrel, zrel){
  xyz <- xyz[x[, "i"], ]
  xyz <- xyz[, c(1, 2, 3, 3, 3)]
  xyz[, 3] <- xyz[, 3] - zrel[x[, "j"]]
  xyz[, 4] <- xrel[x[, "i"]]
  xyz[, 5] <- zrel[x[, "j"]]
  colnames(xyz) <- c("x", "y", "z", "xrel", "zrel")
  return(xyz)
}

# same as above but lines are interpolated to each trace positions
# use flattenlist (global.R)
.getXYZrelIntp <- function(x, method = "linear"){
  u <- flattenlist(x@delineations)
  x_relPos <- relPos(x)
  if(length(x@coord) == 0){
    x@coord <- matrix(0, nrow = ncol(x), ncol = 3)
    x@coord[, 1] <- x_relPos
  }
  lapply(u, .getXYZrel0Intp, x@coord, x_relPos, x@z, method)
}


.getXYZrel0Intp <- function(x, xyz, xrel, zrel, method){
  
  # no interpolation required
  if(nrow(x) == nrow(xyz) && all(diff(x[, "i"]) == 1)){   
    xyz <- x[, c(1, 2, 2, 2, 2, 2)]
    xyz[, 1:2] <- xyz[, 1:2]
    xyz[,   3] <- xyz[, 3] -  zrel[x[,"j"]]
    xyz[,   4] <- xrel[x[, "i"]]
    xyz[,   5] <- zrel[x[,"j"]]
    xyz[,   6] <- x[, "i"]
  }else{   # interpolation
    
    tst <- interpToCoords(i = x[, "i"], u = zrel[x[,"j"]], xy = xyz, method = method)
    
    xyz <- xyz[tst[["i"]], ]
    xyz <- xyz[, c(1, 2, 3, 3, 3, 3)]
    xyz[, 3] <- xyz[, 3] - tst[["u"]]
    xyz[, 4] <- xrel[tst[["i"]]]
    xyz[, 5] <- tst[["u"]]
    xyz[, 6] <- tst[["i"]]
  }
  colnames(xyz) <- c("x", "y", "z", "xrel", "zrel", "i")
  return(xyz)
}

