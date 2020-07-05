
#---------------------------------- DELINEATE ---------------------------------#
# use which.min (global.R)

#' @name delineate
#' @rdname delineation
#' @export
setGeneric("delineate", function(x, 
                                 name     = NULL,
                                 n        = 10000, 
                                 plot_del = NULL, 
                                 ...) 
  standardGeneric("delineate"))

#' Delineate structure on GPR data
#'
#' @name delineate
#' @rdname delineation
#' @export
setMethod("delineate", "GPR", function(x, 
                                       name = NULL, 
                                       n = 10000, 
                                       plot_del = NULL, 
                                       ...){
  plot(x, ...)
  if(is.null(plot_del)) plot_del <- list()
  plot_del[["x"]] <- x
  if(length(x@delineations) > 0 ) do.call(plotDelineations, plot_del)
  # plotDelineations(x)
  itp <- locator(type = "l", n = n)
  if(length(itp) > 0){
    if(length(x@coord) == 0){
      x@coord <- matrix(0, nrow = ncol(x), ncol = 3)
      x@coord[,1] <- x@pos
    }
    # xvalues <- posLine(x@coord)
    xval <- relTrPos(x)
    i <- sapply(itp$x, .whichMin, xval)
    j <- sapply(itp$y, .whichMin, x@depth)
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


#----------------------------------- DELINEATIONS -----------------------------#
#' @name delineations
#' @rdname delineation
#' @export
setGeneric("delineations", function(x, name = NULL) 
  standardGeneric("delineations"))

#' Print the list of delineation of the GPR data
#' 
#' Print and invisible returns the GPR data delineations.
#' @name delineations
#' @rdname delineation
#' @export
setMethod("delineations", "GPR", function(x, name = NULL){
  if(length(x@delineations) > 0){
    message("*** delineated lines ****")
    xyzrel <- .getXYZrel(x)
    if(!is.null(name)){
      xyzrel <- xyzrel[names(xyzrel) %in% name]
    }
    m <- unlist( lapply(xyzrel, .printdelineations) )
    m <- Map(c, paste0(names(m), "\n"), paste0(seq_along(m), m))
    message(unlist(m), appendLF = FALSE)
    message("- - - - - - - - - - -")
    invisible(xyzrel)
  }else{
    message("There is no delineations. Use 'delineate()' ",
            "to delineate reflectors/structures on your data.")
  }
})

# xyz0 <- xyz
.printdelineations <- function(xyz){
  m <- paste0( ". length = ", diff(range(xyz[, 4])), 
               ";  depth = ", round(diff(range(xyz[, 5])), 2),
               ";  ", nrow(xyz), " pts\n")
  return(m)
}


#------------------------------- PLOTDELINEATIONS -----------------------------#

#' @name plotDelineations
#' @rdname delineation
#' @export
setGeneric("plotDelineations", 
           function(x, 
                    method = c("linear", "nearest", "pchip", 
                               "cubic", "spline", "none"), 
                    col = NULL, ...) 
  standardGeneric("plotDelineations"))

#' Plot the delineation on a 2D plot
#'
#' @name plotDelineations
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

#------------------------------- RMDELINEATIONS -------------------------------#

#' @name rmDelineations<-
#' @rdname delineation
#' @export
setGeneric("rmDelineations<-", function(x,value=NULL) 
  standardGeneric("rmDelineations<-"))

#' Remove delineations from the GPR data
#'
#' @name rmDelineations<-
#' @rdname delineation
#' @export
setReplaceMethod("rmDelineations", "GPR", function(x,value=NULL){
  deli <- x@delineations
  n_d <- length(deli)
  if(!is.null(value) && n_d > 0 && value != "all"){
    n_tot <- sum(sapply(deli, .lengthList))
    it <- 0
    value <- n_tot - value + 1
    for(i in n_d:1){
      if(typeof(deli[[i]]) == "list"){
        n_sub_d <- length(deli[[i]])
        for(j in n_sub_d:1){
          it <- it + 1
          if(it %in% value){
            x@delineations[[i]][j] <- NULL
            if(length(x@delineations[[i]])==0 || 
               is.null(unlist(x@delineations[[i]], use.names = FALSE))){
              x@delineations[i] <- NULL
              break
            }
          }
        }
      }else{
        it <- it + 1
        if(it %in% value){
          x@delineations[i] <- NULL
        }
      }
    }
  }else if(n_d <1){
    warning("No delineation to delete\n")
  }else if(is.null(value)){
    stop("You must specified the no of the delineation you want to delete!\n")
  }else if(value == "all"){
    x@delineations <- list()
  }
  return(x)
})


#----------------------------- EXPORTDELINEATIONS -----------------------------#

setGeneric("exportDelineations", function(x, dirpath = "") 
  standardGeneric("exportDelineations"))

#' Export the coordinates of the delineations
#'
#' @name exportDelineations
#' @rdname delineation
#' @export
setMethod("exportDelineations", "GPR", function(x, dirpath = ""){
  if(length(x@delineations) > 0){
    xyzrel <- .getXYZrel(x)
    for(i in seq_along(xyzrel)){
      table_path_name <- paste0(dirpath, name(x), "_del", i, "_", 
                                names(xyzrel[i]), ".txt")
      write.table(xyzrel[[i]], 
                  file = table_path_name, 
                  sep = ";", 
                  row.names = FALSE, 
                  col.names =  TRUE)
    }
  }else{
    message("There is no delineations. Use 'delineate()' ",
            "to delineate reflectors/structures on your data.")
  }
})

#' @export
setMethod("exportDelineations", "GPRsurvey", function(x, dirpath = ""){
  for(i in seq_along(x)){
    exportDelineations(verboseF(x[[i]], verbose = FALSE),  
                       dirpath = dirpath) 
  }
})


#----------------------------- PLOT3DDELINEATIONS -----------------------------#

plotDelineations3D <- function(...){
  stop("Deprecated.",
       "Use 'plot3DDelineation()' instead of 'plotDelineation3D()'.")
}

#' @name plot3DDelineations
#' @rdname delineation
#' @export
setGeneric("plot3DDelineations", 
                function(x, 
                         method = c("linear", "nearest", "pchip", 
                                    "cubic", "spline", "none"), 
                         col = NULL, add = TRUE, ...)
                  standardGeneric("plot3DDelineations"))


#' Plot the delineation on RGL
#'
#' @name plot3DDelineations
#' @rdname delineation
#' @export
setMethod("plot3DDelineations", "GPR", 
          function(x, 
                   method = c("linear", "nearest", "pchip", 
                              "cubic", "spline", "none"), 
                   col = NULL, 
                   add = TRUE, 
                   ...){
  if(length(x@delineations) > 0){
    method <- match.arg(method, c("linear", "nearest", "pchip", 
                                  "cubic", "spline", "none"))
    if(method == "none"){
      xyzrel <- .getXYZrel(x)
    }else{
      xyzrel <- .getXYZrelIntp(x, method)
    }
    n_d <- length(xyzrel)
    if(is.null(col))      col <- 1:n_d
    if(length(col)<=n_d)  col <- rep(col, n_d)
    if(add==FALSE)        rgl::open3d()
    for(i in seq_along(xyzrel)){
      rgl::lines3d(xyzrel[[i]][,2] - x@coordref[2], 
                   xyzrel[[i]][,3] - x@coordref[3], 
                   xyzrel[[i]][,1] - x@coordref[1], 
                   col = col[i], ...)
    }
  }else{
    message("There is no delineations. Use 'delineate()' ",
            "to delineate reflectors/structures on your data.")
  }
})

#' @export
setMethod("plot3DDelineations", "GPRsurvey", 
          function(x, 
                   method = c("linear", "nearest", "pchip", 
                              "cubic", "spline", "none"), 
                   col = NULL, add = TRUE, ...){
            add <- add
            for(i in seq_along(x)){
              plot3DDelineations(x[[i]], method = method, col = col, add = add, ...)
              add <- TRUE
            }  
          })


#---------------------------- IDENTIFYDELINEATIONS ----------------------------#

#' @name identifyDelineation
#' @rdname delineation
#' @export
setGenericVerif("identifyDelineation", function(x, 
                                                method = c("linear", "nearest", "pchip", 
                                                           "cubic", "spline", "none"), 
                                                ...) 
  standardGeneric("identifyDelineation"))

#' Identify the delineation on a 2D plot
#'
#' Works only close to the points !!!
#' @name identifyDelineation
#' @rdname delineation
#' @export
setMethod("identifyDelineation", "GPR", function(x, 
                                                 method = c("linear", "nearest", "pchip", 
                                                            "cubic", "spline", "none"), 
                                                 ...){
  if(is.null(dev.list())){
    stop("You must first plot the GPR profile with the function \"plot\"!\n")
  }
  if(length(x@delineations) > 0){
    method <- match.arg(method, c("linear", "nearest", "pchip", 
                                  "cubic", "spline", "none"))
    if(method == "none"){
      xyzrel <- .getXYZrel(x)
    }else{
      xyzrel <- .getXYZrelIntp(x, method)
    }
    xyzrel_id <- seq_along(xyzrel)
    xzrel <- Map(function(x, y) cbind(x[,4:5], y), xyzrel, xyzrel_id)
    xzrel <- do.call(rbind, xzrel) 
    colnames(xzrel) <- c("xrel", "zrel", "id")
    A <- identify(xzrel, labels = xzrel[, 3], n = 100)
    return(xzrel[A, 3])
  }else{
    message("There is no delineations. Use 'delineate()' ",
            "to delineate reflectors/structures on your data.")
  }
})




#------------------------------ helper functions ------------------------------#

# use flattenlist (global.R)
.getXYZrel <- function(x){
  u <- flattenlist(x@delineations)
  x_relPos <- relTrPos(x)
  if(length(x@coord) == 0){
    x@coord <- matrix(0, nrow = ncol(x), ncol = 3)
    x@coord[, 1] <- x_relPos
  }
  lapply(u, .getXYZrel0, x@coord, x_relPos, x@depth)
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
  x_relPos <- relTrPos(x)
  if(length(x@coord) == 0){
    x@coord <- matrix(0, nrow = ncol(x), ncol = 3)
    x@coord[, 1] <- x_relPos
  }
  lapply(u, .getXYZrel0Intp, x@coord, x_relPos, x@depth, method)
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

.getXYZrel0Intp_old <- function(x, xyz, xrel, zrel, method){
  
  # zpos <- depth(x)
  # xpos <- relTrPos(x)
  x_i <- seq(min(x[,"i"]), max(x[,"i"]), by = 1)
  zrel_j <- signal::interp1(x = xrel[x[,"i"]], y = zrel[x[,"j"]], 
                            xi = xrel[x_i], method = method)
  
  xyz <- xyz[x_i, ]
  xyz <- xyz[, c(1, 2, 3, 3, 3)]
  xyz[, 3] <- xyz[, 3] - zrel_j
  xyz[, 4] <- xrel[x_i]
  xyz[, 5] <- zrel_j
  colnames(xyz) <- c("x", "y", "z", "xrel", "zrel")
  return(xyz)
}

