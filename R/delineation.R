
#---------------------- DELINEATIONS ---------------------#
#' @name delineate
#' @rdname delineation
#' @export
setGenericVerif("delineate", function(x, name = NULL,
                                      type = c("raster", "wiggles"),
                                      addTopo = FALSE, nupspl = NULL, n = 10000, ...) 
  standardGeneric("delineate"))
#' @name rmDelineations<-
#' @rdname delineation
#' @export
setGenericVerif("rmDelineations<-", function(x,value=NULL) 
  standardGeneric("rmDelineations<-"))
#' @name delineations
#' @rdname delineation
#' @export
setGenericVerif("delineations", function(x,sel=NULL,...) 
  standardGeneric("delineations"))
#' @name addDelineation
#' @rdname delineation
#' @export
setGenericVerif("addDelineation", function(x,...) 
  standardGeneric("addDelineation"))
setGenericVerif("showDelineations", function(x,sel=NULL,...) 
  standardGeneric("showDelineations"))
#' @name exportDelineations
#' @rdname delineation
#' @export
setGenericVerif("exportDelineations", function(x, dirpath="") 
  standardGeneric("exportDelineations"))
#' @name plotDelineations3D
#' @rdname delineation
#' @export
setGenericVerif("plotDelineations3D", 
                function(x,sel=NULL,col=NULL,add=TRUE,...)
                  standardGeneric("plotDelineations3D"))
#' @name plotDelineations
#' @rdname delineation
#' @export
setGenericVerif("plotDelineations", function(x,sel=NULL,col=NULL,...) 
  standardGeneric("plotDelineations"))
#' @name identifyDelineation
#' @rdname delineation
#' @export
setGenericVerif("identifyDelineation", function(x,sel=NULL,...) 
  standardGeneric("identifyDelineation"))

#---------------------- DELINEATIONS ---------------------#
#' Delineate structure on GPR data
#'
#' @name delineate
#' @rdname delineation
#' @export
setMethod("delineate", "GPR", 
          function(x,name = NULL, type = c("raster", "wiggles"), 
                   addTopo = FALSE, nupspl = NULL, n = 10000, ...){
            if(is.null(dev.list())){
              stop(paste0("You must first plot the GPR profile",
                          "with the function \"plot\"!\n"))
            }
            xsave <- x
            itp <- locator(type = "l", n = n)
            if(length(itp)>0){
              if(length(x@vel)>0){  
                velo <- x@vel[[1]]
              }else{
                velo <- 0
              }
              if(!is.null(nupspl)){
                x <- upsample(x,n=nupspl)
              }
              topo <- rep(0,length(x))
              type <- match.arg(type, c("raster","wiggles"))
              if(type=="raster"){
                if(addTopo){
                  x <- migration(x)
                }
                # yvalues <- -rev(x@depth) 
                yvalues <- -(x@depth) 
                yvalues <- yvalues + mean(x@time0)
              }else if(type=="wiggles"){
                yvalues <- -rev(x@depth) 
                if(addTopo){
                  topo <- x@coord[,3]
                  topo <- topo - max(topo)
                  yvalues <- yvalues * velo/ 2
                  time_0 <- mean(x@time0)
                  depth_0 <- depthToTime(z = 0, time_0, v = velo, antsep = x@antsep) * 
                    velo/ 2
                  yvalues <- yvalues + depth_0
                }
              }
              if(length(x@coord) == 0){
                x@coord <- matrix(0,nrow=ncol(x),ncol=3)
                x@coord[,1] <- x@pos
              }
              # xvalues <- posLine(x@coord)
              xvalues <- relTrPos(x)
              posxOnPlot <- sapply(itp$x, .whichMin, xvalues)
              posyOnPlot <- sapply(itp$y, .whichMin, yvalues)
              mySel <- posxOnPlot >= 0 & posxOnPlot <= length(x) & 
                posyOnPlot >= 0 & posyOnPlot <= nrow(x)
              posxOnPlot2 <- posxOnPlot[mySel]
              posPts <- posyOnPlot[mySel]
              x_traces <- seq_len(ncol(x))
              # posTrace <- x@traces[posxOnPlot2]
              posTrace <- x_traces[posxOnPlot2]
              xpos <- x@coord[posxOnPlot2,1]
              ypos <- x@coord[posxOnPlot2,2]
              # zpos <- x@coord[posPts,3]
              zpos <- itp$y[mySel]
              if(is.null(name)){
                xsave@delineations <- c(xsave@delineations, 
                                        list(cbind(posTrace,posPts,xpos,ypos,zpos)))
              }else{
                name <- as.character(name)
                if(length(xsave@delineations[[name]])>0){
                  xsave@delineations[[name]] <- c(xsave@delineations[[name]], 
                                                  list(cbind(posTrace,posPts,xpos,ypos,zpos)))
                }else{
                  xsave@delineations[[name]] <- 
                    list(cbind(posTrace,posPts,xpos,ypos,zpos))
                }
              }
            }
            return(xsave)
          }
)

# add "manually" delineation to GPR data
# m - m or m - ns (depends on addTopo = FALSE/TRUE

#' @name addDelineation
#' @rdname delineation
#' @export
setMethod("addDelineation", "GPR", function(x, itp, 
                                            name = NULL, type = c("raster", "wiggles"), addTopo = FALSE, ...){
  if(is.null(dev.list())){
    stop("You must first plot the GPR profile with the function \"plot\"!\n")
  }
  xsave <- x
  # itp <- locator(type="l", n=n)
  topo <- rep(0,length(x))
  type <- match.arg(type, c("raster","wiggles"))
  if(type=="raster"){
    if(addTopo){
      x <- migration(x)
    }
    # yvalues <- -rev(x@depth) 
    yvalues <- -(x@depth) 
    yvalues <- yvalues + mean(x@time0)
  }else if(type=="wiggles"){
    yvalues <- -rev(x@depth) 
    if(addTopo){
      if(length(x@vel)>0){  
        velo <- x@vel[[1]]
      }else{
        velo <- 0
      }
      topo <- x@coord[,3]
      topo <- topo - max(topo)
      yvalues <- yvalues * velo/ 2
      time_0 <- mean(x@time0)
      depth_0 <- depthToTime(z = 0, time_0, v = velo, antsep = x@antsep) * 
        velo/ 2
      yvalues <- yvalues + depth_0
    }
  }
  if(length(x@coord) == 0){
    x@coord <- matrix(0,nrow=ncol(x),ncol=3)
    x@coord[,1] <- x@pos
  }
  #xvalues <- posLine(x@coord)      
  xvalues <- relTrPos(x)      
  posxOnPlot <- sapply(itp$x, .whichMin, xvalues)
  posyOnPlot <- sapply(itp$y, .whichMin, yvalues)
  mySel <- posxOnPlot >= 0 & posxOnPlot <= length(x) & 
    posyOnPlot >= 0 & posyOnPlot <= nrow(x)
  posxOnPlot2 <- posxOnPlot[mySel]
  posPts <- posyOnPlot[mySel]
  x_traces <- seq_len(ncol(x))
  # posTrace <- x@traces[posxOnPlot2]
  posTrace <- x_traces[posxOnPlot2]
  xpos <- x@coord[posxOnPlot2,1]
  ypos <- x@coord[posxOnPlot2,2]
  # zpos <- x@coord[posPts,3]
  zpos <- itp$y[mySel]
  if(is.null(name)){
    xsave@delineations <- c(xsave@delineations, 
                            list(cbind(posTrace,posPts,xpos,ypos,zpos)))
  }else{
    name <- as.character(name)
    if(length(xsave@delineations[[name]])>0){
      xsave@delineations[[name]] <- c(xsave@delineations[[name]], 
                                      list(cbind(posTrace,posPts,xpos,ypos,zpos)))
    }else{
      xsave@delineations[[name]] <- 
        list(cbind(posTrace,posPts,xpos,ypos,zpos))
    }
  }
  return(xsave)
}
)

# Remove manually delineation from the GPR data
#
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
          # itdel <- 0
          if(it %in% value){
            # if(value==it){
            # x@delineations[[i]][[j]] <- NULL
            # j <- j - itdel
            # cat("---- j=", j,"  ,   j-itdel=",j- itdel,"\n")
            x@delineations[[i]][j] <- NULL
            # itdel <- itdel + 1
            if(length(x@delineations[[i]])==0 || 
               is.null(unlist(x@delineations[[i]], use.names = FALSE))){
              x@delineations[i] <- NULL
              # i<-i-1
              break
            }
            # print(x@delineations[[i]])
          }
        }
      }else{
        it <- it + 1
        if(it %in% value){
          # if(value==it){
          x@delineations[i] <- NULL
          # i <- i-1
          # break
          # print(x@delineations)
        }
      }
    }
  }else if(n_d <1){
    warning("No delineation to delete\n")
  }else if(is.null(value)){
    stop("You must specified the no of the delineation you want to delete!\n")
  }else if(value=="all"){
    x@delineations <- list()
  }
  return(x)
}
)

# Show the list of delineation of the GPR data
#
#' @name delineations
#' @rdname delineation
#' @export
setMethod("delineations", "GPR", function(x,sel=NULL,...){
  deli <- x@delineations
  n_d <- length(deli)
  if(n_d >0){
    if(length(x@coord) == 0){
      x@coord <- matrix(0,nrow=ncol(x),ncol=3)
      x@coord[,1] <- x@pos
    }
    # x_dist <- posLine(x@coord)
    x_dist <- relTrPos(x)
    message("*** delineated lines ****")
    it <- 0
    for(i in 1:n_d){
      if(typeof(deli[[i]])=="list"){
        n_sub_d <- length(deli[[i]])
        message(names(deli[i]))
        for(j in 1:n_sub_d){
          it <- it + 1
          x_traces <- seq_len(ncol(x))
          # tracePos <- sapply( deli[[i]][[j]][,1], .which, x@traces)
          tracePos <- sapply( deli[[i]][[j]][,1], .which, x_traces)
          xpos <- x_dist[tracePos]
          zpos <- deli[[i]][[j]][,4]
          message(it, ". length = ", round(diff(range(xpos)), 2), 
                  ";  depth = ", round(diff(range(zpos)), 2),
                  ";  ", length(xpos), " pts")
          # lines(xpos, zpos,col=col[i],...)
        }
      }else{
        it <- it + 1
        x_traces <- seq_len(ncol(x))
        # tracePos <- sapply( deli[[i]][,1], .which, x@traces)
        tracePos <- sapply( deli[[i]][,1], .which, x_traces)
        xpos <- x_dist[tracePos]
        zpos <- deli[[i]][,4]
        message(it, ". length =", round(diff(range(xpos)), 2),
                "; depth =", round(diff(range(zpos)), 2),
                "; number of pts =", length(xpos))
        # lines(xpos, zpos,col=col[i],...)
      }
      message("- - - - - - - - - - -")
    }
  }else{
    message("No lines were delineated!")
  }
}
)

# Export the coordinates of the delineations
#
#' @name exportDelineations
#' @rdname delineation
#' @export
setMethod("exportDelineations", "GPR", function(x, dirpath=""){
  if(length(x@coord) == 0){
    x@coord <- matrix(0,nrow=ncol(x),ncol=3)
    x@coord[,1] <- x@pos
  }
  #x_dist <- posLine(x@coord)
  x_dist <- relTrPos(x)
  deli <- x@delineations
  z0 <- max(coord(x, 3)) 
  it <- 0
  for(i in seq_along(deli)){
    if(typeof(deli[[i]])=="list"){
      n_sub_d <- length(deli[[i]])
      for(j in n_sub_d:1){
        it<-it+1
        x_traces <- seq_len(ncol(x))
        # tracePos <- sapply( deli[[i]][[j]][,1], .which, x@traces)
        tracePos <- sapply( deli[[i]][[j]][,1], .which, x_traces)
        xprofile <- x_dist[tracePos]
        zabs <- z0 + deli[[i]][[j]][,5] 
        # zpos <- deli[[i]][[j]][,5]
        table_path_name <- paste0(dirpath, name(x), "_", it, "_", 
                                  names(deli[i]), ".txt")
        write.table(cbind(deli[[i]][[j]][, c("xpos","ypos","zpos")], zabs, 
                          xprofile),file=table_path_name, sep = ";", 
                    row.names = FALSE, 
                    col.names = c("x","y","zr","z","xprofile"))
      }
    }else{
      it <- it+1
      x_traces <- seq_len(ncol(x))
      # tracePos <- sapply( deli[[i]][,1], .which, x@traces)
      tracePos <- sapply( deli[[i]][,1], .which, x_traces)
      xprofile <- x_dist[tracePos]
      zabs <- z0 + deli[[i]][[j]][,5] 
      table_path_name <- paste0(dirpath, name(x), "_", it, "_", 
                                names(deli[i]), ".txt")
      write.table(cbind(deli[[i]][[j]][, c("xpos","ypos","zpos")], zabs, 
                        xprofile), file = table_path_name, sep = ";", 
                  row.names = FALSE, 
                  col.names =  c("x","y","zr","z","xprofile"))
    }
  }
}
)

# Plot the delineation on RGL
#
#' @name plotDelineations3D
#' @rdname delineation
#' @export
setMethod("plotDelineations3D", "GPR", 
          function(x, sel = NULL, col = NULL, add = TRUE, ...){
            deli <- x@delineations
            n_d <- length(deli)
            if(n_d >0){
              if(is.null(col)){
                col <- 1:n_d
              }
              if(length(col)<=n_d){
                col <- rep(col, n_d)
              }
              if(add==FALSE){
                rgl::open3d()
              }
              for(i in 1:n_d){
                if(typeof(deli[[i]])=="list"){
                  n_sub_d <- length(deli[[i]])
                  for(j in 1:n_sub_d){
                    x_traces <- seq_len(ncol(x))
                    # tracePos <- sapply( deli[[i]][[j]][,1], .which, x@traces)
                    tracePos <- sapply( deli[[i]][[j]][,1], .which, x_traces)
                    xpos <- x@coord[tracePos,1] - x@coordref[1]
                    ypos <- x@coord[tracePos,2] - x@coordref[2]
                    z0 <- max(coord(x,3))   -   x@coordref[3]
                    zpos <- z0 - deli[[i]][[j]][,5] 
                    rgl::lines3d(ypos, zpos, xpos, col = col[i], ...)
                  }
                }else{
                  x_traces <- seq_len(ncol(x))
                  # tracePos <- sapply( deli[[i]][,1], .which, x@traces)
                  tracePos <- sapply( deli[[i]][,1], .which, x_traces)
                  xpos <- x@coord[tracePos,1] - x@coordref[1]
                  ypos <- x@coord[tracePos,2] - x@coordref[2]
                  z0 <- max(coord(x, 3))   -   x@coordref[3]
                  zpos <- z0 - deli[[i]][,5] 
                  rgl::lines3d(ypos, zpos, xpos, col = col[i], ...)
                }
              }
            }
          }
)


#' @export
setMethod("plotDelineations3D", "GPRsurvey", 
          function(x,sel=NULL,col=NULL,add=TRUE,...){
            add<-add
            for(i in seq_along(x)){
              gpr <- readGPR(x@filepaths[[i]])
              if(length(x@coords[[gpr@name]])>0){
                gpr@coord <- x@coords[[gpr@name]]
                # cat(x@coordref,"\n")
                gpr@coordref <- x@coordref
              }
              if(length(coord(gpr))==0){
                message(gpr@name, ": no coordinates, I cannot plot",
                        " this line!!")
              }else if(length(gpr@delineations) == 0){
                message(gpr@name, ": no delineations for this line!!")
              }else{
                plotDelineations3D(gpr,sel=sel,col=col,add=add,...)
              }
              add <- TRUE
            }  
          }
)


# Plot the delineation on a 2D plot
#
#' @name plotDelineations
#' @rdname delineation
#' @export
setMethod("plotDelineations", "GPR", function(x,sel=NULL,col=NULL,...){
  if(is.null(dev.list())){
    stop("You must first plot the GPR profile with the function \"plot\"!\n")
  }
  deli <- x@delineations
  n_d <- length(deli)
  if(n_d >0){
    if(length(x@coord) == 0){
      x@coord <- matrix(0,nrow=ncol(x),ncol=3)
      x@coord[,1] <- x@pos
    }
    #x_dist <- posLine(x@coord)
    x_dist <- relTrPos(x)
    if(is.null(col)){
      col <- 1:n_d
    }
    if(length(col)<=n_d){
      col <- rep(col, n_d)
    }
    for(i in 1:n_d){
      if(typeof(deli[[i]])=="list"){
        n_sub_d <- length(deli[[i]])
        for(j in 1:n_sub_d){
          x_traces <- seq_len(ncol(x))
          # tracePos <- sapply( deli[[i]][[j]][,1], .which, x@traces)
          tracePos <- sapply( deli[[i]][[j]][,1], .which, x_traces)
          xpos <- x_dist[tracePos]
          zpos <- deli[[i]][[j]][,5]
          lines(xpos, zpos,col=col[i],...)
        }
      }else{
        x_traces <- seq_len(ncol(x))
        # tracePos <- sapply( deli[[i]][,1], .which, x@traces)
        tracePos <- sapply( deli[[i]][,1], .which, x_traces)
        xpos <- x_dist[tracePos]
        zpos <- deli[[i]][,5]
        lines(xpos, zpos,col=col[i],...)
      }
    }
  }else{
    message("No lines were delineated!")
  }
}
)

# Identify the delineation on a 2D plot
#
#' @name identifyDelineation
#' @rdname delineation
#' @export
setMethod("identifyDelineation", "GPR", function(x,sel=NULL,...){
  if(is.null(dev.list())){
    stop("You must first plot the GPR profile with the function \"plot\"!\n")
  }
  XY <- list()
  deli <- x@delineations
  n_d <- length(deli)
  it <- 0
  if(n_d >0){
    if(length(x@coord) == 0){
      x@coord <- matrix(0,nrow=ncol(x),ncol=3)
      x@coord[,1] <- x@pos
    }
    #x_dist <- posLine(x@coord)
    x_dist <- relTrPos(x)
    for(i in 1:n_d){
      if(typeof(deli[[i]])=="list"){
        n_sub_d <- length(deli[[i]])
        for(j in 1:n_sub_d){
          x_traces <- seq_len(ncol(x))
          # tracePos <- sapply( deli[[i]][[j]][,1], .which, x@traces)
          tracePos <- sapply( deli[[i]][[j]][,1], .which, x_traces)
          xpos <- x_dist[tracePos]
          zpos <- deli[[i]][[j]][,5]
          it <- it + 1
          XY[[it]] <- cbind(xpos,zpos,rep(it,length(xpos)))
          # it <- it + 1
          # lines(xpos, zpos,col=col[i],...)
        }
      }else{
        x_traces <- seq_len(ncol(x))
        # tracePos <- sapply( deli[[i]][,1], .which, x@traces)
        tracePos <- sapply( deli[[i]][,1], .which, x_traces)
        xpos <- x_dist[tracePos]
        zpos <- deli[[i]][,5]
        it <- it + 1
        XY[[it]] <- cbind(xpos,zpos,rep(it,length(xpos)))
        # lines(xpos, zpos,col=col[i],...)
      }
    }
    XY <- do.call(rbind,XY)
    A<-identify(XY, labels=XY[,3])
    return(XY[A,3])
  }else{
    message("No lines were delineated!")
  }
}
)


#' @export
setMethod("exportDelineations", "GPRsurvey", function(x, dirpath=""){
  for(i in seq_along(x)){
    exportDelineations(verboseF(getGPR(x, id = i), verbose = FALSE),  
                       dirpath = dirpath) 
  }
})

