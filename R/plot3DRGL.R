

setGenericVerif("plot3DRGL", 
                function(x, addTopo = FALSE, clip = NULL, normalize = NULL, 
                         nupspl = NULL, add = FALSE, xlim = NULL, ylim = NULL, 
                         zlim = NULL,...) 
                  standardGeneric("plot3DRGL"))


#' Three-dimensional plot of the GPR data with Open-GL
#'
#' @name plot3DRGL
#' @rdname plot3DRGL
#' @export
setMethod("plot3DRGL", "GPR", 
          function(x, addTopo = FALSE, clip = NULL, normalize = NULL, 
                   nupspl = NULL, add = FALSE, xlim = NULL, ylim = NULL, 
                   zlim = NULL, ...){
            if(length(x@vel) > 0){  
              velo <- x@vel[[1]]
            }else{
              velo <- 0
            }
            xsel <- rep(TRUE,length(x))
            if(!is.null(xlim)){
              xlim <- sort(xlim)
              xsel <- coord(x, 1) >= xlim[1] & coord(x, 1) <= xlim[2]
            }
            ysel <- rep(TRUE,length(x))
            if(!is.null(ylim)){
              ylim <- sort(ylim)
              ysel <- coord(x, 2) >= ylim[1] & coord(x, 2) <= ylim[2]
              cat(ylim,"  range = ", range(coord(x, 2)),"\n")
            }
            xysel <- xsel & ysel
            if(sum(xysel) <= 2){
              return(NULL)
            }
            x <- x[,xysel]
            if(!is.null(nupspl)){
              cat("upsample...")
              x <- upsample(x, n = nupspl)
            }
            if(!is.null(normalize)){
              x@data <- normalize(x@data, type = normalize)
            }
            # warning("First upsample then addTopo. 
            # Problem: interpolate also coord!!!")
            if(!is.null(clip) && is.numeric(clip)){
              if(length(clip)>1){
                x@data <- .clip(x@data, clip[2], clip[1])
              }else if(length(clip) == 1){
                x@data <- .clip(x@data, clip[1])
              }
            }else if(is.null(clip)){
              # clip below the 0.01-quantile and above the 0.99-quantile
              x@data <- .clip(x@data, quantile(as.vector(x@data), 0.99, na.rm = TRUE),
                              quantile(as.vector(x@data), 0.01, na.rm = TRUE))
            }
            if(length(x@coordref)!=3 ){
              refCoord <- apply(coord(x),2,min)
            }else{
              refCoord <-x@coordref
            }
            z0 <- coord(x, 3) - refCoord[3]
            if(addTopo){
              x <- migration(x)
              z0 <- rep(max(coord(x, 3)), length(x)) - refCoord[3]
            }
            cat(refCoord,max(coord(x, 3)),"\n")
            A <-as.matrix(x)
            # cat(refCoord,"\n")
            xpos <- coord(x, 1) - refCoord[1]
            ypos <- coord(x, 2) - refCoord[2]
            zpos <- x@depth
            if(add==FALSE){
              # rgl.open()
              rgl::open3d()
            }else{
              rgl::.check3d()
            }
            .plot3DRGL(A, xpos, ypos, zpos, z0, ...)
          }
)


#' @export
setMethod("plot3DRGL", "GPRsurvey", 
          function(x, addTopo = FALSE, clip = NULL, normalize = NULL, 
                   nupspl=NULL, add = TRUE, xlim = NULL, ylim= NULL, 
                   zlim = NULL, ...){
            add <- add
            for(i in seq_along(x)){
              cat("***", i , "***\n")
              gpr <- readGPR(x@filepaths[[i]])
              if(length(x@coords[[gpr@name]])>0){
                gpr@coord <- x@coords[[gpr@name]]
                # cat(x@coordref,"\n")
                gpr@coordref <- x@coordref
              }
              if(length(coord(gpr))==0){
                message(gpr@name, ": no coordinates, I cannot plot",
                        " this line!!")
              }else{
                plot3DRGL(gpr, addTopo = addTopo, clip = clip, normalize = normalize, 
                          nupspl = nupspl, add = add, xlim = xlim, ylim = ylim, 
                          zlim = zlim, ...)
              }
              add <- TRUE
            }  
          }
)



.plot3DRGL <- function(A, x, y, z, z0, col = palGPR(n = 101), 
                       back = "fill", smooth = TRUE, lit = FALSE,
                       lwd = 0, empty = FALSE, ...){
  nr = nrow(A)
  nc = ncol(A)
  if(empty == TRUE){
    X <- matrix( x, ncol = nc, nrow = 2, byrow = TRUE)
    Y <- matrix( y, ncol = nc, nrow = 2, byrow = TRUE)
    Z <- matrix(z0, ncol = nc, nrow = 2, byrow = TRUE) - 
      matrix(z[c(1, nr)], ncol = nc, nrow = 2, byrow = FALSE)
    colA <- col[1]
    if(!is.null(list(...)$alpha) && (list(...)$alpha==0 || is.null(col))){
      
    }else{
      # rgl::rgl.surface(Y, X, Z, color = colA, back = back, 
      rgl::rgl.surface(X, Z, Y, color = colA, back = back, 
                       smooth = smooth, lit = lit, lwd = lwd, ...) 
    }
    # rgl::lines3d(y, z0, x, col = "black", alpha = 1, lwd = lwd)   
    # rgl::lines3d(y, (z0 - z[length(z)]), x, col = "black", alpha = 1, lwd = lwd)   
    # rgl::lines3d(rep(y[1], 2), (z0[1] - z), rep(x[1], 2), col = "black", 
    #              alpha = 1, lwd = lwd)
    # rgl::lines3d(rep(y[length(y)], 2), (z0[length(z0)] - z), rep(x[length(x)], 2),
    #              col = "black", alpha = 1, lwd = lwd)   
    rgl::lines3d(x, z0, y, col = "black", alpha = 1, lwd = lwd)   
    rgl::lines3d(x, (z0 - z[length(z)]), y, col = "black", alpha = 1, lwd = lwd)   
    rgl::lines3d(rep(x[1], 2), (z0[1] - z), rep(y[1], 2), col = "black", 
                 alpha = 1, lwd = lwd)
    rgl::lines3d(rep(x[length(y)], 2), (z0[length(z0)] - z), rep(y[length(x)], 2),
                 col = "black", alpha = 1, lwd = lwd)   
    
  }else{
    X <- matrix( x, ncol = nc, nrow = nr, byrow = TRUE)
    Y <- matrix( y, ncol = nc, nrow = nr, byrow = TRUE)
    Z <- matrix(z0, ncol = nc, nrow = nr, byrow = TRUE) - 
      matrix(z, ncol = nc, nrow = nr, byrow = FALSE)
    A = (A - min(A, na.rm =TRUE))/(max(A, na.rm =TRUE) - min(A, na.rm =TRUE))
    # assign colors to heights for each point 
    colA <- col[A * (length(col) - 1) + 1] 
    # rgl::rgl.surface(Y, X, Z, color = colA, back = back, smooth = smooth, 
    rgl::rgl.surface(X, Z, Y, color = colA, back = back, smooth = smooth,
                     lit = lit, lwd = lwd,...) 
  }
}

# .plot3DSlice <- function(XYZ,slice=c("x","y","z"),section=1,col=palGPR(n=101), 
#                           sampling = c(0.25,0.25,0.04),rmStripes = TRUE){
#   # k=100
#   # j=25
#   # i=40
#   # col <- tim.colors(101) # height color lookup table
#   slice = match.arg(slice)
#   if(length(slice)>1){
#     slice = slice[1]
#   }
#   
#   dimXYZ = dim(XYZ)
#   vz = seq(0,dimXYZ[3]-1,by=1)*sampling[3]  # dtime / 2 * v
#   vx = seq(0,dimXYZ[1]-1,by=1)*sampling[1]
#   vy = seq(0,dimXYZ[2]-1,by=1)*sampling[2]
#   if(rgl::rgl.cur()==0){  # si la fenêtre rgl est ouverte, on plot dedans...
#     rgl::rgl.open()
#     rgl::rgl.bg( color=c("white"))
#   }
#   i = section
#   j=i
#   k=i
#   if(slice=="x"){
#     if(rmStripes == TRUE){ 
#       Xside = normalizeGPR(removeStripes(t(XYZ[,j,])))
#     }else{  
#       Xside = normalizeGPR((t(XYZ[,j,])))  
#     }
#     
#     Xside_x = matrix(vx,nrow=dimXYZ[3],ncol=dimXYZ[1],byrow=TRUE)
#     Xside_y = matrix( vy[j],nrow=dimXYZ[3],ncol=dimXYZ[1],byrow=TRUE)
#     Xside_z = matrix( max(vz)-vz,nrow=dimXYZ[3],ncol=dimXYZ[1],byrow=FALSE)
# 
#     CCX = (Xside-min(Xside))/(max(Xside)-min(Xside))
#     ClimX <- range(CCX)
#     ClenX <- ClimX[2] - ClimX[1] + 1
#     # col <- tim.colors(101) # height color lookup table
#     #col = palette(gray(0:101 / 101))
#     colCX <- col[ (CCX)*100+1 ] 
#     
#     surface3d(Xside_x, Xside_z, Xside_y, col= setCol(Xside), lit=FALSE,
#             front="fill",back="fill")#, alpha=0.5)
#   }else if(slice=="z"){
#     if(rmStripes == TRUE){ Zside = (removeStripes(t(XYZ[,,k])))
#     }else{  Zside = ((t(XYZ[,,k])))  }
#     
#     Zside_x = matrix(vx,nrow=dimXYZ[2],ncol=dimXYZ[1],byrow=TRUE)
#     Zside_y = matrix( vy,nrow=dimXYZ[2],ncol=dimXYZ[1],byrow=FALSE)
#     Zside_z = matrix(max(vz) - vz[k],nrow=dimXYZ[2],ncol=dimXYZ[1],byrow=FALSE)
# 
#     CCZ = (Zside-min(Zside))/(max(Zside)-min(Zside))
#     ClimZ <- range(CCZ)
#     ClenZ <- ClimZ[2] - ClimZ[1] + 1
#     #col = palette(gray(0:101 / 101))
#     colCZ <- col[ (CCZ)*100+1 ]
#     
#     surface3d(Zside_x, Zside_z, Zside_y, col= setCol(Zside), lit=FALSE,
#               front="fill",back="fill")#, alpha=0.5)
#   }else if(slice=="y"){
#     if(rmStripes == TRUE){ Yside = normalizeGPR(removeStripes(t(XYZ[i,,])))
#     }else{  Yside = normalizeGPR((t(XYZ[i,,])))  }
#     
#     Yside_x = matrix(vx[i],nrow=dimXYZ[3],ncol=dimXYZ[2],byrow=TRUE)
#     Yside_y = matrix( vy,nrow=dimXYZ[3],ncol=dimXYZ[2],byrow=TRUE)
#     Yside_z = matrix( max(vz)-vz,nrow=dimXYZ[3],ncol=dimXYZ[2],byrow=FALSE)
#     
# #     CCY = (Yside-min(Yside))/(max(Yside)-min(Yside))
# #     ClimY <- range(CCY)
# #     ClenY <- ClimY[2] - ClimY[1] + 1
# #     colCY <- col[ (CCY)*100+1 ] 
#     colCY <- colFromPal(Yside , col = col )
# 
#     surface3d(Yside_x, Yside_z, Yside_y, col= setCol(Yside), lit=FALSE,
#               front="fill",back="fill")#, alpha=0.5)
#   }
# }
