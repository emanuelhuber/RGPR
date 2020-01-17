

setGenericVerif("plot3DRGL", 
                function(x, addTopo = FALSE, clip = NULL, normalize = NULL, 
                         nupspl = NULL, add = TRUE, xlim = NULL, ylim = NULL, 
                         zlim = NULL,...) 
                  standardGeneric("plot3DRGL"))


#' Three-dimensional plot of the GPR data with Open-GL
#'
#' @name plot3DRGL
#' @rdname plot3DRGL
#' @export
setMethod("plot3DRGL", "GPR", 
          function(x, addTopo = FALSE, clip = NULL, normalize = NULL, 
                   nupspl = NULL, add = TRUE, xlim = NULL, ylim = NULL, 
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
