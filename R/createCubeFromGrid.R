
#' Create a GPRcube from a GPR grid (GPRsurvey)
#' 
#' Create a GPRcube from a GPR grid (GPRsurvey)
#' 
#' @param obj    (`GPRsurvey`) An object of the class GPRsurvey.
#' @param dz    (`NULL|numeric[1]`) Vertical resolution (if NULL, the mean 
#'              vertical resolution of the data is taken).
#' @param name (`character[1]`) Name of the GPRcube.
#' @param desc (`character[1]`) Description of the GPRcube.
#' @param track (`logical[1]`) Should the processing step be tracked?
#' @return (`GPRcube`) An object of the class GPRcube.
#' @name createCubeFromGrid
#' @rdname createCubeFromGrid
#' @export
#' @concept processing
setGeneric("createCubeFromGrid", 
           function(obj, dz = NULL, direction = c("x", "y"), name = "", desc = "", track = TRUE)
             standardGeneric("createCubeFromGrid"))

#' @rdname createCubeFromGrid
#' @export
setMethod("createCubeFromGrid", "GPRsurvey", 
          function(obj, dz = NULL, direction = c("x", "y"), name = "", desc = "", track = TRUE){
  
    direction <- match.arg(direction[1], c("x", "y"))
    # x2i <- x2[[1]]
    XYZ <- do.call(rbind, coordinates(obj))
    dx <- mean(diff(sort(unique(XYZ[,1]))))
    dy <- mean(diff(sort(unique(XYZ[,2]))))
    # dz <- mean(diff(sort(unique(XYZ[,3]))))
    
    dz <- mean(obj@zlengths/(obj@nz -1))
    
    # xrg <- range(sapply(obj@coords, function(x)  range(x[,1])))
    xrg <- range(XYZ[,1])
    # yrg <- range(sapply(obj@coords, function(x)  range(x[,2])))
    yrg <- range(XYZ[,2])
    # zrg <- range(sapply(obj@coords, function(x)  range(x[,3])))
    
    vz <- RGPR:::.computeTargetDepths(obj, dz)
    
    # zrg2 <- max(obj@nz)
    
    nx <- diff(xrg)/dx +1
    ny <- diff(yrg)/dy +1
    nz <- length(vz)
    
    U <- array(0, dim = c(nx, ny, nz))
    
    for(i in seq_along(obj)){
      obji <- obj[[i]]
      if(direction == "y"){
        vj <- unique(obji@coord[,1]) / dx + 1
        vi <- (obji@coord[,2] - yrg[1])/ dy + 1
      }else{
        vi <- unique(obji@coord[,2]) / dy + 1
        vj <- (obji@coord[,1] - xrg[1])/ dx + 1
      }
      U[vj, vi, ] <-  t(RGPR:::.interpolateProfile(obji, vz))
      # for(j in 1:ncol(obji)){
      #   vk <- round((zrg[2]  - obji@coord[j,3] ) / dz)
      #   U[vj, vi[j], vk+1:nrow(obji)] <- obji@data[,j]
      # }
    }
    
    # image(U[,,350])
    uu <- as(list(data = U, dx = dx, dy = dy, dz = dz,
                  spunit = obj@spunit, crs = obj@crs,
                  zunit = obj@zunits[1], date = Sys.Date(),
                  name = name, desc = desc), "GPRcube")
    return(uu)
})
