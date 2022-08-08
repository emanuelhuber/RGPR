

# 
# interpolateTimeToDepth(x, z, twt, pos = NULL)
# 
# 
# 
# 1D interpolation:
#   
#   interpolateTimeToDepth(x, z = c( 1.4, 2, 3.5) , twt = c(45, 180, 230), pos = NULL)
# 
# 
# 
# 2D interpolation (2 Bohrungen)
# 
# interpolateTimeToDepth(x,
#                        
#                        z = list(c(1.4,  2,  3.5),
#                                 
#                                 c(  2, 2.7, 4 )) ,
#                        
#                        twt = list(c(45, 180, 230),
#                                   
#                                   c(34, 89, 120)),
#                        
#                        pos = c(5.2, 23.5))



interpolateTimeToDepth <- function(x, z = NULL , twt = NULL, pos = NULL, method = "linear"){
  if(length(z) != length(twt)){
    stop("'z' and 'twt' must have the same length!")
  }
  
  if(!isDepthTime(x)){
    stop("'x' must have be expressed as a function of two-way travel time and not depth!")
  }
  if(is.list(z) && is.list(twt)){
    if(!is.null(pos) && length(pos) == length(z)){
      dx <- mean(diff(pos(x)))
      dy <- mean(diff(depth(x)))
      
      para <- getParaBox(x = sf::st_as_sf(data.frame(x = range(pos(x)), y = range(depth(x))), coords = 1:2), 
                         shp = NULL, 
                         extend = "bbox", 
                         buffer = 0, 
                         dx, dy)
      n <- 1
      m <- 1
      # ratio_x_y <- bbox_dy / bbox_dx
      ratio_x_y <- (para$bbox[4] - para$bbox[3]) / (para$bbox[2] - para$bbox[1])
      if(ratio_x_y < 1){
        m <- round(1/ratio_x_y)
      }else{
        n <- round(ratio_x_y)
      }
      if(m < 1) m <- 1L
      if(n < 1) n <- 1L
      
      zz <- unlist(z)
      twttwt <- unlist(twt)
      pospos <- rep(pos,  sapply(z, length))
      D00 <- MBA::mba.surf(cbind(pospos, twttwt, zz), no.X = para$nx , no.Y = para$ny, n = n, m = m, 
                          extend = TRUE, h = 6, b.box = para$bbox)$xyz.est
      D0 <- D00$z
      plot3D::image2D()
      if(all(dim(D0) == dim(x))){
        dz <- mean(D0[2:nrow(D0),] - D0[1:(nrow(D0)-1), ], na.rm = TRUE)
        zv <- seq(from = min(D0, na.rm = TRUE), to = max(D0, na.rm = TRUE), by = dz)
        for(i in 1:ncol(x)){
          x@data[, i] <- signal::interp1(x = D0[, i],
                                         y = x@data[, i],
                                         xi = zv,
                                         method = method,
                                         extrap = TRUE)
        }
        x@depth <- zv
        x@depthunit <- x@posunit
        x@dz        <- mean(diff(x@depth), na.rm = TRUE)
      }else{
        stop("Problem")
      }
      
    }else{
      stop("'pos' must be a numeric vector of same length as 'z' and 'twt'!")
    }
    
    
  }else{
    z <- as.numeric(z)
    twt <- as.numeric(twt)
    if(is.numeric(z) && is.numeric(twt)){
      d <- signal::interp1(x = twt,
                           y = z,
                           xi = x@depth,
                           method = method,
                           extrap = TRUE)
      x@depth <- d
      x@depthunit <- x@posunit
      x@dz        <- mean(diff(x@depth), na.rm = TRUE)
    }else{
      stop("format of 'z' or 'twt' is not correct!")
    }
  }
  return(x)
}