

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
# #                        
#                        z = list(c(1.4,  2,  3.5),
# 
#                                 c(  2, 2.7, 4 )) ,
# 
#                        twt = list(c(45, 180, 230),
# 
#                                   c(34, 89, 120)),
# 
#                        pos = c(5.2, 23.5))
# 
# x <- frenkeLine00

interpolateTimeToDepth <- function(x, z = NULL , twt = NULL, pos = NULL, method = "linear", h = 2){
  if(length(z) != length(twt)){
    stop("'z' and 'twt' must have the same length!")
  }
  
  if(!isDepthTime(x)){
    stop("'x' must have be expressed as a function of two-way travel time and not depth!")
  }
  if(is.list(z) && is.list(twt)){
    if(!is.null(pos) && length(pos) == length(z)){
      D <- list()
      for(i in seq_along(pos)){
        D[[i]] <- signal::interp1(x = twt[[i]],
                                  y = z[[i]],
                                  xi = x@depth,
                                  method = method,
                                  extrap = TRUE)
      }
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
      
      dd <- c(unlist(D), rep(0, length(pos(x))))
      twttwt <- c(rep(depth(x),  length(D)), rep(0, length(pos(x))))
      pospos <- c(rep(pos,  sapply(D, length)), pos(x))
      # plot3D::points2D(pospos, twttwt, colvar = dd)
      DEP <- verboseF( MBA::mba.surf(cbind(pospos, twttwt, dd), no.X = para$nx , no.Y = para$ny, n = n, m = m, 
                          extend = TRUE, h = h, b.box = para$bbox), verbose = FALSE)
      DD <- matrix(NA, ncol = length(depth(x)), nrow = length(pos(x)))
      DD[] <- as.vector(DEP$xyz.est$z)
      # plot3D::image2D(DD, DEP$xyz.est$x, DEP$xyz.est$y)
      # 
      # x1@data[] <- t(DD)
      # plot(x1)
      
      D0 <- t(DD)
      
      if(all(dim(D0) == dim(x))){
        dz <- mean(D0[2:nrow(D0),] - D0[1:(nrow(D0)-1), ], na.rm = TRUE)
        zv <- seq(from = min(D0, na.rm = TRUE), to = max(D0, na.rm = TRUE), by = dz)
        xnew <- x
        xnew@data <- x@data[rep(1, length(zv)),]
        for(i in 1:ncol(x)){
          xnew@data[, i] <- signal::interp1(x = D0[, i],
                                         y = x@data[, i],
                                         xi = zv,
                                         method = method,
                                         extrap = FALSE)
        }
        xnew@depth <- zv
        xnew@depthunit <- xnew@posunit
        xnew@dz        <- mean(diff(xnew@depth), na.rm = TRUE)
        return(xnew)
        # plot(xnew)
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