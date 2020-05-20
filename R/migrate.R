
#--------------------- 'migration()' = DEPRECATED -----------------------------#
#' @name migration
#' @rdname migrate
#' @export
setGenericVerif("migration", function(x, type = c("static", "kirchhoff"), ...) 
  standardGeneric("migration"))


#' Deprecated
#'
#' @name migration
#' @rdname migrate
#' @export
setMethod("migration", "GPR", function(x, type = c("static", "kirchhoff"), ...){
  message("Soon deprecated. Use 'migrate()' instead of 'migration()'.")
  migrate(x, type = type, ...)
})

#------------------------------------------------------------------------------#

#' @name migrate
#' @rdname migrate
#' @export
setGenericVerif("migrate", function(x, type = c("static", "kirchhoff"), ...) 
  standardGeneric("migrate"))


# max_depth = to which depth should the migration be performed
# dz = vertical resolution of the migrated data
# fdo = dominant frequency of the GPR signal

# for static time-to-depth migration 
# dz = depth resolution for the time to depth conversion. If dz = NULL, then
#      dz is set equal to the smallest depth resolution computed from x_depth.
# d_max = maximum depth for the time to depth conversion. If d_max = NULL, then
#         d_max is set equal to the largest depth in x_depth.
# method = method for the interpolation (see ?signal::interp1)

#' Migrate of the GPR data
#' 
#' Fresnel zone defined according to 
#' Perez-Gracia et al. (2008) Horizontal resolution in a non-destructive
#' shallow GPR survey: An experimental evaluation. NDT & E International,
#' 41(8): 611-620.
#' doi:10.1016/j.ndteint.2008.06.002
#'
#' @param max_depth maximum depth to appply the migration
#' @param dz        vertical resolution of the migrated data
#' @param fdo       dominant frequency of the GPR signal
#' 
#' @name migrate
#' @rdname migrate
#' @export
setMethod("migrate", "GPR", function(x, type = c("static", "kirchhoff"), ...){
  if(length(x@antsep) == 0 || (!is.numeric(x@antsep))){
    stop("You must first define the antenna separation ",
         "with 'antsep(x) <- 1' for example!")
  }
  if(is.null(x@vel) || length(x@vel)==0){
    stop("You must first define the EM wave velocity ",
         "with 'vel(x) <- 0.1' for example!")
  }
  if(length(x@coord) != 0 && ncol(x@coord) == 3){
    topo <- x@coord[1:ncol(x@data), 3]
    # stop("You must first set coordinates to the traces ",
    #      "with 'coord(x) <- ...' or ",
    #      "'x <- interpPos(x, ...)' !")
  }else{
    topo <- rep.int(0L, ncol(x@data))
    message("Trace vertical position set to zero!")
  }
  type <- match.arg(type, c("static", "kirchhoff"))
  if(type == "static"){  
    if(any(x@time0 != 0)){
      x <- time0Cor(x, method = c("pchip"))
    }
    if(x@depthunit == "ns"){
      if(length(x@vel[[1]]) == 1){
        message("time to depth conversion with constant velocity (", x@vel[[1]],
                " ", x@posunit, "/", x@depthunit, ")")
        z <- timeToDepth(x@depth, time_0 = 0, v = vel(x), 
                         antsep = antsep(x))
        x <- x[!is.na(z),]
        x@dz <-  x@dz * x@vel[[1]]/ 2
        x@depth <- seq(from = 0, to = tail(z, 1), by = x@dz)
        funInterp <- function(x, z, zreg){
          signal::interp1(x = z, y = x, xi = zreg, 
                          method = "pchip", extrap = TRUE)
        }
        x@data <- apply(x@data, 2, funInterp, 
                        z = z[!is.na(z)], zreg = x@depth)
      }else if(is.matrix(x@vel[[1]])){
        x_depth <- apply(c(0, diff(depth(x))) * x@vel[[1]]/2, 2, cumsum)
        dots <- list(...)
        if( !is.null(dots$dz)){
          dz <- dots$dz
        }else{
          dz <- min(apply(x_depth, 2, diff))
        }
        if( !is.null(dots$dmax)){
          dmax <- dots$dmax
        }else{
          dmax <- max(x_depth)
        }
        if( !is.null(dots$method)){
          method <- match.arg(dots$method, c("linear", "nearest", "pchip", "cubic", "spline"))
        }else{
          method <- "pchip"
        }
        d <- seq(from = 0, by = dz, to = dmax)
        x_new <- matrix(nrow = length(d), ncol = ncol(x))
        for(i in seq_along(x)){
          x_new[, i] <- signal::interp1(x  = as.numeric(x_depth[,i]),
                                        y  = as.numeric(x[,i]),
                                        xi = d,
                                        method = method)
        }
        x@data      <- x_new
        x@depth     <- d
        x@dz        <- dz
      }
      x@depthunit <- "m"
    }else{
      # interpolation at regular interval if x@dz is not unique!!
      if(!length(unique(diff(x@depth))) ){
        x@dz <- min(abs(diff(x@depth)))
        zreg <- seq(from = min(x@depth), to = tail(x@depth, 1), by = x@dz)
        funInterp <- function(x, z, zreg){
          signal::interp1(x = z, y = x, xi = zreg, 
                          method = "pchip", extrap = TRUE)
        }
        x@data <- apply(x@data, 2, funInterp, 
                        z = x@depth, zreg = zreg)
      }
    }
    zShift <- (max(topo) - topo)
    if( all(zShift != 0) ){
      x <- traceShift(x,  ts = zShift, method = c("pchip"), crop = FALSE)
    }
    if(length(x@coord) > 0 && ncol(x@coord) == 3 ){
      x@coord[, 3] <- max(x@coord[,3])
    }
    x@vel <- list() 
  }else if(type == "kirchhoff"){
    A <- x@data
    #topo <- x@coord[,3]
    dx <- x@dx
    dts <- x@dz
    v <- x@vel[[1]]
    # initialisation
    #max_depth <- nrow(x)*x@dx
    max_depth <- max(x@depth) * v / 2 * 0.9
    dz <- 0.25 * x@dz
    fdo <- x@freq
    FUN <- sum
    if( length(list(...)) ){
      dots <- list(...)
      if( !is.null(dots$max_depth)){
        max_depth <- dots$max_depth
      }
      if( !is.null(dots$dz)){
        dz <- dots$dz
      }
      if( !is.null(dots$fdo)){
        fdo <- dots$fdo
      }
      if( !is.null(dots$FUN)){
        FUN <- dots$FUN
      }
    }  
    x@data      <- .kirMig(x@data, topoGPR = topo, xpos = x@pos,
                           dts = x@dz, v = v, max_depth = max_depth, 
                           dz = dz, fdo = fdo, FUN = FUN)
    x@depth     <- seq(0,by=dz, length.out = nrow(x))
    x@time0     <- rep(0, ncol(x))
    x@dz        <- dz
    x@depthunit <- x@posunit          # check!!!
    if(length(x@coord) > 0 && ncol(x@coord) == 3 ){
      x@coord[,3] <- max(x@coord[,3])
    }
  }
  proc(x) <- getArgs()
  return(x)
} 
)


# x = data matrix (col = traces)
# topoGPR = z-coordinate of each trace
# dx = spatial sampling (trace spacing)
# dts = temporal sampling
# v = GPR wave velocity (ns)
# max_depth = to which depth should the migration be performed
# dz = vertical resolution of the migrated data
# fdo = dominant frequency of the GPR signal
.kirMig <- function(x, topoGPR, xpos, dts, v, max_depth = 8, 
                    dz = 0.025, fdo = 80, FUN = sum){
  n <- nrow(x)
  m <- ncol(x)
  z <- max(topoGPR) - topoGPR
  fdo <- fdo * 10^6   # from MHz to Hz
  lambda <- fdo / v * 10^-9
  v2 <- v^2
  message("max depth1 = ", max_depth)
  max_depth <- max_depth + max(z)
  message("max depth2 = ", max_depth)
  kirTopoGPR <- matrix(0, nrow = floor(max_depth/dz) + 1, ncol = m)
  
  dx <- mean(diff(xpos))
  for( i in seq_len(m)){
    x_d <- xpos[i]   # diffraction
    z_d <- seq(z[i], max_depth, by = dz)
    for(k in seq_along(z_d)){
      t_0 <- 2*(z_d[k] - z[i])/v    # = k * dts in reality
      # z_idx <- round(z_d[k] /dz + 1)
      z_idx <-  floor(z_d[k] /dz) + 1
      # print(z_idx)
      # if(z_idx <= nrow(kirTopoGPR) && z_idx > 0){
        # Fresnel zone
        # Pérez-Gracia et al. (2008) Horizontal resolution in a non-destructive
        # shallow GPR survey: An experimental evaluation. NDT & E International,
        # 41(8): 611–620. doi:10.1016/j.ndteint.2008.06.002
        rf <- 0.5 * sqrt(lambda * 2 * (z_d[k] - z[i]))
        rf_tr <- round(rf/dx)
        mt <- (i - rf_tr):(i + rf_tr)
        mt <- mt[mt > 0 & mt <= m]
        
        lmt <- length(mt)
        Ampl <- numeric(lmt)
        for(j in mt){
          # x_a <- (j-1)*dx
          x_a <- xpos[j]
          t_top <-  t_0 - 2*(z[j] - z[i])/v
          t_x <- sqrt( t_top^2 +   4*(x_a - x_d)^2 /v2)
          t1 <- floor(t_x/dts) + 1 # the largest integers not greater
          t2 <- ceiling(t_x/dts) + 1 # smallest integers not less
          if(t2 <= n && t1 > 0 && t_x != 0){
            w <- ifelse(t1 != t2, abs((t1 - t_x)/(t1 - t2)), 0)
            # Dujardin & Bano amplitude factor weight: cos(alpha) = t_top/t_x
            # Ampl[j- mt[1] + 1] <- (t_top/t_x) * 
            # ((1-w)*A[t1,j] + w*A[t2,j])
            # http://sepwww.stanford.edu/public/docs/sep87/SEP087.Bevc.pdf
            Ampl[j- mt[1] + 1] <- (dx/sqrt(2*pi*t_x*v))*
                                  (t_top/t_x) * 
                                  ((1-w)*x[t1,j] + w*x[t2,j])
          }
        # }
        kirTopoGPR[z_idx, i] <- FUN(Ampl)
      }
    }
  }
  kirTopoGPR <- kirTopoGPR/max(kirTopoGPR, na.rm=TRUE) * 50
  #   kirTopoGPR2 <- kirTopoGPR
  
  return(kirTopoGPR)
}
# x = data matrix (col = traces)
# topoGPR = z-coordinate of each trace
# dx = spatial sampling (trace spacing)
# dts = temporal sampling
# v = GPR wave velocity (ns)
# max_depth = to which depth should the migration be performed
# dz = vertical resolution of the migrated data
# fdo = dominant frequency of the GPR signal
.kirMigold <- function(x, topoGPR, dx, dts, v, max_depth = 8, 
                       dz = 0.025, fdo = 80, FUN = sum){
  n <- nrow(x)
  m <- ncol(x)
  z <- max(topoGPR) - topoGPR
  fdo <- fdo * 10^6   # from MHz to Hz
  lambda <- fdo / v * 10^-9
  v2 <- v^2
  kirTopoGPR <- matrix(0, nrow = max_depth/dz + 1, ncol = m)
  for( i in seq_len(m)){
    x_d <- (i - 1)*dx   # diffraction
    z_d <- seq(z[i], max_depth, by = dz)
    #     mt <- (i - migTpl):(i + migTpl) # migration template
    #     mt <- mt[mt > 0 & mt <= m]
    #     l_migtpl <- length(mt)
    
    for(k in seq_along(z_d)){
      t_0 <- 2*(z_d[k] - z[i])/v    # = k * dts in reality
      z_idx <- round(z_d[k] /dz + 1)
      # Fresnel zone
      # Pérez-Gracia et al. (2008) Horizontal resolution in a non-destructive
      # shallow GPR survey: An experimental evaluation. NDT & E International,
      # 41(8): 611–620. doi:10.1016/j.ndteint.2008.06.002
      rf <- 0.5 * sqrt(lambda * 2 * (z_d[k] - z[i]))
      rf_tr <- round(rf/dx)
      mt <- (i - rf_tr):(i + rf_tr)
      mt <- mt[mt > 0 & mt <= m]
      
      lmt <- length(mt)
      Ampl <- numeric(lmt)
      for(j in mt){
        x_a <- (j-1)*dx
        t_top <-  t_0 - 2*(z[j] - z[i])/v
        t_x <- sqrt( t_top^2 +   4*(x_a - x_d)^2 /v2)
        t1 <- floor(t_x/dts) + 1 # the largest integers not greater
        t2 <- ceiling(t_x/dts) + 1 # smallest integers not less
        if(t2 <= n && t1 > 0 && t_x != 0){
          w <- ifelse(t1 != t2, abs((t1 - t_x)/(t1 - t2)), 0)
          # Dujardin & Bano amplitude factor weight: cos(alpha) = t_top/t_x
          # Ampl[j- mt[1] + 1] <- (t_top/t_x) * 
          # ((1-w)*A[t1,j] + w*A[t2,j])
          # http://sepwww.stanford.edu/public/docs/sep87/SEP087.Bevc.pdf
          Ampl[j- mt[1] + 1] <- (dx/sqrt(2*pi*t_x*v))*
            (t_top/t_x) * 
            ((1-w)*x[t1,j] + w*x[t2,j])
        }
      }
      kirTopoGPR[z_idx,i] <- FUN(Ampl)
    }
  }
  kirTopoGPR <- kirTopoGPR/max(kirTopoGPR, na.rm=TRUE) * 50
  #   kirTopoGPR2 <- kirTopoGPR
  
  return(kirTopoGPR)
}