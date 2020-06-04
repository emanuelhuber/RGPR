
#' 2D interpolation of the delineations and surface topography
#' 
#' Takes the delineations having the same name or index and interpolates
#' them to a surface. Interpolate also the topography (surface elevation
#' given by the coordinates).
#' 
#' \describe{
#'   \item{\code{bbox}}{axis-aligned bounding box}
#'   \item{\code{chull}}{convex hull}
#'   \item{\code{buffer}}{BUffer polygon at a specified distance around 
#'                        the GPR lines. The argument \code{buffer} must be 
#'                        specified (i.e., not \code{NULL})}
#' }
#' 
#' @param x      [\code{GPRsurvey class}] An object of the class \code{GPRsurvey}
#' @param dx      [\code{numeric(1)}] x-resolution
#' @param dy      [\code{numeric(1)}] y-resolution
#' @param h      [\code{integer(1)}] Number of levels in the hierarchical 
#'               construction (see \code{\link[MBA]{mba.surf}}).
#' @param extend [\code{character(1)}] Extend type for the interpolation based 
#'               on the GPR data (if \code{shp = NULL} or on the object passed
#'               to \code{shp}), see details.
#' @param buffer [\code{character(1)}] A buffer can be applied to increase the 
#'               extend by the value defined by \code{buffer}.
#' @param shp [\code{matrix|list|sf|sp}] An object to define the interpolation
#'            extend.
#' @return [\code{array(m, n, k + 1)}] An array consisting of \eqn{k + 1} 
#'         surfaces of dimension \eqn{m \times n}. \eqn{k} is the number of
#'         delineations. The first surface corresponds to the topography while
#'         the others to the interpolated delineations.
#' @name interp2DDelineations
setGeneric("interp2DDelineations", function(x, dx = NULL, dy = NULL,  h = 6,
                                            extend = c("bbox", "chull", "buffer"),
                                            buffer = NULL,
                                            shp = NULL) 
  standardGeneric("interp2DDelineations"))

#' @rdname interp2DDelineations   
#' @export
setMethod("interp2DDelineations", "GPRsurvey", function(x, dx = NULL, dy = NULL,  h = 6,
                                 extend = c("bbox", "chull", "buffer"),
                                 buffer = NULL,
                                 shp = NULL){
  extend = match.arg(extend[1], c("bbox", "obbox", "chull", "buffer"))
  xyrange <- apply(do.call(rbind, x@coords), 2, range)[, 1:2]
  if(is.null(dx)){
    dx <- min(round(diff(xyrange)/100))
  }
  if(is.null(dy)){
    dy <- dx
  }
  
  para <- getParaBox(x = as_sf(x@coords, x@crs), 
                     shp = shp, 
                     extend = extend, 
                     buffer = buffer, 
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
  
  test <- list()
  for(i in seq_along(X_del)){
    test[[i]] <- extractDelineations(X_del[[i]])
  }
  
  test_names <- lapply(test, names)
  test_names_uni <- unique(unlist(test_names))
  if(length(test_names_uni) > 1){
    test_tbl <- sapply(test_names, function(x, y) y %in% x, y = test_names_uni)
    if(is.null(dim(test_tbl))){
      message("This should not be possible...")
    }else{
      u <- rowSums(test_tbl)
      test_names_uni <- test_names_uni[u > 1]
    }
  }
  
  nL <- length(test_names_uni) + 1
  Sz <- array(dim = c(para$nx, para$ny, nL))
  S0 <- MBA::mba.surf(do.call(rbind, x@coords), no.X = para$nx , no.Y = para$ny, n = n, m = m, 
                      extend = TRUE, h = h, b.box = para$bbox)$xyz.est 
  Sz[,, 1] <- S0$z[]
  if(!is.null(para$clip)){
    # if(is.null(fk)){
    fk <- outer(S0$x, S0$y, inPoly,
                vertx = para$clip[,1],
                verty = para$clip[,2])
    fk <- !as.logical(fk)
    # }
    Sz[,, 1][fk] <- NA
  }
  for(i in seq_along(test_names_uni)){
    tst_i <- lapply(test, function(x, y) x[[y]][, 1:3], y = test_names_uni[i])
    x_del_i <- do.call(rbind, tst_i)
    parai <- getParaBox(x = as_sf(tst_i, x@crs), 
                        shp = shp, 
                        extend = extend, 
                        buffer = buffer, 
                        dx, dy)
    S0 <- MBA::mba.surf(x_del_i, no.X = para$nx , no.Y = para$ny, n = n, m = m, 
                        extend = TRUE, h = h, b.box = parai$bbox)$xyz.est 
    Sz[,, i+1] <- S0$z[]
    if(!is.null(parai$clip)){
      # if(is.null(fk)){
      fk <- outer(S0$x, S0$y, inPoly,
                  vertx = parai$clip[,1],
                  verty = parai$clip[,2])
      fk <- !as.logical(fk)
      # }
      Sz[,, i + 1][fk] <- NA
    }
    
  }
  S0$z <- Sz
  
  xyref <- c(min(S0$x), min(S0$y))
  xpos <- S0$x - min(S0$x)
  ypos <- S0$y - min(S0$y)
  
  xfreq <- ifelse(length(unique(x@freqs)) == 1, x@freqs[1], numeric())
  
  
  # plot3D::image2D(x = SXY$x, y = SXY$y, z = SXY$z[,,k],
  #                 main = paste0("elevation = ", SXY$vz[k], " m"),
  #                 zlim = zlim, col = palGPR("slice"))
  # 
  # plot3D::points2D(x = SXY$x0, y = SXY$y0, colvar = SXY$z0[[k]],
  #                  add = TRUE, pch = 20, clim = zlim, col = palGPR("slice"))
  
  # FIXME : if only one slice -> create slice object !!!!
  if(dim(S0$z)[3] == 1){
    className <- "GPRslice"
    ddata <- S0$z[,,1]
  }else{
    className <- "GPRcube"
    ddata <- S0$z
  }
  
  y <- new(className,
           version      = "0.1",
           name         = "",
           date         = as.character(Sys.Date()),  
           freq         = xfreq,
           filepaths    = x@filepaths,
           x            = xpos,
           y            = ypos,
           data         = ddata,
           coord        = xyref,
           posunit      = x@posunits[1],
           crs          = x@crs,
           depth        = 1:dim(S0$z)[3],
           depthunit    = "-"
           #transf       = "numeric"
  )
  
  return(y)
} )


# x = x@coords
as_sf <- function(x, x_crs){
  if(length(x_crs) > 1){
    x_crs <- x_crs[!is.null(x_crs) & !is.na(x_crs)][1]
  }
  x_sf <- lapply(x, .as_LINESTRING, x_crs)
  x_sf <- do.call(c, x_sf)
  xsf <- sf::st_combine(x_sf)
  return(xsf)
}

.as_LINESTRING <- function(x, CRSobj){
  if(length(x) > 0){
    xi_sf <- sf::st_as_sf(x      = as.data.frame(x),
                          coords = 1:3,
                          crs    = CRSobj)
    xi_sf <- sf::st_combine(xi_sf)
    xi_sf <- sf::st_cast(xi_sf, "LINESTRING")
    return(xi_sf)
  }
}


extractDelineations <- function(x){
  int <- RGPR:::.getXYZrelIntp(x, method = "pchip")
  if(is.null(names(int))){
    names(int) <- as.character(seq_along(int))
  }else{
    test <- sapply(names(int), function(x) x == "")
    names(int)[test] <- as.character(seq_along(int)[test])
  }
  return(int)
  # do.call(rbind, int)
}





getParaBox <- function(x, shp, extend = NULL, buffer = NULL, dx, dy){
  xy_clip <- NULL
  x_shp <- x
  
  if(!is.null(shp)){
    if(inherits(shp, "sf") || inherits(shp, "sfc") || inherits(shp, "sfg")){
      # x_shp <- sf::st_coordinates(shp)[,1:2]
    }else if(is.list(shp)){
      x_shp <- sf::st_as_sf(x = as.data.frame(cbind(shp[[1]], shp[[2]])), coords = 1:2)
    }else if(is.matrix(shp)){
      x_shp <- sf::st_as_sf(x = as.data.frame(shp[, 1:2]), coords = 1:2)
    }
  }
  
  if(extend == "chull"){
    # xsf <- sf::st_as_sf(x = as.data.frame(x[, 1:2]), coords = 1:2)
    # xsf <- sf::st_combine(xsf)
    xsf_chull <- sf::st_convex_hull(x_shp)
    # xsf_chull <- spConvexHull(x_shp)
    if(is.null(buffer)){
      if(is.null(shp)){
        xsf_chull_xy <- sf::st_coordinates(xsf_chull)
        buffer <- min(diff(range(xsf_chull_xy[, 1])) * 0.05,  
                      diff(range(xsf_chull_xy[, 2])) * 0.05)
      }else{
        buffer <- 0
      }
    }
    if(buffer > 0){
      xsf_chull <- sf::st_buffer(xsf_chull, buffer)
    }
    xy <- sf::st_coordinates(xsf_chull)
    xy_clip <- xy
    buffer <- 0
    # para <- getbbox_nx_ny(xy[,1], xy[,2], dx, dy, buffer)
  }else if(extend == "bbox"){
    if(!is.null(shp)){
      if(is.null(buffer)) buffer <- 0
      # xy <- sf::st_coordinates(x_shp)
      # para <- getbbox_nx_ny(xy[, 1], xy[,2], dx, dy, buffer)
    }#else{
    # xy <- sf::st_coordinates(x_shp)
    # para <- getbbox_nx_ny(xy[, 1], xy[,2], dx, dy, buffer)
    # }
    xy <- sf::st_coordinates(x_shp)
    # }else if(extend == "obbox"){
    #   xy_obb <- spOBB(x_shp)
    #   if(is.null(buffer)){
    #     if(is.null(shp)){
    #       xsf_chull_xy <- sf::st_coordinates(xy_obb)
    #       buffer <- min(diff(range(xsf_chull_xy[, 1])) * 0.05,  
    #                     diff(range(xsf_chull_xy[, 2])) * 0.05)
    #     }else{
    #       buffer <- 0
    #     }
    #   }
    #   if(buffer > 0){
    #     xy_obb <- sf::st_buffer(xy_obb, buffer)
    #     xy_obb <- spOBB(xy_obb)
    #   }
    #   xy <- sf::st_coordinates(xy_obb)
    #   xy_clip <- sf::st_coordinates(sf_obb)
    #   #   
    #   para <- getbbox_nx_ny(xy_clip[,1], xy_clip[,2], dx, dy, buffer = 0)
  }else if(extend == "buffer"){
    if(is.null(buffer) || !(buffer > 0)){
      stop("When 'extend = buffer', 'buffer' must be larger than 0!")
    }else{
      xsf_chull <- sf::st_buffer(x_shp, buffer)
      # xsf_chull <- sf::st_convex_hull(xsf)
      # x_shp <- spBuffer(x, buffer)
      #xy_clip <- sf::st_coordinates(x_shp)
      xy <- sf::st_coordinates(xsf_chull)
      xy_clip <- xy
      buffer <- 0
    }
  }
  para <- getbbox_nx_ny(xy[,1], xy[,2], dx, dy, buffer)
  return(c(para, list(clip = xy_clip)))
}

getbbox_nx_ny <- function(xpos, ypos, dx, dy, buffer = NULL){
  xpos_rg <- range(xpos)
  ypos_rg <- range(ypos)
  
  if(is.null(buffer)){
    buffer <- min(diff(xpos_rg) * 0.05,  diff(ypos_rg) * 0.05)
    # print(buffer)
  }
  if(buffer > 0 ){
    # print(buffer)
    xpos_rg <- xpos_rg + c(-1, 1) * buffer
    ypos_rg <- ypos_rg + c(-1, 1) * buffer
    # print(c(-1, 1) * buffer)
  }
  # if(isTRUE(prettyExtend)){
  #   xpos_rg <- pretty( xpos_rg)
  #   ypos_rg <- pretty(ypos_rg )
  # }
  bbox <- c(xpos_rg, ypos_rg)
  
  # define the number of cells
  bbox_dx <- bbox[2] - bbox[1]
  bbox_dy <- bbox[4] - bbox[3]
  nx <- ceiling(bbox_dx / dx )
  ny <- ceiling(bbox_dy / dy )
  
  # correct bbox (such that dx, dy are correct)
  Dx <- (nx * dx - bbox_dx)/2
  Dy <- (ny * dy - bbox_dy)/2
  bbox[1:2] <- bbox[1:2] + c(-1, 1) * Dx
  bbox[3:4] <- bbox[3:4] + c(-1, 1) * Dy 
  # bbox_dx <- bbox[2] - bbox[1]
  # bbox_dy <- bbox[4] - bbox[3]
  nx <- nx + 1
  ny <- ny + 1
  return(list(bbox = bbox, nx = nx, ny = ny))
}





