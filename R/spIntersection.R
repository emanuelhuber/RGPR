# spIntersection(x=GPR/GPRsurvey, y=NULL)


#' Compute GPR profile intersections
#'
#' Compute GPR profile intersections
#' 
#' Modified slots
#' \itemize{
#'   \item \code{intersects}: trace shifted. The number of rows of data may 
#'         be smaller if \code{crop = TRUE}.
#' }
#'
#' @param x      [\code{object class GPRsurvey}] An object of the class \code{GPRsurvey}
#' @return [\code{object GPRsurvey}] An object of the class GPRsurvey.
#' @name spIntersection
setGeneric("spIntersection", function(x) 
  standardGeneric("spIntersection"))

#' @rdname spIntersection
#' @export
setMethod("spIntersection", "GPRsurvey", function(x){
  sel <- sapply(x@coords, function(x) length(x) > 0)
  if(all(!sel)){
    return(x)
    stop("No coordinates: I cannot compute intersections...")
  }
  if(length(unique(x@crs[!is.na(x@crs)])) != 1){
    warning("Your data have different 'crs'.\n",
            "  I recommend you to set an unique 'crs' to the data\n",
            "  using either 'crs()<-' or 'spProjectToCRS()'")
  }
  x_sf <- verboseF(as.sf(x), verbose = FALSE)
  x_names <- x@names[sel]
  # currently does not support sefl-intersection....
  n <- length(x_sf)
  ntsct <- vector(mode = "list", length = n)
  for(i in 1:(n - 1) ){
    v <- (i + 1):n
    for(j in seq_along(v)){
      pp <- verboseF(sf::st_intersection(x_sf[i], x_sf[v[j]]), verbose = FALSE)
      if(length(pp) > 0 ){
        pp0 <- sf::st_cast(pp, "POINT")
        # plot(pp, add = TRUE, col = "red")
        # print(paste0(i, " - ", v[j]))
        pp <- data.frame(sf::st_coordinates(pp0), name = x_names[v[j]])
        if(!is.null(ntsct[[i]])){
          ntsct[[i]] <- rbind(pp, ntsct[[i]])
        }else{
          ntsct[[i]] <- pp
        }
        pp$name <- x_names[i]
        if(!is.null(ntsct[[v[j]]])){
          ntsct[[v[j]]] <- rbind(pp, ntsct[[v[j]]])
        }else{
          ntsct[[v[j]]] <- pp
        }
      }
    }
  }
  x@intersections <- vector(length = length(x), mode = "list")
  x@intersections[sel] <- ntsct
  # for(i in seq_along(x@paths)){
  #   if(length(x@coords[[i]]) > 0){
  #     top0 <- x@coords[[i]]
  #     Sa <- verboseF(as.SpatialLines(x[i]), verbose = FALSE)
  #     v <- seq_along(x@paths)[-i]
  #     int_coords <- c()
  #     int_traces <- c()
  #     int_names <- c()
  #     for(j in seq_along(v)){
  #       if(length(x@coords[[v[j]]]) > 0){
  #         # test bounding box intersection ???????????????????????????
  #         top1 <- x@coords[[v[j]]]
  #         Sb <- verboseF(as.SpatialLines(x[v[j]]), verbose = FALSE)
  #         pt_int <- rgeos::gIntersection(Sa,Sb)
  #         if(!is.null(pt_int) && class(pt_int) == "SpatialPoints"){
  #           # for each intersection points
  #           for(k in seq_along(pt_int)){
  #             d <- sqrt(rowSums((top0[,1:2] - 
  #                                  matrix(sp::coordinates(pt_int)[k,],
  #                                         nrow = nrow(top0), ncol = 2, byrow = TRUE))^2))
  #             int_coords <- rbind(int_coords, sp::coordinates(pt_int)[k,])
  #             int_traces <- c(int_traces, which.min(d)[1])
  #             int_names  <- c(int_names, x@names[v[j]])
  #           }
  #         }
  #       }
  #     }
  #     if(length(int_names) > 0){
  #       # x@intersections[[x@names[i]]] <- list(coord = int_coords,
  #       x@intersections[[i]] <- list(coord = int_coords,
  #                                    trace = int_traces,
  #                                    name  = int_names)
  #     }else{
  #       # x@intersections[[x@names[i]]] <- NULL
  #       x@intersections[[i]] <- NULL
  #     }
  #   }
  # }
  return(x)
})
