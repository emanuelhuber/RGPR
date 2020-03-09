# spIntersect(x=GPR/GPRsurvey, y=NULL)


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
#' @name spIntersect
setGeneric("spIntersect", function(x) 
  standardGeneric("spIntersect"))

#' @rdname spIntersect
#' @export
setMethod("spIntersect", "GPRsurvey", function(x){
  for(i in seq_along(x@paths)){
    if(length(x@coords[[i]]) > 0){
      top0 <- x@coords[[i]]
      Sa <- verboseF(as.SpatialLines(x[i]), verbose = FALSE)
      v <- seq_along(x@paths)[-i]
      int_coords <- c()
      int_traces <- c()
      int_names <- c()
      for(j in seq_along(v)){
        if(length(x@coords[[v[j]]]) > 0){
          # test bounding box intersection ???????????????????????????
          top1 <- x@coords[[v[j]]]
          Sb <- verboseF(as.SpatialLines(x[v[j]]), verbose = FALSE)
          pt_int <- rgeos::gIntersection(Sa,Sb)
          if(!is.null(pt_int) && class(pt_int) == "SpatialPoints"){
            # for each intersection points
            for(k in seq_along(pt_int)){
              d <- sqrt(rowSums((top0[,1:2] - 
                                   matrix(sp::coordinates(pt_int)[k,],
                                          nrow = nrow(top0), ncol = 2, byrow = TRUE))^2))
              int_coords <- rbind(int_coords, sp::coordinates(pt_int)[k,])
              int_traces <- c(int_traces, which.min(d)[1])
              int_names  <- c(int_names, x@names[v[j]])
            }
          }
        }
      }
      if(length(int_names) > 0){
        # x@intersections[[x@names[i]]] <- list(coord = int_coords,
        x@intersections[[i]] <- list(coord = int_coords,
                                     trace = int_traces,
                                     name  = int_names)
      }else{
        # x@intersections[[x@names[i]]] <- NULL
        x@intersections[[i]] <- NULL
      }
    }
  }
  return(x)
})
