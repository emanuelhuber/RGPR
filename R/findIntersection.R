# FIXME intersect(x=GPR/GPRsurvey, y=NULL)


#' Compute GPR profile intersections
#'
#' Compute GPR profile intersections
#' 
#' Modified slots
#' \itemize{
#'   \item `intersects`: trace shifted. The number of rows of data may 
#'         be smaller if `crop = TRUE`.
#' }
#'
#' @param x      (`GPRsurvey`) An object of the class `GPRsurvey`
#' @return (`object GPRsurvey`) An object of the class GPRsurvey.
#' @name findIntersection
#' @concept spatial computing
setGeneric("findIntersection", function(x) 
  standardGeneric("findIntersection"))

#' @rdname findIntersection
#' @export
setMethod("findIntersection", "GPRsurvey", function(x){
  sel <- sapply(x@coords, function(x) length(x) > 0)
  if(all(!sel)){
    return(x)
    stop("No coordinates: I cannot compute intersections...")
  }
  
  x@intersections <- vector(length = length(x), mode = "list")
  
  if(sum(sel) == 1) return(x)  # FIXME compute self-intersection
  if(!is.na(x@crs) && length(unique(x@crs[!is.na(x@crs)])) != 1){
    warning("Your data have different 'crs'.\n",
            "  I recommend you to set an unique 'crs' to the data\n",
            "  using either 'crs()<-' or 'project()'")
  }
  x_sf <- verboseF(as.sf(x), verbose = FALSE)
  x_names <- x@names[sel]
  # currently does not support sefl-intersection....
  n <- nrow(x_sf)
  ntsct <- vector(mode = "list", length = n)
  for(i in 1:(n - 1) ){
    v <- (i + 1):n
    for(j in seq_along(v)){
      pp <- verboseF(sf::st_intersection(x_sf[i, ], x_sf[v[j], ] ), verbose = FALSE)
      if(nrow(pp) > 0 ){
        # to avoid the case where two GPR lines perfectly overlapp...
        tst <- sapply(sf::st_geometry(pp),  inherits, what = c("MULTIPOINT", "POINT"))
        pp <- pp[tst,]
        if(nrow(pp) > 0){
          pp0 <- sf::st_cast(pp, "POINT",  warn = FALSE)
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
  }
  x@intersections[sel] <- ntsct

  return(x)
})
