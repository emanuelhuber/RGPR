#' Convex hull of GPR data
#' 
#' Return the convex hull.
#' @param x [\code{GPR|GPRsurvey}] 
#' @param verbose [\code{logical(1)}] If \code{FALSE}, all messages and warnings 
#'        are suppressed (use with care).

#' @name convexhull
setGeneric("convexhull", function(x, verbose = FALSE) 
  standardGeneric("convexhull"))

#' @rdname convexhull
#' @export
setMethod("convexhull", "GPRsurvey", function(x, verbose = FALSE) {
  xsf <- verboseF(as.sf(x), verbose = verbose)
  xsf <- sf::st_combine(xsf)
  return(sf::st_convex_hull(xsf))
})

#' @rdname convexhull
#' @export
setMethod("convexhull", "GPR", function(x, verbose = FALSE) {
  xsf <- verboseF(as.sf(x), verbose = verbose)
  return(sf::st_convex_hull(xsf))
})


#' @rdname convexhull
#' @export
setMethod("convexhull", "matrix", function(x, verbose = FALSE) {
  xsf <- sf::st_as_sf(x      = as.data.frame(x[, 1:2]),
                      coords = 1:2)
  xsf <- sf::st_combine(xsf)
  return(sf::st_convex_hull(xsf))
})
