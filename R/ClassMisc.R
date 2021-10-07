

# Georeferencing
#' @export
setMethod("georef", "matrix", 
          function(x, alpha = NULL, cloc = NULL, creg = NULL,
                   ploc = NULL, preg = NULL, FUN = mean){
            # print(ploc)
  x <- .georef (x, alpha = alpha, cloc = cloc, creg = creg,
                   ploc = ploc, preg = preg, FUN = FUN)
  return(x)
})

