
  
#' Print GPRcube
#' @param x (`GPRcube object`) 
#' @param ... Not used. 
#' @export
print.GPRcube <- function(x, ...){
  toprint <- character(8)
  toprint[1] <- "*** Class GPRcube ***"
  prt_name <- c("dim:    ",
                "res:    ",
                "extent: ",
                "center: ",
                "angle:  ",
                "crs:    "
                )
  prt_content <- c(
    paste0(dim(x@data), collapse = " x "),
    paste(c(x@dx, x@dy, x@dz), c(rep(x@xunit, 2), x@zunit), collapse = " x "),
    paste(c(x@dx * (dim(x@data)[1] - 1), 
                    x@dy * (dim(x@data)[2] - 1), 
                    x@dz * (dim(x@data)[1] - 1)), 
                    c(rep(x@xunit, 2), x@zunit), collapse = " x "),
    paste(x@center, c(rep(x@xunit, 2), x@zunit), collapse = ", "),
    ifelse(length(x@rot) == 5, x@rot[5], 0),
    x@crs
  )
  toprint[2:7] <- mapply(paste0, prt_name, prt_content, USE.NAMES = FALSE)
  toprint[8] <- "*********************"
  cat(toprint, sep = "\n")
}

#' Show some information on the GPRcube object
#'
#' Identical to print().
#' @param object (`GPRcube object`) 
#' @export
setMethod("show", "GPRcube", function(object){print.GPRcube(object)}) 
