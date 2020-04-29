
  
#' Print GPRslice
#' @param x [\code{GPRslice object}] 
#' @param ... Not used. 
#' @export
print.GPRslice <- function(x, ...){
  toprint <- character(9)
  toprint[1] <- "*** Class GPRslice ***"
  prt_name <- c("depth:  ",
                "dim:    ",
                "res:    ",
                "extent: ",
                "center: ",
                "angle:  ",
                "crs:    "
                )
  prt_content <- c(
    paste(x@z, x@zunit),
    paste0(dim(x@data), collapse = " x "),
    paste(c(x@dx, x@dy), rep(x@xunit, 2), collapse = " x "),
    paste(c(x@dx * (dim(x@data)[1] - 1), 
                    x@dy * (dim(x@data)[2] - 1)), 
                    rep(x@xunit, 2), collapse = " x "),
    paste(x@center, rep(x@xunit, 2), collapse = ", "),
    ifelse(length(x@rot) == 5, x@rot[5], 0),
    x@crs
  )
  toprint[2:8] <- mapply(paste0, prt_name, prt_content, USE.NAMES = FALSE)
  toprint[9] <- "**********************"
  cat(toprint, sep = "\n")
}

#' Show some information on the GPRslice object
#'
#' Identical to print().
#' @param object [\code{GPRslice object}] 
#' @export
setMethod("show", "GPRslice", function(object){print.GPRslice(object)}) 
