
  
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
  
  d <- dim(x)   # dim(),GPRcube-method: works for in-memory, HDF5-backed, and view cubes
  
  # @z need not be evenly spaced (see GPRcube-class); show the constant
  # spacing if there is one, otherwise the min/max spacing as "irregular".
  z_res <- if (length(x@z) < 2L) {
    NA_character_
  } else {
    zd <- diff(x@z)
    if (length(unique(zd)) == 1L) {
      format(zd[1])
    } else {
      paste0("irregular (", signif(min(abs(zd)), 3), " to ", signif(max(abs(zd)), 3), ")")
    }
  }
  z_extent <- if (length(x@z) >= 1L) diff(range(x@z)) else NA_real_
  
  prt_content <- c(
    paste0(d, collapse = " x "),
    paste(c(x@dx, x@dy, z_res), c(rep(x@xunit, 2), x@zunit), collapse = " x "),
    paste(c(x@dx * (d[1] - 1), 
                    x@dy * (d[2] - 1), 
                    z_extent), 
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
