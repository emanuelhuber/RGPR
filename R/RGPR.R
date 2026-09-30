#' RGPR: A package for processing and visualising ground-penetrating data 
#' radar (GPR) data.
#'
#' The RGPR package provides two classes GPR and GPRsurvey
#' @import stats
#' @import graphics
#' @import utils 
#' @import grDevices 
#' @import methods
#' @import sf
"_PACKAGE"



.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("Don't hesitate to contact me if you ",
                               "have any question:\n",
                               "emanuel.huber@pm.me"))
}

#' @useDynLib RGPR
#' @importFrom Rcpp sourceCpp
NULL
