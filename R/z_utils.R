
#' Extract frequency from string
#' 
#' Extract with regex the antenna frequency in a string
#' @export
freqFromString <- function(s){
  if(grepl("MHz", s, ignore.case = TRUE)){
    a <- regexpr("[0-9]+.MHZ",  s, ignore.case = TRUE, perl = TRUE)
  }else{
    a <- regexpr("[0-9]+",  s, ignore.case = TRUE, perl = TRUE)
  }
  b <- regmatches(s,  a)
  as.numeric(gsub("[^0-9]", "", b))
}

# x = data.frame tag + val
.getHD <- function(x, pattern){
  idx <- grep(pattern, x$tag, ignore.case = TRUE)
  if(length(idx) > 0 ){
    return( x$val[idx[1]] )
  }else{
    return(NULL)
  }
}


#' Bytes to volt conversion
#'       
#' Convert bytes to volt values
#' @param Vmax length-one numeric vector: maximal nominal analog input voltage. 
#'             If \code{Vmax = NULL} it returns \code{1} (no bytes to volt 
#'             transformation)
#' @param Vmin length-one numeric vector: minimal nominal analog input voltage. 
#'             If missing, then \code{Vmin = -Vmax}.
#' @param nbits Length-one numeric vector: number of bits
#' @export
bits2volt <- function( Vmax = 50, Vmin = 50, nbits = 16) {
  if(is.null(Vmax)){
    return(1L)
  }else{
    if( missing(Vmin) ){
      Vmin <- -Vmax
    }
    return( abs(Vmax - Vmin) / ( 2^nbits ) )
  }
}

#' Suppressing output from cat(), warnings & messages
# #' @export
verboseF <- function(g, verbose = TRUE){
  if(verbose){
    g
  }else{
    suppressWarnings(suppressMessages(quiet(g)))
  }
}


#' Suppressing output from cat() or print()
#' 
#' This function suppresses the output from cat() or print() in a function. 
#' It was proposed by Hadley Wickham 
#' https://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
# #' @export
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
} 