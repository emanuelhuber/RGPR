
#' Extract frequency from string
#' 
#' Extract with regex the antenna frequency in a string
#' @param s [\code{character(1)}] Character string that may contain an 
#'                                indication of a frequency.
#' @return [\code{numeric(1)}] The frequency.
#' @examples 
#' s <- "1230 fds 200-MHZ 12.3"
#' freqFromString(s) 
#' s <- "1230MLF"
#' freqFromString(s) 
#' s <- "D1230MLF"
#' freqFromString(s) 
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




#' Estimate antenna separation from antenna frequency
#' @param antfreq [\code{numeric(1)}] Antenna frequency.
#' @param verbose [\code{logical(1)}] If \code{TRUE} the function gives a
#'                                    message.
#' @return [\code{numeric(1)}] Antenna separation
#' @name antSepFromAntFreq
#' @rdname antSepFromAntFreq
#' @export
antSepFromAntFreq <- function(antfreq, verbose = TRUE){
  ant <- list(f = c(12.5, 25, 50, 100, 110, 200, 225, 450,   900, 1200),
              s = c( 8,    4,  2,   1,   1, 0.5, 0.5, 0.25, 0.17, 0.075))
  antsep <- approx(ant$f, ant$s, xout = antfreq)$y
  antsep <- round(antsep, 3)
  if(verbose){
    message("Antenna separation (", antsep, " m) estimated from antenna", 
            " frequency (", antfreq, " MHz).",
            "\nCorrect if wrong with 'antsep(x) <- ...'")
  }
  
  if(is.na(antsep)) antsep <- numeric(0)
  return(antsep)
}

#' Bytes to volt conversion
#'       
#' Convert bytes to volt values
#' @param Vmax [\code{numeric(1)}] Maximal nominal analog input voltage. 
#'             If \code{Vmax = NULL} it returns \code{1} (no bytes to volt 
#'             transformation)
#' @param Vmin [\code{numeric(1)}] Minimal nominal analog input voltage. 
#'             If missing, then \code{Vmin = -Vmax}.
#' @param nbits [\code{integer(1)} Number of bits.
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

#' Suppressing output from cat(), warnings & messages in functions
#' 
#' @param g       [\code{function}] A function.
#' @param verbose [\code{logical(1)}] If \code{FALSE}, suppress any warnings
#'                and messages in function \code{g}, else do nothing.
#' @export
verboseF <- function(g, verbose = TRUE){
  if(verbose){
    g
  }else{
    suppressWarnings(suppressMessages(quiet(g)))
  }
}


# #' Suppressing output from cat() or print()
# #' 
# #' This function suppresses the output from cat() or print() in a function. 
# #' It was proposed by Hadley Wickham 
# #' https://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
# #' @export
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
} 