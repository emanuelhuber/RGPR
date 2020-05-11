


#' Bits to volt conversion
#'       
#' Convert bits to volt values
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


#--- see trTime in "ClassGPR.R"
# #' @export                  
# trRecTime <- function(x, origin = "1970-01-01"){
#   return(as.POSIXct(x@time, origin = origin))
# }

#' Time to depth conversion
#'
#' Convert two-way travel time into depth by accounting for the antenna 
#' separation between the transmitter and the receiver.
#' @param twt    [\code{numeric}] Two-way travel time vector.
#' @param t0     [\code{numeric(1)}] Time-zero: only \code{x >= x0} will be
#'               considered.
#' @param v      [\code{numeric(1)}] Electromagnetic wave propagation in the 
#'               ground (or in the considered media).
#' @param antsep [\code{numeric(1)}] Antenna separation (distance between the
#'                transmitter and the receiver).
#' @return [\code{numeric}] Corresponding depth.
#' @export                  
timeToDepth <- function(twt, t0 = 0, v = 0.1, antsep = 1){
  # t0 <- t0 - antsep/c0
  y <- v^2 * (twt - t0)^2 - antsep^2
  test <- (y >= 0) & ((twt - t0) >= 0)
  y[!test] <- NA
  y[test] <- sqrt(y[test])/2
  return(y)
  # sqrt(v^2*(x - t0)- antsep^2)/2
}

# #' @param c0     [\code{numeric(1)}] Electromagnetic wave propagation in 
# #'               the air.

#' Depth to time conversion
#' 
#' Convert depth to the equivalent two-way travel time by accounting for the 
#' antenna separation between the transmitter and the receiver.
#' @param x      [\code{numeric(n)}] Depth vector.
#' @param t0     [\code{numeric(1)}] Time-zero.
#' @param v      [\code{numeric(1)}] Electromagnetic wave propagation in the 
#'               ground (or in the considered media).
#' @param antsep [\code{numeric(1)}] Antenna separation (distance between the
#'                transmitter and the receiver).
#' @return [\code{numeric}] Corresponding two-way travel time
#' @export
depthToTime <- function(x, t0, v = 0.1, antsep = 1){
  #FIXME
  # t0 <- t0 - antsep/c0
  sqrt((4*x^2 + antsep^2)/(v^2)) + t0
}

# #' @param c0     [\code{numeric(1)}] Electromagnetic wave propagation in 
# #'               the air.

#' Return the position of depth-zero on the two-way travel time axis
#' 
#' Useful if you want to plot a depth axis beside the two-way travel time axis.
#' @param t0     [\code{numeric(1)}] Time-zero.
#' @param v      [\code{numeric(1)}] Electromagnetic wave propagation in the 
#'               ground (or in the considered media).
#' @param antsep [\code{numeric(1)}] Antenna separation (distance between the
#'                transmitter and the receiver).
#' @return [\code{numeric}] Position of depth-zero on the two-way time axis
#' @export
depth0 <- function(t0 = 0, v = 0.1, antsep = 1){#, c0 = 0.299){
  # t0 - antsep/c0 + antsep/v
  t0 + antsep/v
}


.xlab <- function(x){
  paste0(x@xlab, " (", x@xunit, ")")
}
.ylab <- function(x){
  paste0(x@ylab, " (", x@yunit, ")")
}
.zlab <- function(x){
  paste0(x@zlab, " (", x@zunit, ")")
}
.dlab <- function(x){
  paste0(x@dlab, " (", x@dunit, ")")
}
.vlab <- function(x){
  paste0("Velocity (", x@xunit,"/", x@zunit, ")")
}

.subsetMat <- function(x, i){
  if(length(x) == 0) return(x)
  if(nrow(x) == 1) return(x)
  x[i,, drop = FALSE]
}

.subsetVec <- function(x, i){
  if(length(x) == 0) return(x)
  if(length(x) == 1) return(x)
  x[i]
}

.subsetClip <- function(x, i, j){
  if(!is.null(x@md[["clip"]])){
    test <- .clipMat(x@md[["clip"]], n = nrow(x@data))
    md_clip <- list()
    md_clip[["clipmin"]] <- apply(test[i, j, drop = FALSE], 2, 
                                  function(x) which(x == -1))
    md_clip[["clipmax"]] <- apply(test[i, j, drop = FALSE], 2, 
                                  function(x) which(x ==  1))
    return(md_clip)
  }else{
    return(NULL)
  }
}


#' Frequency of GSSI antenna
#' 
#' Given the antenna name, returns the frequency of GSSI antenna
#' @param x [\code{character}] Name(s) of the antenna(e)
#' @return [\code{list}] List of numeric values corresponding to the 
#'                       frequency/frequencies
#' @export
getAntFreqGSSI <- function(x){
  if(length(x) > 1){
    sapply(x, getAntFreqGSSI)
  }else{
    switch(x,
           '3200'      = NA, # adjustable
           '3200MLF'   = NA, # adjustable
           '500MHz'    = 500,
           '3207'      = 100,
           '3207AP'    = 100,
           '5106'      = 200,
           '5106A'     = 200,
           '50300'     = 300,
           '350'       = 350,
           '350HS'     = 350,
           '50270'     = 270,
           '50270S'    = 270,
           '50400'     = 400,
           '50400S'    = 400,
           '800'       = 800,
           '3101'      = 900,
           '3101A'     = 900,
           '51600'     = 1600,
           '51600S'    = 1600,
           '62000'     = 2000,
           '62000-003' = 2000,
           '62300'     = 2300,
           '62300XT'   = 2300,
           '52600'     = 2600,
           '52600S'    = 2600,
           'D50800'    = 800,
           NA)  # 800,300,
    
  }
}

#' Extract frequency from string
#' 
#' Extract with regex the antenna frequency in a string
#' @param s [\code{character}] Character string that may contain an 
#'                                indication of a frequency.
#' @return [\code{numeric}] The frequency (\code{NA} if no frequency value
#'                             is found)
#' @examples 
#' s <- "1230 fds 200-MHZ 12.3"
#' freqFromString(s) 
#' s <- "1230MLF"
#' freqFromString(s) 
#' s <- "D1230MLF"
#' freqFromString(s) 
#' @export
freqFromString <- function(s){
  if(length(s) > 1){
    unlist(sapply(s, freqFromString))
  }else{
    if(grepl("MHz", s, ignore.case = TRUE)){
      a <- regexpr("[0-9]+.MHZ",  s, ignore.case = TRUE, perl = TRUE)
    }else{
      a <- regexpr("[0-9]+",  s, ignore.case = TRUE, perl = TRUE)
    }
    if(a == -1){
      return(NA)
    }else{
      b <- regmatches(s,  a)
      return(as.numeric(gsub("[^0-9]", "", b)))
    }
  }
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


inPoly <- function(x, y, vertx, verty){
  inPo <- rep(0L, length(x))
  nvert <- length(vertx)
  for(i in 1:nvert){
    j <- ifelse(i==1, nvert,i-1)
    myTest <- ((verty[i] > y) != (verty[j]>y)) &
      (x < (vertx[j]-vertx[i]) * (y-verty[i]) / 
         (verty[j]-verty[i]) + vertx[i])
    inPo[myTest] <- !inPo[myTest]
  }
  return(inPo)
}

