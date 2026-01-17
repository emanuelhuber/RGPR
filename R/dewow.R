#' Trace dewowing
#' 
#' `dewow` remove the low-frequency component (the so-called 'wow') of 
#' every traces.
#' 
#' The low-frequency component is computed by different methods:
#'   * `runmed` running median based on [stats::runmed]
#'   * `runmean` running mean based on [stats::filter]
#'   * `MAD` DEPRECATED - Median Absolute Deviation filter
#'   * `Gaussian` Gaussian smoothing applied to the trace samples
#'         after time-zero based on [mmand::gaussianSmooth]
#' 
#' Modified slots:
#'   * `data`: trace dewowed.
#'   * `proc`: updated with function name and arguments.
#' 
#' @param obj    (`GPR`) An object of the class GPR.
#' @param type (`character[1]`) Dewow method,
#'             one of `runmed` (running median),
#'             `runmean` (running mean),
#'             `Gaussian` (Gaussian smoothing).
#' @param w    (`numeric[1]`) If `type` = `runmed`, 
#'             `MAD` or `runmean`, window length of the filter in
#'             trace unit;
#'             If `type` = `Gaussian`, standard deviation in trace
#'             unit.
#'             If `w = NULL`, `w` is estimated as five times the 
#'             wavelength corresponding to the maximum frequency of obj 
#'             (estimated with [spec])
#' @param track (`logical[1]`) Should the processing step be tracked?
#' @return (`GPR`) An object of the class GPR whose traces are dewowed.
#' @name dewow
#' @rdname dewow
#' @export
#' @concept processing
setGeneric("dewow", 
           function(obj, type = c("runmed", "runmean", 
                                "gaussian"), 
                    w = NULL, track = TRUE)
             standardGeneric("dewow"))

#' @rdname dewow
#' @export
setMethod("dewow", "GPR", function(obj, type = c("runmed", "runmean", 
                                               "gaussian"), 
                                   w = NULL, track = TRUE){
  # type <- match.arg(type, c("MAD", "Gaussian"))
  type <- tolower(type[1])
  
  #------------------- check arguments
  msg <- checkArgInit()
  msg <- checkArg(type, msg, 
                  "STRING_CHOICE", 
                  c("runmed", "runmean", "gaussian"))
  msg <- checkArg(w,    msg, "NUMERIC1_SPOS_NULL", Inf)
  checkArgStop(msg)
  #-----------------------------------
  
  dz <- mean(diff(obj@z))
  
  if(is.null(w)){
    # argument initialization
    # pulse width in ns, (obj@freq is in MHz)
    
    # FIXME
    stop("YOU MUST FIRST INTEGRATE FUNCTION 'spec' IN RGPR")
    
    # a <- RGPR::spec(obj, plotSpec = FALSE, unwrapPhase = FALSE)
    # freq <- a$freq[which.max(rowMeans(a$pow))]
    # pw <- 1/(freq * 10^6)/10^-9
    # w <- round((5 * pw)/dz)
  }else{
    w <- round(w / dz)
  }
  if(type == "runmed"){
    obj@data <- obj@data - .runmmmMat(obj@data, w, type = "runmed")
  }else if(type == "runmean"){
    obj@data <- obj@data - .runmmmMat(obj@data, w, type = "runmean")
  }else if(type == "gaussian"){
    xdata <- obj@data
    xDepth <- matrix(obj@z, byrow = FALSE, nrow = nrow(obj), ncol = ncol(obj))
    xTime0 <- matrix(obj@z0, byrow = TRUE, nrow = nrow(obj), ncol = ncol(obj))
    test <- xDepth <= xTime0
    # before_t0 <- obj@z <= mean(obj@z0)
    xdata[test] <- 0
    obj@data[!test] <- obj@data[!test] - mmand::gaussianSmooth(xdata, w)[!test]
  }
  if(isTRUE(track)) proc(obj) <- getArgs()
  return(obj) 
})
