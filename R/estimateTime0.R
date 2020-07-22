time0Estimation <- function(...){
  stop("DEPRECATED!\n",
       "Use 'estimateTime0()' instead.")
}


#' @name estimateTime0
#' @rdname estimateTime0
#' @export
setGenericVerif("estimateTime0",
                function(x, method = c("coppens", "threshold", "MER"), 
                         thr = 0.12, w = 11, ns = NULL, bet = NULL, c0 = 0.299, 
                         FUN = NULL, ..., track = TRUE)
                  standardGeneric("estimateTime0"))


#' Estimate and set time-zero
#' 
#' \code{estimateTime0} estimates for each trace individually the first wave 
#' break, computes the corresponding time-zero knowing the propagation speed
#' of the electromagnetic wave through air and returns an object of the class
#' \code{GPR} with updated time-zero. It is possible to apply a function 
#' provided by the user (e.g., \code{FUN}) on time-zero (e.g., to set time-zero
#' equal to the average value of the time-zeros computed for every traces; in
#' this case, all traces would have the same time-zero).
#' 
#' This function is a wrapper for the following commands
#' \itemize{
#'   \item \code{tfb <- firstBreak(x, ...)}
#'   \item \code{t0 <- firstBreakToTime0(tfb, x)}
#'   \item \code{time0(x) <- t0} (if \code{FUN} is not \code{NULL}
#'          \code{time0(x) <- FUN(t0, ...)})
#' }
#' 
#' Modified slots
#' \itemize{
#'   \item \code{time0}: new estimated time-zero.
#'   \item \code{proc}: updated with function name and arguments.
#' }
#'
#' @param x [\code{GPR class}] An object of the class \code{GPR}
#' @param method [\code{character(1)}] Method to be applied (either
#'              \code{coppens}, \code{threshold} or \code{MER}). 
#'              \code{"coppens"} corresponds to the modified Coppens method, 
#'              \code{"threshold"} to the threshold method, 
#'              and \code{"MER"} to the modified energy ratio method.
#' @param thr [\code{numeric(1)}] Threshold for the signal 
#'              amplitude (in \%) at which time zero is picked (only for the
#'              threshold method). \code{thr} ranges between 0 and 1.
#' @param w [\code{numeric(1)}] Length of the leading window in unit of time
#'          (only for the modified Coppens and modified energy ratio 
#'          methods). Recommended value: about one period of the first-arrival 
#'          waveform.
#' @param ns [\code{numeric(1)}] Length of the edge preserving smoothing 
#'           window in unit of time (only for the modified Coppens 
#'           method). Recommended value: between one and two signal periods.
#'           When \code{ns = NULL} the value of \code{ns} is set to 
#'           \code{1.5 * w}.
#' @param bet [\code{numeric(1)}] Stabilisation constant (only for the 
#'            modified Coppens method). Not critical. 
#'            When \code{bet = NULL} the value of \code{bet} is set to 
#'            20\% of the maximal signal amplitude. 
#' @param c0     [\code{numeric(1)}] Propagation speed of the GPR wave 
#'               through air in unit of space per unit of time 
#'               (generally in m/ns).
#' @param FUN [\code{function}] A function to apply on the 
#'            estimated time-zero of every traces (e.g., \code{mean} or 
#'            \code{median} to get set a single time-zero value to the data).
#' @param ... [\code{ANY}] Further arguments to be passed to \code{FUN}.
#'  
#' @return [\code{GPR class}] An object of the class \code{GPR}.
#'          
#' @seealso \code{\link{firstBreak}} to estimate the first wave break;
#'          \code{\link{firstBreakToTime0}} to convert the first wave break
#'          into time zero.
#'          \code{\link{time0}} and \code{\link{setTime0}} to set time-zero;
#'          \code{\link{time0Cor}} to shift the traces such that they start
#'          at time-zero.
#'          
#' @examples 
#' data("frenkeLine00")
#' x <- frenkeLine00
#' x1 <- estimateTime0(x, w = 10)
#' time0(x1)
#' x2 <- estimateTime0(x, w = 10, FUN = mean)
#' time0(x2)
#' 
#' @name estimateTime0
#' @rdname estimateTime0
#' @export
setMethod("estimateTime0", "GPR", 
          function(x, method = c("coppens", "threshold", "MER"), 
                   thr = 0.12, w = 11, ns = NULL, bet = NULL, c0 = 0.299, 
                   FUN = NULL, ..., track = TRUE){
            
            method <- method[1]
            
            # # shorten the file -> computed only for argument checking
            # nmax <- nrow(x)
            # tst <- which(as.matrix(x) == max(x), arr.ind = TRUE)
            # if(length(tst) > 0 ){
            #   nmax <- max(tst[,"row"])
            # }
            
            #------------------- check arguments
            msg <- checkArgInit()
            msg <- checkArg(method, msg, "STRING_CHOICE", 
                            c("coppens", "threshold",  "MER"))
            msg <- checkArg(thr   , msg, "PERCENT1")
            # msg <- checkArg(w     , msg, "NUMERIC1_SPOS", round((nmax - 1) * x@dz/1.5))
            msg <- checkArg(w     , msg, "NUMERIC1_SPOS", max(x@depth)/2)
            msg <- checkArg(ns    , msg, "NUMERIC1_SPOS_NULL", round((nmax - 1) * x@dz))
            msg <- checkArg(bet   , msg, "NUMERIC1_SPOS_NULL", Inf)
            msg <- checkArg(c0    , msg, "NUMERIC1_SPOS", Inf)
            msg <- checkArg(FUN   , msg, "FUNCTION_NULL")
            checkArgStop(msg)
            #-----------------------------------
            
            tfb <- firstBreak(x, method = method, thr = thr, w = w, 
                              ns = ns, bet = bet)
            t0 <- firstBreakToTime0(tfb, x, c0 = c0)
            
            if(!is.null(FUN)) t0 <- FUN(t0, ...)
            
            x <- setTime0(x, t0, track = FALSE)
            
            # x@proc <- x@proc[-length(x@proc)] # remove proc "time0()<-"
            if(isTRUE(track)) proc(x) <- getArgs()
            return(x)
          })