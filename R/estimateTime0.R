#' @name estimateTime0
#' @rdname estimateTime0
#' @export
setGeneric("estimateTime0",
                function(x, method = c("coppens", "threshold", "MER"), 
                         thr = NULL, w = NULL, ns = NULL, bet = NULL, 
                         shorten = TRUE, c0 = 0.299, 
                         FUN = NULL, ..., track = TRUE)
                  standardGeneric("estimateTime0"))


#' Estimate and set time-zero
#' 
#' `estimateTime0` estimates for each trace individually the first wave 
#' break, computes the corresponding time-zero knowing the propagation speed
#' of the electromagnetic wave through air and returns an object of the class
#' `GPR` with updated time-zero. It is possible to apply a function 
#' provided by the user (e.g., `FUN`) on time-zero (e.g., to set time-zero
#' equal to the average value of the time-zeros computed for every traces; in
#' this case, all traces would have the same time-zero).
#' 
#' This function is a wrapper for the following commands:
#' * `tfb <- pickFirstBreak(x, ...)`
#' * `t0 <- firstBreakToTime0(x, tfb)`
#' * `time0(x) <- t0` (if `FUN` is not `NULL` else `time0(x) <- FUN(t0, ...)`)
#' 
#' 
#' Modified slots:
#' * `time0`: new estimated time-zero.
#' * `proc`: updated with function name and arguments.
#'
#' @param x (`GPR`) An object of the class `GPR
#' @param method (`character[1]) Method to be applied (either
#'              `"coppens"`, `"threshold"` or `"MER"`). 
#'              `"coppens"` corresponds to the modified Coppens method, 
#'              `"threshold"` to the threshold method, 
#'              and `"MER"` to the modified energy ratio method.
#' @param thr (`numeric[1]`] Threshold for the signal 
#'              amplitude (in percent) at which time zero is picked (only for the
#'              threshold method). 
#'              `thr` ranges between 0 (0 percent) and 1 (100 percent).
#' @param w (`numeric[1]`) Length of the short-term leading window in 
#'          unit of time (only for `method = "coppens"` or 
#'          `method = "MER"`). 
#'          Recommended value: about one period of the first-arrival 
#'          waveform.
#' @param ns (`numeric[1]`] Length of the edge preserving smoothing 
#'           window in unit of time (only for  `method = "coppens"`). 
#'           Recommended value: between one and two signal periods.
#'           When `ns = NULL` the value of `ns` is set to 
#'           `1.5 * w`.
#' @param bet (`numeric[1]`) Stabilisation constant (only for 
#'            `method = "coppens"`). Not critical. 
#'            When `bet = NULL` the value of `bet` is set to 
#'            20 percent of the maximal signal amplitude.
#' @param shorten (`logical[1]`) If `TRUE`, each trace is shortened
#'                 by removing the samples that are \eqn{2 \times w} after the
#'                 maximum value (only for `method = "coppens"` or 
#'                 `method = "MER"`). You may set 
#'                 `shorten = FALSE` if the first wave break occurs 
#'                 after the maximum absolute amplitude time.
#' @param c0     (`numeric[1]`) Propagation speed of the GPR wave 
#'               through air in unit of space per unit of time 
#'               (generally in m/ns).
#' @param FUN (`function()`) A function to apply on the 
#'            estimated time-zero of every traces (e.g., `mean` or 
#'            `median` to get set a single time-zero value to the data).
#' @param ...  Further arguments to be passed to `FUN`.
#'  
#' @return (`GPR`) An object of the class `GPR`.
#'          
#' @seealso [pickFirstBreak()] to estimate the first wave break;
#'          [firstBreakToTime0()] to convert the first wave break
#'          into time zero.
#'          [time0()] and [setTime0()] to set time-zero;
#'          [shiftToTime0()] to shift the traces such that they start
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
#' @concept processing
setMethod("estimateTime0", "GPR", 
          function(x, method = c("coppens", "threshold", "MER"), 
                   thr = NULL, w = NULL, ns = NULL, bet = NULL, 
                   shorten = TRUE, c0 = 0.299, 
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
            msg <- checkArg(method,  msg, "STRING_CHOICE", 
                            c("coppens", "threshold",  "MER"))
            msg <- checkArg(thr,     msg, "PERCENT1_NULL")
            msg <- checkArg(w,       msg, "NUMERIC1_SPOS_NULL", max(x@z)/2)
            # msg <- checkArg(ns     , msg, "NUMERIC1_SPOS_NULL", round((nmax - 1) * x@dz))
            msg <- checkArg(ns,      msg, "NUMERIC1_SPOS_NULL", max(x@z))
            msg <- checkArg(bet,     msg, "NUMERIC1_SPOS_NULL", Inf)
            msg <- checkArg(shorten, msg, "LOGICAL_LEN", 1)
            msg <- checkArg(c0,      msg, "NUMERIC1_SPOS", Inf)
            msg <- checkArg(FUN,     msg, "FUNCTION_NULL")
            checkArgStop(msg)
            #-----------------------------------
            
            tfb <- pickFirstBreak(x, method = method, thr = thr, w = w, 
                              ns = ns, bet = bet, shorten = shorten)
            t0 <- firstBreakToTime0(x, tfb, c0 = c0)
            
            if(!is.null(FUN)) t0 <- FUN(t0, ...)
            
            x <- setTime0(x, t0, track = FALSE)
            
            # x@proc <- x@proc[-length(x@proc)] # remove proc "time0()<-"
            if(isTRUE(track)) proc(x) <- getArgs()
            return(x)
          })