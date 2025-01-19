
#' @name filter1D
#' @rdname filter1D
setGeneric("filter1D", 
                function(x, 
                         type = c("runmed", "runmean", "MAD", "Gaussian"), 
                         w = NULL,
                         track = TRUE) 
                  standardGeneric("filter1D"))


#' One dimensional filters
#' 
#' @name filter1D
#' @rdname filter1D
#' @export
setMethod("filter1D", 
          "GPR", 
          function(x, 
                   type = c("runmed", "runmean", "mad", "gaussian", "hampel"), 
                   w = NULL, track = TRUE){
            # type <- match.arg(type, c("MAD", "Gaussian"))
            type <- tolower(type[1])
            
            #------------------- check arguments
            msg <- checkArgInit()
            msg <- checkArg(type, msg, "STRING_CHOICE", c("runmed", "runmean",
                                                          "mad", "gaussian", "hampel"))
            msg <- checkArg(w,    msg, "NUMERIC1_SPOS_NULL", Inf)
            checkArgStop(msg)
            #-----------------------------------
            
            dz <- mean(diff(x@z))
            if(is.null(w)) w <- 10 * dz
            w <- round(w / dz)
            
            if(type == "runmed"){
              x@data <- .runmmmMat(x@data, w, type = "runmed")
            }else if(type == "runmean"){
              x@data <- .runmmmMat(x@data, w, type = "runmean")
            }else if(type == "gaussian"){
              xdata <- x@data
              xDepth <- matrix(x@z, byrow = FALSE, nrow = nrow(x), ncol = ncol(x))
              xTime0 <- matrix(x@z0, byrow = TRUE, nrow = nrow(x), ncol = ncol(x))
              test <- xDepth <= xTime0
              # before_t0 <- x@z <= mean(x@z0)
              xdata[test] <- 0
              x@data[!test] <- x@data[!test] - mmand::gaussianSmooth(xdata, w)[!test]
            }else if(type == "hampel"){
              x@data <- .runmmmMat(x@data, w, type = "hampel")
            }
            if(isTRUE(track)) proc(x) <- getArgs()
            return(x)
          } 
)
