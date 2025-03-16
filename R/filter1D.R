#' One dimensional filters
#' 
#' One dimensional filters
#' @param obj (`GPR* object`)
#' @param type (`character[1]`) Filter method.
#' @param w (`numeric[1]`)  Filter window width.
#' @param track (`logical[1]`) Should the processing step be tracked? 
#' @return (`GPR* object`)
#' @name filter1D
#' @rdname filter1D
#' @export
setGeneric("filter1D", 
                function(obj, 
                         type = c("runmed", "runmean", "mad", "gaussian", "hampel"), 
                         w = NULL,
                         track = TRUE) 
                  standardGeneric("filter1D"))


#' @rdname filter1D
#' @export
setMethod("filter1D", 
          "GPR", 
          function(obj, 
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
            
            dz <- mean(diff(obj@z))
            if(is.null(w)) w <- 10 * dz
            w <- round(w / dz)
            
            if(type == "runmed"){
              obj@data <- .runmmmMat(obj@data, w, type = "runmed")
            }else if(type == "runmean"){
              obj@data <- .runmmmMat(obj@data, w, type = "runmean")
            }else if(type == "gaussian"){
              objdata <- obj@data
              objDepth <- matrix(obj@z, byrow = FALSE, nrow = nrow(obj), ncol = ncol(obj))
              objTime0 <- matrix(obj@z0, byrow = TRUE, nrow = nrow(obj), ncol = ncol(obj))
              test <- objDepth <= objTime0
              # before_t0 <- obj@z <= mean(obj@z0)
              objdata[test] <- 0
              obj@data[!test] <- obj@data[!test] - mmand::gaussianSmooth(objdata, w)[!test]
            }else if(type == "hampel"){
              obj@data <- .runmmmMat(obj@data, w, type = "hampel")
            }
            if(isTRUE(track)) proc(obj) <- getArgs()
            return(obj)
          } 
)
