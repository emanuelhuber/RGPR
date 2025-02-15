
#' @name scale1D
#' @rdname scale1D
setGeneric("scale1D", 
           function(x, 
                    type = c("stat", "min-max", "95", "eq", 
                             "sum", "rms", "mad", "invNormal"),
                    track = TRUE) 
             standardGeneric("scale1D"))



#' Scale the traces
#'
#' @name scale1D
#' @rdname scale1D
#' @export
setMethod("scale1D", 
          "GPR", 
          function(x,type = c("stat", "min-max", "95", "eq", 
                              "sum", "rms", "mad", "invNormal"),
                   track = TRUE){
            x@data <- scaleCol(x@data, type = type)
            if(isTRUE(track)) proc(x) <- getArgs()
            #   x@proc <- c(x@proc, proc)
            return(x)
          }
)



scaleCol <- function(A, type = c("stat", "min-max", "95",
                                 "eq", "sum", "rms", "mad", "invNormal")){
  A <-  as.matrix(A)
  test <- suppressWarnings(as.numeric(type))
  if(!is.na(test) && test >0 && test < 100){
    A_q95 <- apply(A, 2, quantile, test/100, na.rm = TRUE)
    A_q05 <- apply(A, 2, quantile, 1 - test/100, na.rm = TRUE)
    Ascl <- scale(A, center = FALSE, scale = A_q95 - A_q05)
    # matrix(A_q95 - A_q05, nrow = nrow(A), ncol = ncol(A), byrow=TRUE)
    #A <- A/Ascl
  }else{
    type <- match.arg(type)
    if( type == "invNormal"){
      Ascl <- apply( A, 2, .nScoreTrans)
      return(Ascl)
    }else if(type == "stat"){
      # A <- scale(A, center=.colMeans(A, nrow(A), ncol(A)), 
      #            scale = apply(A, 2, sd, na.rm = TRUE))
      Ascl <- scale(A)
    }else if(type == "sum"){
      Ascl <- scale(A, center=FALSE, scale = colSums(abs(A)))
    }else if(type == "eq"){
      # equalize line such each trace has same value for 
      # sqrt(\int  (A(t))^2 dt)
      Aamp <- matrix(apply((A)^2,2,sum), nrow = nrow(A), 
                     ncol = ncol(A), byrow=TRUE)
      Ascl <- A*sqrt(Aamp)/sum(sqrt(Aamp))
    }else if(type == "rms"){
      Ascl <- scale(A, center = FALSE)
      # Ascl <- matrix(apply(A ,2, .rms), nrow = nrow(A), 
      #                ncol = ncol(A), byrow=TRUE)
    }else if(type == "min-max"){  # min-max
      Ascl <- scale(A, center = FALSE, 
                    scale = apply(A, 2, max, na.rm = TRUE) - 
                      apply(A, 2, min, na.rm = TRUE))
    }else if(type == "mad"){  # mad
      Ascl <- scale(A, center = apply(A, 2, median), 
                    scale = apply(A, 2, mad))
    }
  }
  # FIXME: why did I write these 3 commented lines below?
  # test <- (!is.na(Ascl[1,] ) & abs(Ascl[1,]) > .Machine$double.eps^0.75) 
  # A[,test] <- Ascl[,test]
  # A[, !test] <- 0test <- (!is.na(Ascl[1,] ) & abs(Ascl[1,]) > .Machine$double.eps^0.75) 
  tst <- is.na(Ascl)
  A[!tst] <- Ascl[!tst]
  A[test] <- 0
  return(A)
}
