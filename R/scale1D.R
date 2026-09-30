#' Scale the traces
#' 
#' Scale the traces
#' @param obj (`GPR* object`)
#' @param type (`character[1]`) Type of scaling
#' @param track (`logical[1]`) Should the processing step be tracked? 
#' @name scale1D
#' @rdname scale1D
#' @export
setGeneric("scale1D", 
           function(obj, 
                    type = c("stat", "min-max", "95", "eq", 
                             "sum", "rms", "mad", "invNormal"),
                    track = TRUE) 
             standardGeneric("scale1D"))



#' @rdname scale1D
#' @export
setMethod("scale1D", 
          "GPR", 
          function(obj,type = c("stat", "min-max", "95", "eq", 
                              "sum", "rms", "mad", "invNormal"),
                   track = TRUE){
            obj@data <- scaleCol(obj@data, type = type)
            if(isTRUE(track)) proc(obj) <- getArgs()
            return(obj)
          }
)


#' Scale the columns of a numeric matrix
#'
#' Applies a selected scaling or normalization method independently to each
#' column of a numeric matrix. In the context of GPR data, each column is
#' generally interpreted as an individual trace.
#'
#' The normal-score transformation (`type = "invNormal"`) replaces the
#' empirical distribution of each trace with an approximately Gaussian
#' distribution while preserving the original mean and standard deviation.
#' Equal input values receive equal transformed values.
#'
#' A numeric value between 0 and 100 can also be supplied through `type`.
#' In that case, each column is divided by the difference between two
#' complementary quantiles. For example, `"95"` uses the difference between
#' the 95th and 5th percentiles.
#'
#' @param A A numeric matrix or an object coercible to a numeric matrix.
#' Columns are scaled independently.
#'
#' @param type Character or numeric scalar defining the scaling method.
#' Available character methods are:
#'
#' \describe{
#' \item{`"stat"`}{
#' Standardize each column by subtracting its mean and dividing by its
#' standard deviation.
#' }
#' \item{`"min-max"`}{
#' Divide each column by its range, without subtracting the minimum.
#' }
#' \item{`"95"`}{
#' Divide each column by the difference between the 95th and 5th
#' percentiles. Other numeric percentages between 0 and 100 may also
#' be supplied.
#' }
#' \item{`"eq"`}{
#' Apply trace-energy equalization based on the sum of squared
#' amplitudes.
#' }
#' \item{`"sum"`}{
#' Divide each column by the sum of its absolute amplitudes.
#' }
#' \item{`"rms"`}{
#' Divide each column by its root-mean-square amplitude. This corresponds
#' to the scaling factor used by [base::scale()] when `center = FALSE`.
#' }
#' \item{`"mad"`}{
#' Center each column on its median and divide it by its median absolute
#' deviation.
#' }
#' \item{`"invNormal"`}{
#' Apply a rank-based normal-score transformation independently to each
#' column.
#' }
#' }
#'
#' @return A numeric matrix with the same dimensions and dimnames as `A`.
#' Non-finite values introduced by undefined scaling factors, such as
#' scaling a constant trace, are replaced by zero. Existing missing values
#' are preserved by the normal-score transformation.
#'
#' @details
#' Scaling is performed independently for each column.
#'
#' For `type = "invNormal"`, average ranks are assigned to tied amplitudes.
#' Consequently, identical input amplitudes receive identical normal scores.
#' This avoids interpolation warnings caused by duplicated amplitude values.
#'
#' The normal-score transformation is nonlinear. It changes relative
#' amplitudes within a trace and should therefore be used cautiously when
#' amplitudes have a physical interpretation. It is generally more
#' appropriate for visualization or distribution normalization than for
#' amplitude-preserving processing.
#'
#' @seealso [base::scale()], [stats::quantile()]
#'
#' @examples
#' A <- matrix(
#' c(
#' 1, 1, 2, 3, 4,
#' 2, 3, 4, 5, 6
#' ),
#' nrow = 5,
#' ncol = 2
#' )
#'
#' # Standardize each column
#' scaleCol(A, type = "stat")
#'
#' # Divide each column by its amplitude range
#' scaleCol(A, type = "min-max")
#'
#' # Rank-based normal-score transformation
#' scaleCol(A, type = "invNormal")
#'
#' # Scale using the difference between the 90th and 10th percentiles
#' scaleCol(A, type = "90")
#'
#' @keywords internal
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