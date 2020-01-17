#--------------- EIGENIMAGE RECONSTRUCTION

setGeneric("eigenimages", function(x, center = TRUE, 
                                  scale = FALSE, track = TRUE) 
  standardGeneric("eigenimages"))

#' Eigenimage
#'
#' Decompose the GPR data (radargram) using singular value decomposition (SVD) 
#' and return the reconstructed data for the selected singular values 
#' (eigenvalues). This method is sometimes refered to in the litterature as 
#' the Karhunen-Loeve (KL) transformation or the eigenimage decomposition.
#'
#' @param x           An object of the class GPR
#' @param center      Logical or numeric. If \code{TRUE}, centering is done by 
#'                    subtracting the layer means (omitting \code{NA}'s), and 
#'                    if \code{FALSE}, no centering is done. If center is a 
#'                    numeric vector with length equal to the 
#'                    \code{nlayers(x)}, then each layer of \code{x} has the 
#'                    corresponding value from center subtracted from it.
#' @param scale       Logical or numeric. If \code{TRUE}, scaling is done by 
#'                    dividing the (centered) layers of \code{x} by their 
#'                    standard deviations if center is \code{TRUE}, and the 
#'                    root mean square otherwise. 
#'                    If scale is \code{FALSE}, no scaling is done. If scale is
#'                    a numeric vector with length equal to \code{nlayers(x)}, 
#'                    each layer of \code{x} is divided by the corresponding 
#'                    value. Scaling is done after centering.
#'
#' @return An object of the class GPR.
#' 
#' @references
#' \itemize{
#'   \item{Textbook: Sacchi (2002) Statistical and Transform Methods
#'         in Geophysical Signal Processing}
#'         }
#'         
#' @examples  
#' data(frenkeLine00)
#' x <- frenkeLine00
#' x1 <- eigenimage(x)
#' plot(x)
#' plot(x1[ , , 1])
#'
#' @name eigenimages
#' @rdname eigenimages
#' @export
setMethod("eigenimages", "GPR", function(x, center = TRUE, 
                                        scale = FALSE, track = TRUE){
  
  # ev <- unique(eigenvalue)
  X <- scale(x@data, center = center, scale = scale)
  Xsvd <- svd(X)
  xCenter <- attr(x, "scaled:center")
  xScale <- attr(x, "scaled:scale")
  lambda <- Xsvd$d^2
  .eigenimage <- function(i, x){
    x$d[i] * x$u[,i] %*% t(x$v[,i])
  }
  OUT <- lapply(seq_along(seq_len(ncol(X))), .eigenimage, Xsvd)
  OUT <- base::simplify2array(OUT, higher = TRUE)
  
  xs <- as.GPRset.GPR(x)
  xs@data <- OUT
  
  
  xs@sets <- lambda
  xs@setnames <- paste0("eigenimage ", seq_len(ncol(x)))
  xs@setunit <- "eigenvalue"
  xs@extraData <- list(center = attr(x, "scaled:center"),
                       scale  = attr(x, "scaled:scale"))
  # 
  # if(is.null(ev)){
  #   ev <- c(seq_len(ncol(X)))
  # }else if(!any(is.na(ev))){
  #   if(max(ev) > ncol(X)){
  #     stop("The number of eigenvalues selected cannot exceed the number ",
  #          "of GPR traces.")
  #   }
  #   if(min(ev) < 0){
  #     stop("The eigenvalues number must be strictly positive.")
  #   }
  # }
  # 
  # if(any(is.na(ev))){
  #   plot(seq_along(lambda), lambda, type = "b", col = "blue", pch = 16,
  #        xlab = "Eigenvalue Index", ylab = "Eigenvalue", 
  #        main = paste0(length(lambda), " eigenvalues"))
  #   ev <- readline(paste0("What eigenvalues do you want to use to reconstruct ",
  #                         "the radargram ?"))
  #   if(grepl(":", ev)){
  #     ev <- as.integer(eval(parse(text = ev)))
  #   }else{
  #     ev <- as.integer(unlist(strsplit(ev, split = ",")))
  #   }
  #   if(!is.integer(ev) || is.na(ev)){
  #     stop("Select the eigenvalues by typing (for example)\n",
  #          "either '2:12' or '1,2,3,4' or '10'.")
  #   }
  # }
  # 
  # d <- numeric(length(Xsvd$d))
  # d[ev] <- Xsvd$d[ev]
  # Xnew <- Xsvd$u %*% diag(d) %*% t(Xsvd$v)
  # 
  
  
  # Xeigen <- array(NA, dim = c(dim(Xsvd$u), length(ev)))
  # for(i in seq_along(ev)){
  #   Xeigen[,,i] <- Xsvd$d[ev[i]] * Xsvd$u[,ev[i]] %*% t(Xsvd$v[,ev[i]])
  # }
  # 
  # if(length(ev)>1){
  #   Xnew <- rowSums(Xeigen, dims=2)
  # } else{
  #   Xnew <- Xeigen[,,1]
  # }
  
  
  # if(!is.null(attr(X, 'scaled:scale')) && !is.null(attr(X, 'scaled:center'))){
  #   Xnew <- t(apply(Xnew, 1, function(r) r * attr(X, 'scaled:scale') + 
  #                     attr(X, 'scaled:center')))
  # }else if(is.null(attr(X, 'scaled:scale')) && 
  #          !is.null(attr(X, 'scaled:center'))){
  #   Xnew <- t(apply(Xnew , 1, function(r) r + attr(X, 'scaled:center')))
  # }else if(!is.null(attr(X, 'scaled:scale')) && 
  #          is.null(attr(X, 'scaled:center'))){
  #   Xnew <- t(apply(Xnew , 1, function(r) r * attr(X, 'scaled:scale')))
  # }
  
  # x@data <- unscale(X, Xnew)
  
  # if(is.null(eigenvalue)){
  #   proc(x) <- getArgs(addArgs = list('eigenvalue' = eigenvalue, 
  #                                     'center' = as.character(center), 
  #                                     'scale' = as.character(scale)))
  # } else{
  #   proc(x) <- getArgs(addArgs = list('eigenvalue' = ev, 
  #                                     'center' = as.character(center), 
  #                                     'scale' = as.character(scale)))
  # }
  if(isTRUE(track)) proc(xs) <- getArgs()
  
  return(xs)
} 
)
