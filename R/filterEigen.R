#' Eigenimage filter
#'
#' Decompose the GPR data (radargram) using singular value decomposition (SVD) 
#' and return the reconstructed data for the selected singular values 
#' (eigenvalues). This method is sometimes refered to in the litterature as 
#' the Karhunen-Loeve (KL) transformation or the eigenimage decomposition.
#'
#' @param obj           An object of the class GPR
#' @param eigenvalue  An integer vector specifying the eigenvalues selected. 
#'                    \code{eigenvalue = 1:5} returns the image reconstructed 
#'                    using the first five eigenvalues while \code{c(2,4,6)}
#'                    will return the image reconstructed for these three 
#'                    specific eigenvalues. 
#'                    If \code{NA}, a plot of the eigenvalues spectrum 
#'                    will be displayed and the user will be prompted to enter 
#'                    the pair of selected eigenvalues separated by a comma. 
#'                    That is for example \code{1,2} will return the image 
#'                    reconstructred using tthe first two eigenvalues. 
#'                    If \code{NULL}, the image is reconstructed using 
#'                    all eigenvalues.
#'                    The number of eigenvalue is equal to \code{min(dim(x))}.
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
#'                    each layer of \code{obj} is divided by the corresponding 
#'                    value. Scaling is done after centering.
#'
#' @return An object of the class GPR.
#' 
#' @references
#' \itemize{
#'   \item{Textbook: Sacchi (2002) Statistical and Transform Methods
#'         in Geophysical Signal Processing}
#'         }
setGeneric("filterEigen", function(obj, eigenvalue = NA, center = TRUE,
                                        scale = FALSE, track = TRUE)
  standardGeneric("filterEigen"))


#' @name filterEigen
#' @rdname filterEigen
#' @export
setMethod("filterEigen", "GPR", function(obj, eigenvalue = NA, center = TRUE, 
                                         scale = FALSE, track = TRUE){
  
  ev <- unique(eigenvalue)
  OBJ <- scale(obj@data, center = center, scale = scale)
  OBJsvd <- svd(OBJ)
  lambda <- OBJsvd$d^2
  
  if(any(is.na(ev))){
    plot(seq_along(lambda), lambda, type = "b", col = "blue", pch = 16,
         xlab = "Eigenvalue Index", ylab = "Eigenvalue", 
         main = paste0(length(lambda), " eigenvalues"))
    ev <- readline(paste0("What eigenvalues do you want to use to reconstruct ",
                          "the radargram ?"))
    if(grepl(":", ev)){
      ev <- as.integer(eval(parse(text = ev)))
    }else{
      ev <- as.integer(unlist(strsplit(ev, split = ",")))
    }
    if(!is.integer(ev) || is.na(ev)){
      stop("Select the eigenvalues by typing (for example)\n",
           "either '2:12' or '1,2,3,4' or '10'.")
    }
  }
  
  if(is.null(ev)){
    ev <- seq_len(min(dim(OBJ)))
  }else if(!any(is.na(ev))){
    if(max(ev) > min(dim(OBJ))){
      stop("The number of eigenvalues selected cannot exceed ",
           "'min(dim(x))'.")
    }
    if(min(ev) < 0){
      stop("The eigenvalues number must be strictly positive.")
    }
  }
  
  
  d <- numeric(length(OBJsvd$d))
  d[ev] <- OBJsvd$d[ev]
  OBJnew <- OBJsvd$u %*% diag(d) %*% t(OBJsvd$v)

  
  obj@data <- unscale(OBJ, OBJnew)
  
  if(isTRUE(track)) proc(obj) <- getArgs()
  
  return(obj)
} 
)