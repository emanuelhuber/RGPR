
#' Compute the Radon Transform of a 2D Image
#'
#' This function is an R wrapper for the C++ implementation `radon_transform_rcpp`.
#' It computes the discrete Radon transform (sinogram) of a given 2D numeric image.
#'
#' @param image_matrix Numeric matrix representing the image to be transformed.
#' @param n_theta Integer, number of projection angles (default 180).
#' @param n_rho Integer, number of distance bins (rho) in the sinogram (default 0, auto).
#' @param delta_x Numeric, spacing of pixels in the image coordinate system (default 1.0).
#' @param theta_min Numeric, minimum angle in radians (default 0).
#' @param theta_max Numeric, maximum angle in radians (default pi).
#' @param linear_interp Logical, if TRUE use linear interpolation along rho (default TRUE).
#' @param normalization Character, type of normalization: 'none', 'PET', or 'skimage' (default 'PET').
#' 
#' @return Numeric matrix: the Radon transform (sinogram) with rows corresponding to rho bins and columns to angles.
#'
#' @examples
#' # Generate a simple test image
#' n <- 64
#' img <- matrix(0, n, n)
#' mid <- n/2
#' img[mid, ] <- 1
#' img[, mid] <- 1
#'
#' # Compute sinogram
#' sino <- radon_wrapper(img, n_theta = 180, n_rho = 90)
#' image(t(sino[nrow(sino):1, ]), col = gray.colors(256), main = 'Sinogram')
#'
#' @export
radon <- function(image_matrix,
                  n_theta = 180,
                  n_rho = 0,
                  delta_x = 1.0,
                  theta_min = 0,
                  theta_max = pi,
                  linear_interp = TRUE,
                  normalization = c('PET', 'none', 'skimage')) {
  if(!is.matrix(image_matrix)) stop("image_matrix must be a numeric matrix.")
  if(!is.numeric(image_matrix)) stop("image_matrix must contain numeric values.")
  # Ensure parameters are correctly typed
  n_theta <- as.integer(n_theta)
  n_rho <- as.integer(n_rho)
  delta_x <- as.numeric(delta_x)
  theta_min <- as.numeric(theta_min)
  theta_max <- as.numeric(theta_max)
  linear_interp <- as.logical(linear_interp)
  
  # Match normalization argument
  normalization <- match.arg(normalization)
  norm_map <- c(none = 0L, PET = 1L, skimage = 2L)
  norm_value <- norm_map[[normalization]]
  
  # Call the underlying C++ implementation
  sinogram <- radon_transform_rcpp(
    A = image_matrix,
    n_theta = n_theta,
    n_rho = n_rho,
    delta_x = delta_x,
    theta_min = theta_min,
    theta_max = theta_max,
    rho_min_in = NA_real_,      # <-- default
    delta_rho_in = NA_real_,    # <-- default
    linear_interp = linear_interp,
    normalization = norm_value
  )
  # Return the sinogram
  return(sinogram)
}


#' Filtered Backprojection (FBP) for PET / CT
#'
#' This function reconstructs an image from a sinogram using filtered backprojection (FBP)
#' via a high-performance C++ implementation. Supports multiple filter types and normalization
#' conventions (PET vs scikit-image / MATLAB style).
#'
#' @param sinogram Numeric matrix of size n_rho x n_theta representing the sinogram.
#' @param N Integer. Number of rows in the reconstructed image.
#' @param M Integer. Number of columns in the reconstructed image.
#' @param delta_x Pixel spacing in x and y directions. Default is 1.
#' @param theta_min Minimum projection angle in radians. Default is 0.
#' @param theta_max Maximum projection angle in radians. Default is pi.
#' @param rho_min Minimum rho value (detector coordinate). Default is NA, automatically set.
#' @param delta_rho Spacing between rho bins. Default is NA, automatically set.
#' @param filter Character. Filter type: "ramp", "shepp-logan", "cosine", "hann". Default is "ramp".
#' @param normalization Character. Normalization scheme: "delta" (PET), "scikit" (scikit-image/MATLAB), "none" (average). Default is "delta".
#'
#' @return Numeric matrix of size N x M representing the reconstructed image.
#' @examples
#' # Small numeric example
#' sinogram <- matrix(1:9, nrow = 3, ncol = 3)
#' img_pet <- iradon_fbp_wrapper(sinogram, N = 3, M = 3, normalization = "delta")
#' img_sci <- iradon_fbp_wrapper(sinogram, N = 3, M = 3, normalization = "scikit")
#' img_pet
#' img_sci
#'
#' @export
radoninv <- function(sinogram,
                               N,
                               M,
                               delta_x = 1,
                               theta_min = 0,
                               theta_max = pi,
                               rho_min = NA,
                               delta_rho = NA,
                               filter = c("ramp", "shepp-logan", "cosine", "hann"),
                               normalization = c("delta", "scikit", "none")) {
  
  if (!requireNamespace("RcppEigen", quietly = TRUE)) {
    stop("RcppEigen package required but not installed")
  }
  
  # Match arguments
  filter <- match.arg(filter)
  normalization <- match.arg(normalization)
  
  # Ensure sinogram is numeric matrix
  sinogram <- as.matrix(sinogram)
  if (!is.numeric(sinogram)) stop("Sinogram must be numeric")
  
  # Call C++ function
  out <- iradon_transform_fbp_rcpp(
    R = sinogram,
    N = as.integer(N),
    M = as.integer(M),
    delta_x = delta_x,
    theta_min = theta_min,
    theta_max = theta_max,
    rho_min_in = rho_min,
    delta_rho_in = delta_rho,
    filter = filter,
    normalization = normalization
  )
  
  return(out)
}