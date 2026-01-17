################################################################################
#' Two-dimensional processing methods for GPR objects (filter2D family)
#'
#' A collection of 2-D processing methods that operate on objects inheriting
#' from \code{GPRvirtual} (including \code{GPR}, \code{GPRslice}, \code{GPRcube}).
#'
#' Each method has the form \code{filter2D<name>()} and an S4 method for the
#' \code{GPRvirtual} parent class that applies the operation to \code{obj@data}.
#' For \code{GPRcube} objects the operation is applied slice-by-slice.
#'
#' The functions include classic filters (median3x3, gaussian) and wrappers for
#' common \pkg{imager} processing functions (isoblur, medianblur, anisotropic,
#' diffusion_tensors, imgradient) plus a small FFT-based low-pass
#' destriper.
#'
#' @param obj (`GPR* object`) An object inheriting from \code{GPRvirtual}.
#'   The slot \code{obj@data} must be a numeric matrix (for \code{GPR} /
#'   \code{GPRslice}) or a 3D numeric array with slices along the 3rd
#'   dimension (for \code{GPRcube}).
#' @param ... Additional parameters passed to the underlying processing
#'   implementation (see each function for details).
#' @param track (`logical(1)`) If \code{TRUE} the processing call is recorded
#'   using \code{proc(obj) <- getArgs()} (package-specific tracking).
#'
#' @details
#' The S4 methods implemented here rely on an internal helper
#' \code{.filter2D_apply(obj, FUN, ...)} that dispatches to the appropriate
#' behaviour depending on whether \code{obj} is a \code{GPR}, \code{GPRslice},
#' or \code{GPRcube}. For \code{GPRcube} each slice \code{obj@data[,,i]} is
#' processed independently.
#'
#' Many functions rely on the \pkg{imager} package. If \pkg{imager} is not
#' installed the corresponding functions will throw an informative error
#' telling the user to install \pkg{imager}.
#'
#' @name filter2D
#' @rdname filter2D
#' @aliases filter2Dmedian3x3 filter2Dadimpro filter2Dgaussian
#'          filter2Disoblur filter2Dmedianblur filter2Danisotropic
#'          filter2Ddiffusiontensors filter2Dgradient
#'          filter2Dfftlowpass
#' @concept processing
#' @export
NULL
################################################################################

# -------------------------------------------------------------------------
# Helper: apply FUN to obj@data respecting classes (GPR, GPRslice, GPRcube)
# -------------------------------------------------------------------------
.filter2D_apply <- function(obj, FUN, track = TRUE, ...){
  # FUN: function(A, ...) returning processed numeric matrix of same dims
  if (!is.function(FUN)) stop("FUN must be a function(A, ...) returning a matrix")
  cl <- class(obj)[1]
  if (cl %in% c("GPR", "GPRslice")) {
    obj@data <- FUN(obj@data, ...)
    if (isTRUE(track)) proc(obj) <- getArgs()
    return(obj)
  } else if (cl == "GPRcube") {
    # apply slice by slice
    d3 <- dim(obj)[3]
    for (i in seq_len(d3)) {
      obj@data[,,i] <- FUN(obj@data[,,i], ...)
    }
    if (isTRUE(track)) proc(obj) <- getArgs()
    return(obj)
  } else {
    stop(sprintf("Unsupported class '%s' for .filter2D_apply", cl))
  }
}

# -------------------------------------------------------------------------
# Utility conversions (matrix <-> cimg)
# -------------------------------------------------------------------------
.mat_to_cimg <- function(A){
  if (!is.matrix(A)) stop("Input must be a numeric matrix")
  if (!requireNamespace("imager", quietly = TRUE)) {
    stop("Please install the 'imager' package to use this function.")
  }
  # imager::as.cimg preserves orientation: columns -> x, rows -> y
  imager::as.cimg(A)
}
.cimg_to_mat <- function(im){
  arr <- as.array(im)
  # arr dims: x,y,z,cc ; we typically expect z=1,cc=1 for grayscale
  if (length(dim(arr)) == 2) return(arr)
  if (length(dim(arr)) >= 4) return(drop(arr[,,1,1]))
  if (length(dim(arr)) == 3) return(drop(arr[,,1]))
  stop("Unexpected cimg structure")
}

# -------------------------------------------------------------------------
# small custom median 3x3 (keeps same edges behaviour as original)
# -------------------------------------------------------------------------
.medianFilter3x3_mat <- function(A){
  # vectorized implementation using imager if available, else fallback
  if (requireNamespace("imager", quietly = TRUE)) {
    im <- .mat_to_cimg(A)
    out <- imager::medianblur(im, n = 3)
    return(.cimg_to_mat(out))
  } else {
    # simple R fallback (naive loops)
    nr <- nrow(A); nc <- ncol(A)
    B <- A
    for (i in 1:(nr-2)){
      for (j in 1:(nc-2)){
        xm <- A[i + 0:2, j + 0:2]
        B[i+1, j+1] <- xm[order(xm)[5]]
      }
    }
    return(B)
  }
}

# -------------------------------------------------------------------------
# Custom FFT low-pass along columns (useful for destriping)
# -------------------------------------------------------------------------
.filter2D_fft_lowpass_mat <- function(A, cutoff = 0.04, strength = 0.85){
  n <- nrow(A)
  # build mask in centered frequency coordinates then shift to R FFT order
  freqs_centered <- ((0:(n-1)) - floor(n/2)) / n
  mask_centered <- rep(1, n)
  mask_centered[abs(freqs_centered) < cutoff] <- 1 - strength
  # shift
  mask <- c(mask_centered[(floor(n/2)+1):n], mask_centered[1:floor(n/2)])
  Aout <- apply(A, 2, function(col){
    nas <- is.na(col)
    if (any(nas)){
      idx <- seq_along(col)
      col[nas] <- approx(idx[!nas], col[!nas], idx[nas], rule = 2)$y
    }
    F <- stats::fft(col)
    F2 <- F * mask
    Re(stats::fft(F2, inverse = TRUE) / length(F2))
  })
  dimnames(Aout) <- dimnames(A)
  Aout
}

# -------------------------------------------------------------------------
# GENERIC / METHOD: median3x3
# -------------------------------------------------------------------------
#' Two-dimensional median 3x3 filter
#'
#' Apply a 3x3 median filter to \code{obj@data}.
#'
#' @rdname filter2D
#' @export
setGeneric("filter2Dmedian3x3", function(obj, ..., track = TRUE)
  standardGeneric("filter2Dmedian3x3"))

#' @rdname filter2D
#' @export
setMethod("filter2Dmedian3x3", "GPRvirtual", function(obj, ..., track = TRUE){
  .filter2D_apply(obj, FUN = .medianFilter3x3_mat, track = track, ...)
})

# -------------------------------------------------------------------------
# GENERIC / METHOD: adimpro (your existing adimpro wrapper)
# -------------------------------------------------------------------------
#' Two-dimensional adimpro anisotropic smoothing
#'
#' Wrapper around \pkg{adimpro} anisotropic smoothing (awsaniso).
#'
#' @param ... Passed to \code{adimpro::awsaniso()} (e.g. \code{hmax}, \code{sigma}, ...).
#' @rdname filter2D
#' @export
setGeneric("filter2Dadimpro", function(obj, ..., track = TRUE)
  standardGeneric("filter2Dadimpro"))

#' @rdname filter2D
#' @export
setMethod("filter2Dadimpro", "GPRvirtual", function(obj, ..., track = TRUE){
  .filter2D_apply(obj, FUN = function(A, ...){
    if (!requireNamespace("adimpro", quietly = TRUE)) {
      stop("Please install the 'adimpro' package to use filter2Dadimpro().")
    }
    IMG <- (A - min(A, na.rm = TRUE)) / (diff(range(A, na.rm = TRUE)))
    adimg <- adimpro::make.image(IMG)
    img.smooth <- adimpro::awsaniso(adimg, ...)
    AA <- adimpro::extract.image(img.smooth)
    # rescale to original sd & mean
    AA_scaled <- ((AA - mean(AA, na.rm = TRUE)) / sd(AA, na.rm = TRUE)) * sd(A, na.rm = TRUE) + mean(A, na.rm = TRUE)
    AA_scaled
  }, track = track, ...)
})

# -------------------------------------------------------------------------
# GENERIC / METHOD: gaussian (mmand::gaussianSmooth)
# -------------------------------------------------------------------------
#' Two-dimensional Gaussian smoothing
#'
#' Apply a 2-D Gaussian smoothing using \pkg{mmand} (or fallback to imager).
#'
#' @param ... parameters passed to \code{mmand::gaussianSmooth} (sigma, ...) or
#'   to \code{imager::isoblur} if \pkg{mmand} is not available.
#' @rdname filter2D
#' @export
setGeneric("filter2Dgaussian", function(obj, ..., track = TRUE)
  standardGeneric("filter2Dgaussian"))

#' @rdname filter2D
#' @export
setMethod("filter2Dgaussian", "GPRvirtual", function(obj, ..., track = TRUE){
  .filter2D_apply(obj, FUN = function(A, ...){
    if (requireNamespace("mmand", quietly = TRUE)) {
      return(mmand::gaussianSmooth(A, ...))
    } else if (requireNamespace("imager", quietly = TRUE)) {
      im <- .mat_to_cimg(A)
      # map sigma argument if provided to isoblur
      dots <- list(...)
      sigma <- dots$sigma %||% 1
      out <- imager::isoblur(im, sigma = sigma)
      return(.cimg_to_mat(out))
    } else {
      stop("Either 'mmand' or 'imager' is required for gaussian smoothing.")
    }
  }, track = track, ...)
})

# -------------------------------------------------------------------------
# imager-based filters: isoblur, medianblur, blur_anisotropic, diffusion_tensors,
# imgradient, imsobel
# -------------------------------------------------------------------------
#' 2D isometric / Gaussian blur (imager::isoblur)
#'
#' @param sigma numeric blur sigma (default 1)
#' @rdname filter2D
#' @export
setGeneric("filter2Disoblur", function(obj, sigma = 1, ..., track = TRUE)
  standardGeneric("filter2Disoblur"))

#' @rdname filter2D
#' @export
setMethod("filter2Disoblur", "GPRvirtual", function(obj, sigma = 1, ..., track = TRUE){
  .filter2D_apply(obj, FUN = function(A, sigma, ...){
    if (!requireNamespace("imager", quietly = TRUE)) stop("Install 'imager' to use filter2Disoblur()")
    im <- .mat_to_cimg(A)
    out <- imager::isoblur(im, sigma = sigma, ...)
    .cimg_to_mat(out)
  }, track = track, sigma = sigma, ...)
})

#' 2D median blur (imager::medianblur)
#' @param n odd integer kernel size (default 3)
#' @rdname filter2D
#' @export
setGeneric("filter2Dmedianblur", function(obj, n = 3, threshold = 0, ..., track = TRUE)
  standardGeneric("filter2Dmedianblur"))

#' @rdname filter2D
#' @export
setMethod("filter2Dmedianblur", "GPRvirtual", function(obj, n = 3, threshold = 0, ..., track = TRUE){
  .filter2D_apply(obj, FUN = function(A, n, threshold, ...){
    if (!requireNamespace("imager", quietly = TRUE)) stop("Install 'imager' to use filter2Dmedianblur()")
    im <- .mat_to_cimg(A)
    out <- imager::medianblur(im, n = n, threshold = threshold, ...)
    .cimg_to_mat(out)
  }, track = track, n = n, threshold = threshold, ...)
})

#' Anisotropic blur / edge-preserving smoothing (imager::blur_anisotropic)
#' @param amplitude numeric
#' @param sharpness numeric
#' @param anisotropy numeric
#' @param alpha numeric
#' @param sigma numeric
#' @rdname filter2D
#' @export
setGeneric("filter2Danisotropic", function(obj, amplitude = 1, sharpness = 0.7,
                                           anisotropy = 0.6, alpha = 0.6, sigma = 1.1,
                                           dl = 0.8, da = 30, ..., track = TRUE)
  standardGeneric("filter2Danisotropic"))

#' @rdname filter2D
#' @export
setMethod("filter2Danisotropic", "GPRvirtual", function(obj, amplitude = 1, sharpness = 0.7,
                                                        anisotropy = 0.6, alpha = 0.6, sigma = 1.1,
                                                        dl = 0.8, da = 30, ..., track = TRUE){
  .filter2D_apply(obj, FUN = function(A, amplitude, sharpness, anisotropy, alpha, sigma, dl, da, ...){
    if (!requireNamespace("imager", quietly = TRUE)) stop("Install 'imager' to use filter2Danisotropic()")
    im <- .mat_to_cimg(A)
    out <- imager::blur_anisotropic(im, amplitude = amplitude,
                                    sharpness = sharpness,
                                    anisotropy = anisotropy,
                                    alpha = alpha,
                                    sigma = sigma,
                                    dl = dl, da = da, ...)
    .cimg_to_mat(out)
  }, track = track, amplitude = amplitude, sharpness = sharpness,
  anisotropy = anisotropy, alpha = alpha, sigma = sigma, dl = dl, da = da, ...)
})

#' Diffusion tensors (imager::diffusion_tensors)
#' @param sharpness numeric
#' @param anisotropy numeric
#' @param alpha numeric
#' @param sigma numeric
#' @rdname filter2D
#' @export
setGeneric("filter2Ddiffusiontensors", function(obj, sharpness = 0.7, anisotropy = 0.6,
                                                alpha = 0.6, sigma = 1.1, ..., track = TRUE)
  standardGeneric("filter2Ddiffusiontensors"))

#' @rdname filter2D
#' @export
setMethod("filter2Ddiffusiontensors", "GPRvirtual", function(obj, sharpness = 0.7, anisotropy = 0.6,
                                                             alpha = 0.6, sigma = 1.1, ..., track = TRUE){
  .filter2D_apply(obj, FUN = function(A, sharpness, anisotropy, alpha, sigma, ...){
    if (!requireNamespace("imager", quietly = TRUE)) stop("Install 'imager' to use filter2Ddiffusiontensors()")
    im <- .mat_to_cimg(A)
    out <- imager::diffusion_tensors(im, sharpness = sharpness, anisotropy = anisotropy, alpha = alpha, sigma = sigma, ...)
    .cimg_to_mat(out)
  }, track = track, sharpness = sharpness, anisotropy = anisotropy, alpha = alpha, sigma = sigma, ...)
})

#' 2D gradient (imager::imgradient)
#' @rdname filter2D
#' @export
setGeneric("filter2Dgradient", function(obj, type = c("xy","x","y"), ..., track = TRUE)
  standardGeneric("filter2Dgradient"))

#' @rdname filter2D
#' @export
setMethod("filter2Dgradient", "GPRvirtual", function(obj, type = c("xy","x","y"), ..., track = TRUE){
  type <- match.arg(type)
  .filter2D_apply(obj, FUN = function(A, type, ...){
    if (!requireNamespace("imager", quietly = TRUE)) stop("Install 'imager' to use filter2Dgradient()")
    im <- .mat_to_cimg(A)
    g <- imager::imgradient(im, ...)
    # return gradient magnitude if "xy", else corresponding component
    if (type == "xy"){
      gx <- g[[1]]; gy <- g[[2]]
      mag <- sqrt(gx*gx + gy*gy)
      .cimg_to_mat(mag)
    } else if (type == "x"){
      .cimg_to_mat(g[[1]])
    } else {
      .cimg_to_mat(g[[2]])
    }
  }, track = track, type = type, ...)
})


# -------------------------------------------------------------------------
# FFT low-pass destriping wrapper
# -------------------------------------------------------------------------
#' Column-wise FFT low-pass (destriping)
#'
#' This function performs a column-wise FFT attenuation of low-frequency
#' components (normalized frequency cutoff controlled with \code{cutoff}).
#'
#' @param cutoff numeric normalized frequency cutoff (0-0.5)
#' @param strength numeric attenuation factor (0-1)
#' @rdname filter2D
#' @export
setGeneric("filter2Dfftlowpass", function(obj, cutoff = 0.04, strength = 0.85, ..., track = TRUE)
  standardGeneric("filter2Dfftlowpass"))

#' @rdname filter2D
#' @export
setMethod("filter2Dfftlowpass", "GPRvirtual", function(obj, cutoff = 0.04, strength = 0.85, ..., track = TRUE){
  .filter2D_apply(obj, FUN = .filter2D_fft_lowpass_mat, track = track, cutoff = cutoff, strength = strength)
})


# -------------------------------------------------------------------------
#' Two-dimensional anisotropic blur / edge-preserving smoothing
#'
#' Apply an edge-preserving anisotropic blur to the GPR data.
#' Uses \pkg{imager}::blur_anisotropic internally.
#'
#' @param obj (`GPR* object`) Object inheriting from \code{GPRvirtual}.
#' @param amplitude numeric, default 1. Controls intensity of blur.
#' @param sharpness numeric, default 0.7. Controls edge sharpness preservation.
#' @param anisotropy numeric, default 0.6. Controls anisotropy of smoothing.
#' @param alpha numeric, default 0.6. Controls blending factor for diffusion.
#' @param sigma numeric, default 1.1. Gaussian pre-smoothing sigma.
#' @param dl numeric, default 0.8. Step size along linear diffusion.
#' @param da numeric, default 30. Angular resolution for anisotropic blur.
#' @param ... Additional arguments passed to \pkg{imager}::blur_anisotropic.
#' @param track (`logical(1)`), default TRUE. Track processing step.
#'
#' @return Object of same class as \code{obj} with updated \code{@data}.
#' @examples
#' \dontrun{
#' gpr2 <- filter2Dblur_anisotropic(gpr_obj, amplitude = 0.9, sigma = 1.2)
#' }
#' @rdname filter2D
#' @export
setGeneric("filter2Dblur_anisotropic", function(obj, amplitude = 1, sharpness = 0.7,
                                                anisotropy = 0.6, alpha = 0.6, sigma = 1.1,
                                                dl = 0.8, da = 30, ..., track = TRUE)
  standardGeneric("filter2Dblur_anisotropic"))

setMethod("filter2Dblur_anisotropic", "GPRvirtual", function(obj, amplitude = 1, sharpness = 0.7,
                                                             anisotropy = 0.6, alpha = 0.6, sigma = 1.1,
                                                             dl = 0.8, da = 30, ..., track = TRUE){
  .filter2D_apply(obj, FUN = function(A, amplitude, sharpness, anisotropy, alpha, sigma, dl, da, ...){
    if (!requireNamespace("imager", quietly = TRUE)) stop("Install 'imager' to use filter2Dblur_anisotropic()")
    im <- .mat_to_cimg(A)
    out <- imager::blur_anisotropic(im, amplitude = amplitude,
                                    sharpness = sharpness,
                                    anisotropy = anisotropy,
                                    alpha = alpha,
                                    sigma = sigma,
                                    dl = dl, da = da, ...)
    .cimg_to_mat(out)
  }, track = track, amplitude = amplitude, sharpness = sharpness,
  anisotropy = anisotropy, alpha = alpha, sigma = sigma, dl = dl, da = da, ...)
})

# -------------------------------------------------------------------------
#' Two-dimensional Canny edge detection
#'
#' Applies the Canny edge detection algorithm to 2D GPR data.
#' Uses \pkg{imager}::cannyEdges internally.
#'
#' @param obj (`GPR* object`) Object inheriting from \code{GPRvirtual}.
#' @param sigma numeric, default 1. Gaussian smoothing before gradient.
#' @param alpha numeric, default 0.5. Threshold blending factor.
#' @param ... Additional arguments passed to \pkg{imager}::cannyEdges.
#' @param track (`logical(1)`), default TRUE. Track processing step.
#'
#' @return Object of same class as \code{obj} with updated \code{@data}.
#' @examples
#' \dontrun{
#' gpr2 <- filter2DcannyEdges(gpr_obj, sigma = 1, alpha = 0.5)
#' }
#' @rdname filter2D
#' @export
setGeneric("filter2DcannyEdges", function(obj, sigma = 1, alpha = 0.5, ..., track = TRUE)
  standardGeneric("filter2DcannyEdges"))

setMethod("filter2DcannyEdges", "GPRvirtual", function(obj, sigma = 1, alpha = 0.5, ..., track = TRUE){
  .filter2D_apply(obj, FUN = function(A, sigma, alpha, ...){
    if (!requireNamespace("imager", quietly = TRUE)) stop("Install 'imager' to use filter2DcannyEdges()")
    im <- .mat_to_cimg(A)
    out <- imager::cannyEdges(im, sigma = sigma, alpha = alpha, ...)
    .cimg_to_mat(out)
  }, track = track, sigma = sigma, alpha = alpha, ...)
})



# -------------------------------------------------------------------------
#' Two-dimensional Hessian filter
#'
#' Enhances ridges and edges using Hessian-based filtering.
#' Uses \pkg{imager}::imhessian.
#'
#' @param obj (`GPR* object`) Object inheriting from \code{GPRvirtual}.
#' @param sigma numeric, default 1. Gaussian smoothing before computing Hessian.
#' @param ... Additional arguments passed to \pkg{imager}::imhessian.
#' @param track (`logical(1)`), default TRUE.
#'
#' @return Object of same class as \code{obj} with processed data.
#' @examples
#' \dontrun{
#' gpr2 <- filter2Dimhessian(gpr_obj, sigma = 1)
#' }
#' @rdname filter2D
#' @export
setGeneric("filter2Dimhessian", function(obj, sigma = 1, ..., track = TRUE)
  standardGeneric("filter2Dimhessian"))

setMethod("filter2Dimhessian", "GPRvirtual", function(obj, sigma = 1, ..., track = TRUE){
  .filter2D_apply(obj, FUN = function(A, sigma, ...){
    if (!requireNamespace("imager", quietly = TRUE)) stop("Install 'imager' to use filter2Dimhessian()")
    im <- .mat_to_cimg(A)
    out <- imager::imhessian(im, sigma = sigma, ...)
    .cimg_to_mat(out)
  }, track = track, sigma = sigma, ...)
})

# -------------------------------------------------------------------------
#' Two-dimensional Laplacian filter
#'
#' Enhances edges by computing the Laplacian of the image.
#' Uses \pkg{imager}::imlap.
#'
#' @param obj (`GPR* object`) Object inheriting from \code{GPRvirtual}.
#' @param ... Additional arguments passed to \pkg{imager}::imlap.
#' @param track (`logical(1)`), default TRUE.
#'
#' @return Object of same class as \code{obj} with processed data.
#' @examples
#' \dontrun{
#' gpr2 <- filter2Dimlap(gpr_obj)
#' }
#' @rdname filter2D
#' @export
setGeneric("filter2Dimlap", function(obj, ..., track = TRUE) standardGeneric("filter2Dimlap"))

setMethod("filter2Dimlap", "GPRvirtual", function(obj, ..., track = TRUE){
  .filter2D_apply(obj, FUN = function(A, ...){
    if (!requireNamespace("imager", quietly = TRUE)) stop("Install 'imager' to use filter2Dimlap()")
    im <- .mat_to_cimg(A)
    out <- imager::imlap(im, ...)
    .cimg_to_mat(out)
  }, track = track, ...)
})

# -------------------------------------------------------------------------
#' Two-dimensional sharpening
#'
#' Sharpens the GPR data using \pkg{imager}::imsharpen.
#'
#' @param obj (`GPR* object`) Object inheriting from \code{GPRvirtual}.
#' @param amount numeric, default 1. Strength of sharpening.
#' @param ... Additional arguments passed to \pkg{imager}::imsharpen.
#' @param track (`logical(1)`), default TRUE.
#'
#' @return Object of same class as \code{obj} with processed data.
#' @examples
#' \dontrun{
#' gpr2 <- filter2Dimsharpen(gpr_obj, amount = 1.5)
#' }
#' @rdname filter2D
#' @export
setGeneric("filter2Dimsharpen", function(obj, amplitude = 1, type = "diffusion", edge = 1, alpha = 0, sigma = 0, ..., track = TRUE)
  standardGeneric("filter2Dimsharpen"))

setMethod("filter2Dimsharpen", "GPRvirtual", function(obj, amplitude = 1, type = "diffusion", edge = 1, alpha = 0, sigma = 0, ..., track = TRUE){
  .filter2D_apply(obj, FUN = function(A, amplitude, type , edge , alpha , sigma, ...){
    if (!requireNamespace("imager", quietly = TRUE)) stop("Install 'imager' to use filter2Dimsharpen()")
    im <- .mat_to_cimg(A)
    out <- imager::imsharpen(im, amplitude = amplitude, type , edge , alpha , sigma, ...)
    .cimg_to_mat(out)
  }, track = track, amplitude = amplitude, type , edge , alpha , sigma, ...)
})




#' Local Contrast Enhancement Using Sliding Window
#'
#' Enhances the local contrast of a grayscale image (matrix) using a sliding window approach.
#' Each pixel is transformed based on the local mean and standard deviation in its neighborhood.
#'
#' @param image Numeric matrix representing a grayscale image.
#' @param window_size Odd integer specifying the size of the sliding window (default is 3).
#' @param epsilon Small numeric value to avoid division by zero (default is 1e-8).
#'
#' @return Numeric matrix of the same dimensions as `image`, with values normalized to [0,1].
#'
#' @examples
#' set.seed(1)
#' img <- matrix(runif(100), nrow = 10)
#' enhanced <- local_contrast_enhancement(img, window_size = 5)
#' print(enhanced)
#'
#' @rdname filter2D
#' @export
setGeneric("filter2DlocalContrast", function(obj, win = 3, alpha = 0, epsilon = 1e-8, ..., track = TRUE)
  standardGeneric("filter2DlocalContrast"))

setMethod("filter2DlocalContrast", "GPRvirtual", function(obj,  win = 3, alpha = 0, epsilon = 1e-8, ..., track = TRUE){
  .filter2D_apply(obj, FUN = .local_contrast_enhancement, track = track, win , alpha , epsilon)
})

.local_contrast_enhancement <- function(image, window_size = 3, alpha = 0, epsilon = 1e-8) {
  # image: numeric matrix
  # window_size: odd integer (e.g., 3, 5, 7)
  # epsilon: small value to avoid division by zero
  
  if (window_size %% 2 == 0) {
    stop("window_size must be an odd number")
  }
  
  nr <- nrow(image)
  nc <- ncol(image)
  pad <- floor(window_size / 2)
  
  # Pad image using edge replication
  padded <- matrix(0, nr + 2 * pad, nc + 2 * pad)
  padded[(pad + 1):(pad + nr), (pad + 1):(pad + nc)] <- image
  
  # Replicate borders
  padded[1:pad, (pad + 1):(pad + nc)] <- image[rep(1, pad), ]
  padded[(pad + nr + 1):(nr + 2 * pad), (pad + 1):(pad + nc)] <- image[rep(nr, pad), ]
  padded[, 1:pad] <- padded[, rep(pad + 1, pad)]
  padded[, (pad + nc + 1):(nc + 2 * pad)] <- padded[, rep(pad + nc, pad)]
  
  # Output image
  output <- matrix(0, nr, nc)
  
  for (i in 1:nr) {
    for (j in 1:nc) {
      window <- padded[i:(i + 2 * pad), j:(j + 2 * pad)]
      
      local_mean <- mean(window)
      local_sd   <- sd(as.vector(window))
      local_min <- min(window)
      local_max <- max(window)
      # Local contrast enhancement
      output[i, j] <- (image[i, j] - local_mean) / (local_sd + epsilon)
      # if (local_max != local_min) {
      #   local_eq <- (image[i,j] - local_min) / (local_max - local_min)
      # } else {
      #   local_eq <- 0
      # }
      # output[i,j] <- (1 - alpha) * image[i,j] + alpha * local_eq
    }
  }
  
  # Normalize output to [0, 1]
  output <- (output - min(output)) / (max(output) - min(output))
  
  return(output)
}



#' Smooth Sparse Enhancement of GPR Depth-Slice
#'
#' Applies a smooth nonlinear transformation to increase sparsity in a GPR depth-slice.
#' Small values near zero are compressed while peaks and valleys are amplified.
#'
#' @param image_matrix Numeric matrix representing the GPR depth-slice.
#' @param scale Numeric value controlling soft threshold (default 0.05).
#' @param power Numeric exponent for amplifying high values (default 1.5).
#'
#' @return Numeric matrix of same size as image_matrix, normalized to [0,1].
#'
#' @examples
#' set.seed(1)
#' gpr_slice <- matrix(rnorm(100, 0, 0.05), nrow = 10)
#' enhanced_slice <- smooth_sparse_gpr_image(gpr_slice, scale = 0.05, power = 2)
#' print(enhanced_slice)
#'
#' @rdname filter2D
#' @export
setGeneric("filter2DsmoothSparse", function(obj, scale = 0.05, power = 1.5, ..., track = TRUE)
  standardGeneric("filter2DsmoothSparse"))

setMethod("filter2DsmoothSparse", "GPRvirtual", function(obj,  scale = 0.05, power = 1.5, ..., track = TRUE){
  .filter2D_apply(obj, FUN = .smooth_sparse_gpr_image, track = track, scale , power )
})


.smooth_sparse_gpr_image <- function(image_matrix, scale = 0.05, power = 1.5) {
  # Apply smooth thresholding using tanh
  enhanced <- sign(image_matrix) * (tanh(abs(image_matrix)/scale)^power)
  
  # Normalize to [0,1]
  enhanced <- enhanced - min(enhanced)
  enhanced <- enhanced / max(enhanced)
  
  return(enhanced)
}
