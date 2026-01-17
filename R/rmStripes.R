#' Hybrid destriping of geophysical raster images
#'
#' Removes striping artefacts from gridded geophysical data
#' (e.g. GPR, DEMs, magnetic or resistivity grids) using a
#' hybrid spatial–frequency approach.
#'
#' The algorithm combines:
#' \itemize{
#'   \item Adaptive smoothing along the stripe direction
#'   \item Oimoen-style perpendicular high-pass correction
#'   \item Optional FFT-based low-frequency attenuation
#'   \item Final Gaussian regularization
#' }
#'
#' @param obj Numeric matrix representing a gridded image.
#'   Stripes are assumed to be aligned along columns by default.
#' @param stripeDir Character string. Direction of stripes:
#'   `"column"` (default) or `"row"`.
#' @param adaptive Logical. If `TRUE`, apply adaptive stripe-direction
#'   smoothing based on stripe strength.
#' @param fft Logical. If `TRUE`, apply frequency-domain destriping.
#' @param fftCutoff Numeric. Normalized frequency cutoff (0–0.5)
#'   for FFT attenuation.
#' @param fftStrength Numeric. Attenuation strength (0–1) applied
#'   below the cutoff frequency.
#' @param sig Numeric. Standard deviation of the final Gaussian
#'   smoothing kernel.
#' @param radius Integer. Radius of the Gaussian kernel (in pixel).
#' @param track (`logical[1]`) Should the processing step be tracked?
#' @return (`GPRcube|GPRslice`) Object with striping artefacts reduced.
#' @references
#' Oimoen, M. (2000). An effective filter for removal of production
#' artifacts in digital elevation models.
#'
#' Ernenwein, E. G., & Kvamme, K. L. (2008).
#' Data processing issues in large-area GPR surveys.
#' @name rmStripes
#' @rdname rmStripes
#' @export
#' @concept processing 
setGeneric("rmStripes", 
          function(obj, stripeDir = "column",
                   adaptive = TRUE,
                   fft = TRUE,
                   fftCutoff = 0.04,
                   fftStrength = 0.85,
                   sig = 1,
                   radius = 2,
                   track = TRUE)
            standardGeneric("rmStripes"))

#' @rdname rmStripes
#' @export
setMethod("rmStripes", "GPRcube", 
          function(obj, stripeDir = "column",
                   adaptive = TRUE,
                   fft = TRUE,
                   fftCutoff = 0.04,
                   fftStrength = 0.85,
                   sig = 1,
                   radius = 2,
                   track = TRUE){
            
  
    for(i in 1:dim(obj)[3])
      obj@data[,,i] <- .destripe(
                                obj@data[,,i],
                                stripeDir = stripeDir,
                                adaptive = adaptive,
                                fft = fft,
                                fftCutoff = fftCutoff,
                                fftStrength = fftStrength,
                                sig = sig,
                                radius = radius
                              )             
  if(isTRUE(track)) proc(obj) <- getArgs()
  return(obj) 
})  

#' @rdname rmStripes
#' @export
setMethod("rmStripes", "GPRslice", 
          function(obj, stripeDir = "column",
                    adaptive = TRUE,
                    fft = TRUE,
                    fftCutoff = 0.04,
                    fftStrength = 0.85,
                    sig = 1,
                    radius = 2,
                    track = TRUE){
            
  obj@data <- .destripe(
    obj@data,
    stripeDir = stripeDir,
    adaptive = adaptive,
    fft = fft,
    fftCutoff = fftCutoff,
    fftStrength = fftStrength,
    sig = sig,
    radius = radius
  )             
  if(isTRUE(track)) proc(obj) <- getArgs()
  return(obj) 
}) 
  
  
  
#' Hybrid destriping of geophysical raster images
#'
#' Removes striping artefacts from gridded geophysical data
#' (e.g. GPR, DEMs, magnetic or resistivity grids) using a
#' hybrid spatial–frequency approach.
#'
#' The algorithm combines:
#' \itemize{
#'   \item Adaptive smoothing along the stripe direction
#'   \item Oimoen-style perpendicular high-pass correction
#'   \item Optional FFT-based low-frequency attenuation
#'   \item Final Gaussian regularization
#' }
#'
#' @param S Numeric matrix representing a gridded image.
#'   Stripes are assumed to be aligned along columns by default.
#' @param stripeDir Character string. Direction of stripes:
#'   `"column"` (default) or `"row"`.
#' @param adaptive Logical. If `TRUE`, apply adaptive stripe-direction
#'   smoothing based on stripe strength.
#' @param fft Logical. If `TRUE`, apply frequency-domain destriping.
#' @param fftCutoff Numeric. Normalized frequency cutoff (0–0.5)
#'   for FFT attenuation.
#' @param fftStrength Numeric. Attenuation strength (0–1) applied
#'   below the cutoff frequency.
#' @param sig Numeric. Standard deviation of the final Gaussian
#'   smoothing kernel.
#' @param radius Integer. Radius of the Gaussian kernel.
#'
#' @return Numeric matrix of the same dimensions as `S`,
#'   with striping artefacts reduced.
#'
#' @references
#' Oimoen, M. (2000). An effective filter for removal of production
#' artifacts in digital elevation models.
#'
#' Ernenwein, E. G., & Kvamme, K. L. (2008).
#' Data processing issues in large-area GPR surveys.
.destripe <- function(
    S,
    stripeDir = "column",
    adaptive = TRUE,
    fft = TRUE,
    fftCutoff = 0.04,
    fftStrength = 0.85,
    sig = 1,
    radius = 2
) {
  
  if (stripeDir == "column") {
    S <- t(S)
  }
  
  # 1) Adaptive stripe smoothing along the stripes
  if (adaptive) {
    S_smooth <- .adaptiveStripeSmoothing(S)
  } else {
    k <- rep(1 / 7, 7)
    S_smooth <- apply(S, 2, stats::filter, k, circular = TRUE)
  }
  
  # 2) Oimoen high-pass correction across the stripes
  hp_kernel <- c(1, -1) / 2
  S_hp <- t(apply(S_smooth, 1, stats::filter, hp_kernel, circular = TRUE))
  S_corr <- S + S_hp
  
  # 3) FFT destriping
  if (fft) {
    S_corr <- .fft_destripe(
      S_corr,
      cutoff = fftCutoff,
      strength = fftStrength
    )
  }
  
  # 4) Final Gaussian smoothing
  x <- -radius:radius
  g <- exp(-(x^2) / (2 * sig^2))
  g <- g / sum(g)
  
  S_final <- t(apply(S_corr, 1, stats::filter, g, circular = TRUE))
  
  if (stripeDir == "column") {
    S_final <- t(S_final)
  }
  
  return(S_final)
}

#' Estimate stripe strength per column
#'
#' Computes a quantitative estimate of striping intensity
#' for each column of a gridded image by measuring
#' high-frequency perpendicular differences.
#'
#' This metric is used to drive adaptive smoothing.
#'
#' @param S Numeric matrix.
#'
#' @return Numeric vector of length `ncol(S)` representing
#'   relative stripe strength per column.
.estimateStripeStrength <- function(S) {
  # High-pass vertically (perpendicular to stripes)
  hp <- t(apply(S, 1, function(x) diff(c(x[1], x))))
  colMeans(abs(hp), na.rm = TRUE)
}


#' Adaptive smoothing along stripe direction
#'
#' Applies column-wise smoothing using variable kernel sizes
#' determined from estimated stripe strength.
#'
#' Columns with stronger striping are smoothed more strongly,
#' while well-behaved columns are preserved.
#'
#' @param S Numeric matrix.
#' @param min_len Integer. Minimum smoothing window length.
#' @param max_len Integer. Maximum smoothing window length.
#'
#' @return Numeric matrix with adaptively smoothed columns.
.adaptiveStripeSmoothing <- function(
    S,
    min_len = 3,
    max_len = 21
) {
  strength <- .estimateStripeStrength(S)
  
  # Normalize strength to [0,1]
  s_norm <- (strength - min(strength)) /
    (max(strength) - min(strength) + .Machine$double.eps)
  
  # Map to window sizes
  win_sizes <- round(min_len + s_norm * (max_len - min_len))
  win_sizes <- pmax(win_sizes, 3)
  win_sizes <- ifelse(win_sizes %% 2 == 0, win_sizes + 1, win_sizes)
  
  # Smooth each column with its own window
  S_out <- S
  for (j in seq_len(ncol(S))) {
    k <- win_sizes[j]
    kernel <- rep(1 / k, k)
    S_out[, j] <- stats::filter(S[, j], kernel, circular = TRUE)
  }
  
  return(S_out)
}

# -----------------------------------------------------------
# fft_destripe()
# -----------------------------------------------------------
# Frequency-domain destriping
#
# Args:
#   S           numeric matrix
#   stripe_dir  "column" or "row"
#   cutoff      normalized frequency cutoff (0–0.5)
#   strength    attenuation factor (0–1)
#
# Returns:
#   Filtered matrix
# -----------------------------------------------------------
#' Frequency-domain destriping
#'
#' Removes low-frequency stripe components by attenuating
#' spectral energy perpendicular to the stripe direction.
#'
#' Particularly effective for periodic or acquisition-induced
#' striping artefacts.
#'
#' @param S Numeric matrix.
#' @param stripe_dir Character string. `"column"` or `"row"`.
#' @param cutoff Numeric. Normalized frequency cutoff (0–0.5).
#' @param strength Numeric. Attenuation strength (0–1).
#'
#' @return Numeric matrix with frequency-domain stripe reduction.
.fft_destripe <- function(
    S,
    stripe_dir = c("column", "row"),
    cutoff = 0.05,
    strength = 0.9
) {
  stripe_dir <- match.arg(stripe_dir)
  
  if (stripe_dir == "row") {
    S <- t(S)
  }
  
  n <- nrow(S)
  freqs <- abs(seq_len(n) - ceiling(n / 2)) / n
  
  # Frequency attenuation mask
  mask <- rep(1, n)
  mask[freqs < cutoff] <- 1 - strength
  
  S_fft <- apply(S, 2, function(col) {
    F <- fft(col)
    F <- F * mask
    Re(fft(F, inverse = TRUE) / length(F))
  })
  
  if (stripe_dir == "row") {
    S_fft <- t(S_fft)
  }
  
  return(S_fft)
}
