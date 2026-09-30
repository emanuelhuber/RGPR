#' Topographic Kirchhoff migration
#'
#' Migrate a two-dimensional GPR profile directly from the acquisition
#' topography onto a regular distance-depth grid. The Kirchhoff migration
#' supports zero-offset, common-offset, and bistatic antenna geometries.
#'
#' Migration is performed directly from the acquisition surface. No elevation
#' static correction is applied. This follows the principle of topographic
#' Kirchhoff migration described by Dujardin and Bano (2013).
#'
#' For every image point \eqn{\mathbf{p}}, the two-way travel time associated
#' with trace \eqn{j} is calculated as
#'
#' \deqn{
#' t_j(\mathbf{p}) =
#' \frac{
#'   \Vert \mathbf{p} - \mathbf{s}_j \Vert +
#'   \Vert \mathbf{p} - \mathbf{r}_j \Vert
#' }{v},
#' }
#'
#' where \eqn{\mathbf{s}_j} and \eqn{\mathbf{r}_j} are the transmitter and
#' receiver coordinates, respectively, and \eqn{v} is the
#' electromagnetic-wave velocity.
#'
#' @param obj An object of class `"GPR"`.
#'
#' @param type Character string specifying the migration method. Currently,
#'   `"kirchhoff"` performs topographic Kirchhoff migration.
#'
#' @param dz vertical sampling interval of the migrated image, in metres.
#'     The default is `0.25 * obj_dz`, where `obj_dz` is the mean vertical
#'     sampling interval.
#' @param x Optional numeric vector giving the horizontal position, in metres,
#'   of every trace. If `NULL`, `obj@x` is used. The vector must have length
#'   `ncol(obj@data)` and must contain finite, strictly increasing values.
#'
#' @param fdo Optional positive numeric scalar giving the antenna centre or
#'   dominant frequency in MHz. If `NULL`, `obj@freq` is used. The frequency is
#'   required when `maxangle = NULL`, because the migration aperture is then
#'   calculated from the first Fresnel zone.
#'
#' @param maxangle Maximum migration aperture angle in degrees, measured from
#'   the vertical. A trace contributes only if both the transmitter-to-pixel
#'   and receiver-to-pixel angles do not exceed this value. Use `90` to disable
#'   the angle-based aperture restriction.
#'
#'   If `NULL`, the aperture is derived separately for each image point from
#'   the first depth-dependent Fresnel zone following Pérez-Gracia et al.
#'   (2008). With wavelength
#'   \eqn{\lambda = 1000 v / f_{do}} and depth \eqn{d} below the local ground
#'   surface, the Fresnel radius is
#'   \eqn{r_f = 0.5\sqrt{2\lambda d}}, and the limiting angle is
#'   \eqn{\arctan(r_f/d)}. The aperture therefore narrows with depth.
#'
#' @param weight Character string specifying the migration weighting:
#'
#'   - `"none"` applies only the spatial quadrature weights.
#'
#'   - `"obliquity"` additionally applies the symmetric bistatic obliquity
#'     factor
#'     \eqn{\sqrt{\cos(\theta_{tx})\cos(\theta_{rx})}}.
#'     This is the default.
#'
#'   - `"legacy"` applies the obliquity factor and the distance-dependent
#'     amplitude decay used by the former RGPR Kirchhoff migration
#'     implementation, proportional to
#'     \eqn{1/\sqrt{2\pi t v}}.
#'
#' @param normalize Logical. If `TRUE`, divide every migrated pixel by the sum
#'   of the absolute migration weights contributing to that pixel. This
#'   reduces amplitude variations caused by changes in aperture size. This is
#'   not a display normalization.
#'
#' @param spreading Logical. If `TRUE`, compensate for two-dimensional
#'   geometrical spreading by multiplying each contribution by
#'   \eqn{\sqrt{d_{tx}d_{rx}}}, where the distances are expressed in metres.
#'
#' @param waveletfilter Character string specifying the wavelet-shaping filter:
#'
#'   - `"none"` does not apply wavelet shaping.
#'
#'   - `"halfderiv"` applies a half-derivative filter
#'     \eqn{\sqrt{i\omega}} along the time axis. This converts the
#'     three-dimensional point-source response into the two-dimensional
#'     line-source response expected by two-dimensional Kirchhoff summation.
#'
#' @param antialias Logical. If `TRUE`, apply an anti-alias triangle filter to
#'   each migrated contribution. The filter half-width is based on the
#'   travel-time moveout between neighbouring traces.
#'
#' @param aafactor Positive numeric scalar multiplying the anti-alias filter
#'   half-width. The default is `1`.
#'
#' @param ... Additional arguments passed to the migration implementation.
#'   Currently supported arguments include:
#'
#'   - `max_depth`: maximum migration depth below the local ground surface, in
#'     metres. If omitted, it is estimated from the time axis and the velocity
#'     model.
#'
#'   - `vel_mode`: travel-time algorithm. One of `"auto"`, `"constant"`,
#'     `"layered"`, or `"general"`.
#'
#'   - `vel_dx`, `vel_dz`: horizontal and vertical spacing, in metres, of the
#'     regular velocity grid used by `vel_mode = "general"`.
#'
#'   - `ray_step`: spacing, in metres, between slowness samples along a ray.
#'
#'   - `n_ray`: maximum number of samples along each ray leg.
#'
#' @details
#' The main Kirchhoff migration processing steps are:
#'
#' 1. Construct a regular distance-depth output grid.
#'
#' 2. Mask image points situated above the local ground surface or below the
#'    specified maximum migration depth.
#'
#' 3. Calculate transmitter-to-pixel and receiver-to-pixel distances.
#'
#' 4. Select traces within the angular or Fresnel migration aperture.
#'
#' 5. Calculate bistatic travel times.
#'
#' 6. Linearly interpolate trace amplitudes at the calculated travel times.
#'
#' 7. Apply spatial-integration, obliquity, and optional spreading weights.
#'
#' 8. Sum the trace contributions into the migrated image.
#'
#' The implementation assumes a two-dimensional profile, isotropic velocity,
#' straight propagation paths, and coordinates expressed in metres.
#'
#' With a non-constant velocity model, the travel time of each ray leg is
#' obtained by integrating slowness along the straight segment between the
#' antenna and the image point. Ray bending and refraction are not modelled.
#'
#' The migrated vertical coordinates are stored as depth below the highest
#' trace elevation. The original acquisition elevations remain available in
#' the coordinate slot, while the migrated traces share the elevation of the
#' highest input trace.
#'
#' @return The migrated `"GPR"` object. The data matrix is replaced by the
#'   migrated image, and its vertical axis is replaced by the migration-depth
#'   axis.
#'
#' @references
#' Dujardin, J.-R. and Bano, M. (2013). Topographic migration of GPR data:
#' Examples from Chad and Mongolia. *Comptes Rendus Geoscience*, 345(2),
#' 73–80.
#' \doi{10.1016/j.crte.2013.01.003}
#'
#' Pérez-Gracia, V., Di Capua, D., Caselles, O., Rial, F., Lorenzo, H.,
#' González-Drigo, R. and Armesto, J. (2008). Horizontal resolution in a
#' non-destructive shallow GPR survey: An experimental evaluation.
#' *NDT & E International*, 41(8), 611–620.
#' \doi{10.1016/j.ndteint.2008.06.002}
#'
#' @name migrate
#' @rdname migrate
#' @export
#' @concept processing
setGeneric(
  "migrate",
  function(
    obj,
    type = c("static", "kirchhoff"),
    dz = NULL,
    fdo = NULL,
    x = NULL,
    maxangle = NULL,
    weight = "obliquity",
    normalize = FALSE,
    spreading = TRUE,
    waveletfilter = "halfderiv",
    antialias = FALSE,
    aafactor = 1,
    ...) {
    standardGeneric("migrate")
  }
)


#' @name migrate
#' @rdname migrate
#' @export
setMethod(
  "migrate",
  signature(obj = "GPR"),
  function(
    obj,
    type = c("static", "kirchhoff"),
    dz = NULL,
    fdo = NULL,
    x = NULL,
    maxangle = NULL,
    weight = "obliquity",
    normalize = FALSE,
    spreading = TRUE,
    waveletfilter = "halfderiv",
    antialias = FALSE,
    aafactor = 1,
    ...) {
    
    type <- match.arg(type)
    
    # The supplied implementation only contains the Kirchhoff branch.
    # This check prevents type = "static" from silently running Kirchhoff
    # migration.
    if (type == "static") {
      stop(
        "The supplied migrate() implementation does not contain the ",
        "'static' migration branch."
      )
    }
    
    weight <- match.arg(weight, choices = c("obliquity", "legacy", "none"))
    
    waveletfilter <- match.arg(waveletfilter, choices = c("halfderiv", "none"))
    
    # --------------------------------------------------------------------- #
    # Validate object properties
    # --------------------------------------------------------------------- #
    
    if (length(obj@antsep) == 0L || !is.numeric(obj@antsep)) {
      stop("You must first define the antenna separation, for example with ",
        "'antsep(obj) <- 0'.")
    }
    
    if (is.null(obj@vel) || length(obj@vel) == 0L) {
      stop("You must first define the EM-wave velocity, for example with ",
        "'vel(obj) <- 0.1'.")
    }
    
    if (!isTRUE(isSamplingRegular(obj, axes = 2))) {
      stop("Vertical sampling must be regular. Use `resampleRegGrid()` first.")
    }
    
    # --------------------------------------------------------------------- #
    # Trace positions
    # --------------------------------------------------------------------- #
    if (is.null(x)) {
      x <- obj@x
    }
    if (!is.numeric(x) || length(x) != ncol(obj@data) || any(!is.finite(x))) {
      stop("'x' must be a finite numeric vector with length ncol(obj@data).")
    }
    if (is.unsorted(x, strictly = TRUE)) {
      stop("'x' must be strictly increasing.")
    }
    
    # --------------------------------------------------------------------- #
    # Acquisition topography
    # --------------------------------------------------------------------- #
    if (length(obj@coord) != 0L && ncol(obj@coord) >= 3L) {
      topo <- obj@coord[, 3L]
      if (length(topo) != ncol(obj@data) || any(!is.finite(topo))) {
        stop("The third column of 'obj@coord' must contain one finite ",
          "elevation per trace.")
      }
    } else {
      topo <- rep.int(0, ncol(obj@data))
      message("Trace vertical positions set to zero.")
    }
    
    # --------------------------------------------------------------------- #
    # Input data and velocity
    # --------------------------------------------------------------------- #
    A   <- obj@data
    dts <- abs(mean(diff(obj@z)))
  
    # Interval velocity in m/ns:
    # scalar, vector with length nrow(A), or matrix with dim(A).
    v <- .getVel(obj, type = "vint", strict = FALSE)
    
    # --------------------------------------------------------------------- #
    # Defaults derived from the GPR object
    # --------------------------------------------------------------------- #
    if (length(v) == 1L) {
      max_depth <- max(obj@z) * v / 2 * 0.9
    } else {
      tt <- obj@z - obj@z[1L]
      vm <- if (is.matrix(v)) {
        v
      } else {
        matrix(v, nrow = length(v), ncol = ncol(A))
      }
      max_depth <- max(apply(c(0, diff(tt)) * vm / 2, 2L, sum)) * 0.9
    }
    if(is.null(dz))    dz <- 0.25 * dts
    if (is.null(fdo)) {
      fdo <- obj@freq
    }
    
    # --------------------------------------------------------------------- #
    # Additional advanced arguments
    # --------------------------------------------------------------------- #
    
    vel_dx   <- NULL
    vel_dz   <- NULL
    ray_step <- NULL
    n_ray    <- 16L
    vel_mode <- "auto"
    
    dots <- list(...)
    
    if (!is.null(dots$max_depth)) max_depth <- dots$max_depth
    if (!is.null(dots$dz))        dz        <- dots$dz
    if (!is.null(dots$vel_mode))  vel_mode  <- dots$vel_mode
    if (!is.null(dots$vel_dx))    vel_dx    <- dots$vel_dx
    if (!is.null(dots$vel_dz))    vel_dz    <- dots$vel_dz
    if (!is.null(dots$ray_step))  ray_step  <- dots$ray_step
    if (!is.null(dots$n_ray))     n_ray     <- dots$n_ray
    
    # Catch former internal argument names. Without this check, users could
    # supply an argument that appears to be accepted but is ignored.
    obsolete <- intersect(
      names(dots),
      c(
        "max_angle",
        "wavelet_filter",
        "aa_factor",
        "xpos"
      )
    )
    
    if (length(obsolete) > 0L) {
      stop(
        "Use the public argument name(s) ",
        paste(
          sQuote(
            c(
              max_angle = "maxangle",
              wavelet_filter = "waveletfilter",
              aa_factor = "aafactor",
              xpos = "x"
            )[obsolete]
          ),
          collapse = ", "
        ),
        "."
      )
    }
    
    # --------------------------------------------------------------------- #
    # Kirchhoff migration
    # --------------------------------------------------------------------- #
    
    xkir <- .kirMigTopo(
      x              = A,
      topoGPR        = topo,
      xpos           = x,
      dts            = dts,
      v              = v,
      fdo            = fdo,
      max_depth      = max_depth,
      dz             = dz,
      xout           = x,
      tx_x           = x,
      rx_x           = x,
      tx_z           = topo,
      rx_z           = topo,
      max_angle      = maxangle,
      weight         = weight,
      normalize      = normalize,
      spreading      = spreading,
      wavelet_filter = waveletfilter,
      antialias      = antialias,
      aa_factor      = aafactor,
      vel_mode       = vel_mode,
      vel_dx         = vel_dx,
      vel_dz         = vel_dz,
      ray_step       = ray_step,
      n_ray          = n_ray
    )
    
    # --------------------------------------------------------------------- #
    # Update the GPR object
    # --------------------------------------------------------------------- #
    
    obj@data <- unclass(xkir)
    
    # Horizontal coordinates may have been supplied explicitly.
    obj@x <- x
    
    # Depth below the highest acquisition elevation.
    obj@z <- attr(xkir, "z")
    
    obj@z0 <- rep.int(0, ncol(obj@data))
    
    # The migrated vertical coordinate is a distance/depth coordinate.
    obj@zunit <- obj@xunit
    
    if (length(obj@coord) > 0L && ncol(obj@coord) >= 3L) {
      # Ensure that the horizontal coordinates stored in @coord remain
      # consistent with obj@x, assuming column 1 contains profile distance.
      obj@coord[, 1L] <- x
      
      # All migrated traces use the highest acquisition elevation as their
      # vertical reference.
      obj@coord[, 3L] <- attr(xkir, "zmax")
    }
    
    # Record the processing call using the actual method arguments.
    proc(obj) <- getArgs()
    
    obj
  }
)




# --------------------------------------------------------------------------- #
# 2D wavelet-shaping filter: half derivative (sqrt(i*omega)) along time.
# Converts the 3D point-source wavelet of the data into the 2D line-source
# response expected by 2D Kirchhoff summation (amplitude ~ sqrt(|w|),
# phase +45 degrees). Zero padding avoids circular wrap-around.
# --------------------------------------------------------------------------- #
.halfDerivative <- function(x, dts) {
  nt <- nrow(x)
  n  <- 2L * nextn(nt)                       # even FFT length >= 2 * nt
  xp <- rbind(x, matrix(0, n - nt, ncol(x)))
  k  <- 0:(n - 1)
  kk <- ifelse(k <= n / 2, k, k - n)         # signed frequency index
  w  <- 2 * pi * kk / (n * dts)              # angular frequency (rad/ns)
  H  <- sqrt(abs(w)) * exp(1i * pi / 4 * sign(w))
  H[k == n / 2] <- sqrt(abs(w[k == n / 2]))  # Nyquist term must be real
  y  <- Re(mvfft(mvfft(xp) * H, inverse = TRUE)) / n
  y[seq_len(nt), , drop = FALSE]
}



# --------------------------------------------------------------------------- #
# Cumulative one-way slowness Sigma(d) = int_0^d (1/v) dd for horizontal layers,
# tabulated at the depths `depth_out` below the reference (highest trace).
#
# With d(t) = int v dt / 2 (as in convertTimeToDepth) one gets exactly
# Sigma(d(t)) = t / 2, so the table is a simple interpolation of time against
# depth. Below the last sample the deepest velocity is extended.
# --------------------------------------------------------------------------- #
.kirLayerSigma <- function(v, tt, depth_out) {
  nt <- length(tt)
  if (length(v) == 1L) v <- rep(v, nt)
  if (length(v) != nt) stop("Layered velocity must be a scalar or a vector of length nrow(x).")
  if (any(!is.finite(v)) || any(v <= 0)) stop("Velocities must be finite and positive.")
  dep <- cumsum(c(0, diff(tt)) * v / 2)
  sig <- stats::approx(dep, tt / 2, xout = depth_out, rule = 1)$y
  deeper <- depth_out > dep[nt]
  sig[deeper] <- tt[nt] / 2 + (depth_out[deeper] - dep[nt]) / v[nt]
  list(sigma = sig, vref = mean(v))
}

# --------------------------------------------------------------------------- #
# Build the slowness grid (ns/m) used by the variable-velocity Kirchhoff
# migration.
#
# v   : interval velocity (m/ns) in the TIME domain: scalar, vector of length
#       nt (depth-varying) or matrix nt x nx (depth- and laterally-varying).
#       Rows = time samples of the data, columns = traces (at `xpos`).
#
# The time-to-depth relation is the one of convertTimeToDepth():
#       depth_i(t_m) = sum_{k <= m} (t_k - t_{k-1}) * v_{k,i} / 2 ,
# with depth measured from the ground surface at trace i. The velocity is
# therefore a function of DEPTH BELOW THE LOCAL GROUND SURFACE, and follows the
# topography. It is resampled to a regular (x, depth) grid, then to the
# (x, elevation) frame of the output image. Above the ground the surface
# velocity is used, below the last sample the deepest velocity.
# --------------------------------------------------------------------------- #
.kirSlownessGrid <- function(v, tt, xpos, topoGPR, xrange, zout, zmax,
                             dz, vel_dx = NULL, vel_dz = NULL) {
  nt <- length(tt)
  nx <- length(xpos)
  if (is.matrix(v)) {
    if (nrow(v) != nt || ncol(v) != nx)
      stop("A velocity matrix must have dim = c(nrow(x), ncol(x)).")
    vm <- v
  } else if (length(v) == nt) {
    vm <- matrix(v, nrow = nt, ncol = nx)
  } else if (length(v) == 1L) {
    vm <- matrix(v, nrow = nt, ncol = nx)
  } else {
    stop("Velocity must be a scalar, a vector of length nrow(x) or a matrix.")
  }
  if (any(!is.finite(vm)) || any(vm <= 0))
    stop("Velocities must be finite and positive.")
  
  # depth below the local surface of every time sample (nt x nx)
  dep <- apply(c(0, diff(tt)) * vm / 2, 2, cumsum)
  
  # regular depth grid
  if (is.null(vel_dz)) vel_dz <- dz
  dmax <- max(dep[nt, ])
  nd   <- max(2L, ceiling(dmax / vel_dz) + 1L)
  dgrid <- vel_dz * (seq_len(nd) - 1L)
  Vd <- vapply(seq_len(nx), function(i)
    stats::approx(dep[, i], vm[, i], xout = dgrid, rule = 2)$y, numeric(nd))
  
  # regular horizontal grid covering pixels and antennas
  if (is.null(vel_dx)) vel_dx <- mean(diff(xpos))
  xv0 <- xrange[1]
  nxv <- max(2L, ceiling((xrange[2] - xrange[1]) / vel_dx - 1e-9) + 1L)
  xv  <- xv0 + vel_dx * (seq_len(nxv) - 1L)
  Vx  <- t(vapply(seq_len(nd), function(r)
    stats::approx(xpos, Vd[r, ], xout = xv, rule = 2)$y, numeric(nxv)))
  Vx <- matrix(Vx, nrow = nd, ncol = nxv)
  
  # to the output frame: row k, column i -> depth = surface_i - zout_k
  surf_v <- stats::approx(xpos, topoGPR, xout = xv, rule = 2)$y - zmax
  S <- matrix(0, nrow = length(zout), ncol = nxv)
  for (i in seq_len(nxv)) {
    f  <- pmax(surf_v[i] - zout, 0) / vel_dz
    i0 <- pmin(floor(f), nd - 2L)
    w  <- pmin(f - i0, 1)
    S[, i] <- 1 / ((1 - w) * Vx[i0 + 1L, i] + w * Vx[i0 + 2L, i])
  }
  list(slow = S, xv0 = xv0, dxv = vel_dx, vel_dz = vel_dz, vel_dx = vel_dx,
       vref = mean(vm))
}

#' Topographic Kirchhoff migration with bistatic antenna geometry
#'
#' Migrate a two-dimensional GPR profile directly from the acquisition
#' topography onto a regular distance-elevation grid. Transmitter and receiver
#' coordinates may differ, allowing bistatic or common-offset antenna
#' geometries. Coincident transmitter and receiver coordinates give the
#' zero-offset formulation.
#'
#' For every image point \eqn{\mathbf{p}}, the two-way travel time associated
#' with trace \eqn{j} is calculated as
#'
#' \deqn{
#' t_j(\mathbf{p}) =
#' \frac{
#'   \Vert \mathbf{p} - \mathbf{s}_j \Vert +
#'   \Vert \mathbf{p} - \mathbf{r}_j \Vert
#' }{v},
#' }
#'
#' where \eqn{\mathbf{s}_j} and \eqn{\mathbf{r}_j} are the transmitter and
#' receiver coordinates and \eqn{v} is the constant electromagnetic-wave
#' velocity. The input trace is linearly interpolated at this travel time and
#' its contribution is added to the image point.
#'
#' Migration is performed directly from the real acquisition surface; no
#' elevation static correction is applied. This follows the principle of
#' topographic Kirchhoff migration described by Dujardin and Bano (2013).
#'
#' @param x Numeric matrix containing the GPR amplitudes. Rows are time
#'   samples and columns are traces.
#' @param topoGPR Numeric vector giving the ground-surface elevation, in
#'   metres, at each position in `xpos`.
#' @param xpos Numeric vector giving the profile position, in metres, of each
#'   trace. Positions must be finite and strictly increasing.
#' @param dts Positive numeric scalar giving the temporal sampling interval in
#'   nanoseconds.
#' @param v GPR-wave interval velocity in metres per nanosecond. Either a
#'   positive scalar (constant velocity), a numeric vector of length `nrow(x)`
#'   (velocity varying with two-way time, i.e. with depth) or a numeric matrix
#'   of dimension `nrow(x)` x `ncol(x)` (velocity varying with depth and
#'   laterally). Vectors and matrices are defined in the time domain, exactly as
#'   for [convertTimeToDepth()]; the time axis is `(seq_len(nrow(x)) - 1) * dts`.
#' @param vel_mode Algorithm used for the travel times. `"auto"` (default)
#'   chooses from the type of `v`: scalar -> `"constant"`, vector ->
#'   `"layered"`, matrix -> `"general"`.
#'   \describe{
#'     \item{`"constant"`}{Travel time = distance / v (fastest).}
#'     \item{`"layered"`}{Horizontal layers. The velocity profile is applied
#'       as a function of depth below the highest trace, so layers are
#'       horizontal in the image. Travel times along straight rays are exact
#'       and computed in constant time from the cumulative slowness (about as
#'       fast as `"constant"`). With strong topography the layers do not follow
#'       the ground surface; for this use `"general"`.}
#'     \item{`"general"`}{Velocity varying with depth and position. The
#'       velocity is a function of depth below the LOCAL ground surface (it
#'       follows the topography, like in [convertTimeToDepth()]) and travel
#'       times are obtained by integrating the slowness along straight rays.
#'       A vector or scalar `v` is accepted and replicated for all traces,
#'       which gives topography-following layers.}
#'   }
#' @param vel_dx,vel_dz Horizontal and vertical spacing (m) of the regular grid
#'   on which the velocity model is resampled. Defaults: mean trace spacing and
#'   `dz`. Only used for `vel_mode = "general"`.
#' @param ray_step,n_ray For `vel_mode = "general"`, travel times are
#'   obtained by integrating the slowness along the straight
#'   antenna-to-pixel segments, with one sample every `ray_step` metres
#'   (default `max(vel_dx, vel_dz)`), but at most `n_ray` (default 16) and at
#'   least 2 samples per segment. Increase `n_ray` for strong velocity
#'   contrasts (computing time grows about linearly with it).
#' @param max_depth Positive numeric scalar giving the maximum vertical
#'   migration depth below the local ground surface, in metres.
#' @param dz Positive numeric scalar giving the vertical sampling interval of
#'   the migrated image, in metres.
#' @param xout Numeric vector giving the horizontal coordinates of the output
#'   image. By default, the input trace positions are used.
#' @param tx_x,rx_x Numeric vectors giving the horizontal transmitter and
#'   receiver coordinates for every trace. By default, both are equal to
#'   `xpos`, corresponding to zero-offset acquisition.
#' @param tx_z,rx_z Numeric vectors giving the transmitter and receiver
#'   elevations for every trace. By default, both are equal to `topoGPR`.
#' @param fdo Positive numeric scalar giving the antenna centre frequency in
#'   MHz. Required when `max_angle = NULL`.
#' @param max_angle Maximum migration aperture angle in degrees, measured from
#'   the vertical. A trace contributes only if both the transmitter-to-pixel
#'   and receiver-to-pixel angles do not exceed this value. Set to `90` to
#'   disable angle-based aperture restriction. If `NULL` (default), the
#'   aperture is derived for each image point from the first depth-dependent Fresnel zone
#'   (Pérez-Gracia et al., 2008): with wavelength
#'   \eqn{\lambda = 1000 v / f_{do}} and depth \eqn{d} below the local ground
#'   surface, the Fresnel radius is \eqn{r_f = 0.5\sqrt{2 \lambda d}} and the
#'   limiting angle is \eqn{\arctan(r_f / d)}. The aperture therefore
#'   narrows with depth.
#' @param weight Character string specifying the migration weighting:
#'   `"none"` applies only the spatial quadrature weights;
#'   `"obliquity"` additionally applies the symmetric bistatic
#'   obliquity factor
#'   \eqn{\sqrt{\cos(\theta_{tx})\cos(\theta_{rx})}};
#'   `"legacy"` applies the obliquity factor and the distance-dependent
#'   amplitude decay used by the former RGPR Kirchhoff implementation,
#'   proportional to
#'   \eqn{1 / \sqrt{2\pi t v}}.
#' @param normalize Logical. If `TRUE`, divide the migrated image by the sum of
#'   the absolute migration weights contributing to each pixel. This reduces
#'   amplitude variations caused by changing aperture size. It is not a
#'   display normalization.
#' @param spreading Logical. Compensate 2D geometrical spreading by
#'   multiplying each contribution by sqrt(d_tx * d_rx) (metres).
#' @param wavelet_filter "none" or "halfderiv" (2D wavelet-shaping filter).
#' @param antialias Logical. Low-pass each contribution with a triangle filter
#'   whose half-width equals the travel-time moveout between neighbouring
#'   traces (Lumley et al., 1994; Gray, 1992).
#' @param aa_factor Scaling of the anti-alias half-width (default 1).
#'
#' @return A numeric matrix with elevations in rows and horizontal positions
#'   in columns. Pixels above the ground surface or more than `max_depth`
#'   below it are returned as `NA_real_`.
#'
#'   Output coordinates are stored in attributes:
#'   \describe{
#'     \item{`x`}{Horizontal output coordinates.}
#'     \item{`z`}{Output elevations, ordered from high to low.}
#'     \item{`topography`}{Interpolated ground elevation at `xout`.}
#'   }
#'
#' @details
#' The main processing steps are:
#'
#' \enumerate{
#'   \item Construct a regular distance-elevation output grid.
#'   \item Mask image points outside the subsurface domain.
#'   \item Calculate transmitter-to-pixel and receiver-to-pixel distances.
#'   \item Select traces within the angular migration aperture.
#'   \item Calculate bistatic travel times.
#'   \item Linearly interpolate trace amplitudes at those travel times.
#'   \item Apply spatial-integration and optional obliquity weights.
#'   \item Sum the contributions into the migrated image.
#' }
#'
#' The implementation assumes a two-dimensional profile, isotropic velocity,
#' straight propagation paths, and coordinates expressed in metres. With a
#' non-constant velocity model, the travel time of each leg is the integral of
#' the slowness (1/v) along the straight segment between the antenna and the
#' image point (no ray bending, no refraction at the ground surface). The
#' Fresnel aperture uses the velocity at the image point, and the anti-aliasing
#' moveout uses the surface slowness at the antennas. The spreading
#' (\eqn{\sqrt{d_{tx} d_{rx}}}) and legacy \eqn{1/\sqrt{2\pi t v}} weights
#' use geometric path lengths. Antenna radiation patterns and out-of-plane
#' energy are not modelled.
#'
#' @references
#' Dujardin, J.-R. and Bano, M. (2013). Topographic migration of GPR
#' data: Examples from Chad and Mongolia. Comptes Rendus Geoscience,
#' 345(2), 73--80. \doi{10.1016/j.crte.2013.01.003}
#' 
#' Pérez-Gracia, V., Di Capua, D., Caselles, O., Rial, F., Lorenzo, H.,
#' González-Drigo, R. and Armesto, J. (2008). Horizontal resolution in a
#' non-destructive shallow GPR survey: An experimental evaluation.
#' NDT & E International, 41(8), 611--620.
#' \doi{10.1016/j.ndteint.2008.06.002}
#'
#' @keywords internal
#' @noRd
.kirMigTopo <- function(
    x, topoGPR, xpos, dts, v,
    fdo = NULL,
    max_depth = 8,
    dz = 0.025,
    xout = xpos,
    tx_x = xpos, rx_x = xpos,
    tx_z = topoGPR, rx_z = topoGPR,
    max_angle = NULL,
    weight = c("obliquity", "legacy", "none"),
    normalize = FALSE,
    spreading = FALSE,
    wavelet_filter = c("none", "halfderiv"),
    antialias = TRUE,
    aa_factor = 1,
    vel_mode = c("auto", "constant", "layered", "general"),
    vel_dx = NULL, vel_dz = NULL,
    ray_step = NULL, n_ray = 16L) {
  
  weight         <- match.arg(weight)
  wavelet_filter <- match.arg(wavelet_filter)
  
  # ---- Input checks -------------------------------------------------------
  if (!is.matrix(x) || !is.numeric(x)) stop("'x' must be a numeric matrix.")
  if (any(!is.finite(x)))
    stop("'x' must not contain NA/NaN/Inf; replace them (e.g. by 0) first.")
  nx <- ncol(x)
  if (nx < 2L) stop("At least two traces are required.")
  
  geometry <- list(topoGPR = topoGPR, xpos = xpos, tx_x = tx_x, rx_x = rx_x,
                   tx_z = tx_z, rx_z = rx_z)
  if (any(lengths(geometry) != nx))
    stop("All coordinate vectors must have length ncol(x).")
  if (any(!vapply(geometry, function(a) all(is.finite(a)), logical(1))))
    stop("All coordinate vectors must contain finite values.")
  
  pos <- function(a) is.numeric(a) && length(a) == 1L && is.finite(a) && a > 0
  if (!pos(dts))       stop("'dts' must be positive.")
  vel_mode <- match.arg(vel_mode)
  if (!is.numeric(v)) stop("'v' must be numeric.")
  if (vel_mode == "auto")
    vel_mode <- if (length(v) == 1L) "constant" else if (is.matrix(v)) "general" else "layered"
  if (vel_mode == "constant" && length(v) != 1L)
    stop("vel_mode = 'constant' requires a scalar 'v'.")
  if (vel_mode == "layered" && is.matrix(v))
    stop("vel_mode = 'layered' requires a scalar or a vector 'v', not a matrix.")
  if (any(!is.finite(v)) || any(v <= 0)) stop("'v' must contain finite positive values.")
  if (!is.numeric(n_ray) || length(n_ray) != 1L || n_ray < 2)
    stop("'n_ray' must be a single number >= 2.")
  if (!pos(max_depth)) stop("'max_depth' must be positive.")
  if (!pos(dz))        stop("'dz' must be positive.")
  if (!pos(aa_factor)) stop("'aa_factor' must be positive.")
  for (o in c("normalize", "spreading", "antialias"))
    if (!isTRUE(get(o)) && !isFALSE(get(o)))
      stop("'", o, "' must be TRUE or FALSE.")
  if (!is.numeric(xout) || any(!is.finite(xout)))
    stop("'xout' must contain finite numeric values.")
  if (is.unsorted(xpos, strictly = TRUE)) stop("'xpos' must be strictly increasing.")
  
  mid_x <- (tx_x + rx_x) / 2
  if (is.unsorted(mid_x, strictly = TRUE))
    stop("Antenna midpoint coordinates must be strictly increasing.")
  
  use_fresnel <- is.null(max_angle)
  lambda <- 0
  if (use_fresnel) {
    if (!pos(fdo)) stop("'fdo' must be positive when 'max_angle' is NULL.")
    max_angle <- 90
  } else if (!is.numeric(max_angle) || length(max_angle) != 1L ||
             !is.finite(max_angle) || max_angle <= 0 || max_angle > 90) {
    stop("'max_angle' must be NULL or a single value in (0, 90].")
  }
  
  # ---- Optional wavelet shaping ------------------------------------------
  if (wavelet_filter == "halfderiv") x <- .halfDerivative(x, dts)
  
  # ---- Output grid --------------------------------------------------------
  # The image is expressed as DEPTH BELOW THE HIGHEST TRACE (row 1 = depth 0
  # = max(topoGPR)), not as absolute elevation. Internally the C++ core works
  # with elevations relative to zmax, i.e. z_rel = z - zmax (<= 0), on a
  # decreasing grid zout = 0, -dz, -2*dz, ... All vertical quantities (grid,
  # ground surface, antenna positions) MUST use this same reference.
  zmax <- max(topoGPR)                       # reference = highest trace
  zmin <- min(topoGPR)
  ndep <- (zmax - zmin) + max_depth          # deepest depth below zmax (m)
  nz   <- floor(ndep / dz + 1e-9) + 1L
  depth_out <- dz * (seq_len(nz) - 1L)       # depth below zmax (m), increasing
  zout <- -depth_out                         # relative elevation, decreasing
  
  surface <- stats::approx(xpos, topoGPR, xout = xout, rule = 1)$y - zmax
  tx_z    <- tx_z - zmax
  rx_z    <- rx_z - zmax
  
  # ---- Spatial quadrature weights ----------------------------------------
  dmx <- diff(mid_x)
  wq  <- (c(dmx, 0) + c(0, dmx)) / 2
  
  # ---- Velocity model -----------------------------------------------------
  # constant : nothing to prepare (fast path, original algorithm)
  # layered  : cumulative slowness Sigma(depth) on the output rows
  # general  : slowness grid in the (x, elevation) frame of the image
  tt <- (seq_len(nrow(x)) - 1L) * dts
  cum  <- numeric(0)
  slow <- matrix(0, 0, 0); xv0 <- 0; dxv <- 1
  if (vel_mode == "constant") {
    # nothing to prepare
  } else if (vel_mode == "layered") {
    cum   <- .kirLayerSigma(v, tt, depth_out)$sigma
  } else {
    sg <- .kirSlownessGrid(
      v, tt = tt, xpos = xpos, topoGPR = topoGPR,
      xrange = range(c(xout, tx_x, rx_x)), zout = zout, zmax = zmax, dz = dz,
      vel_dx = vel_dx, vel_dz = vel_dz)
    slow <- sg$slow; xv0 <- sg$xv0; dxv <- sg$dxv
    if (is.null(ray_step)) ray_step <- max(sg$vel_dx, sg$vel_dz)
  }
  if (is.null(ray_step)) ray_step <- 1
  lambda_per_v <- if (use_fresnel) 1000 / fdo else 0   # lambda = 1000 v / fdo
  
  weight_type <- switch(
    weight,
    none = 0L,
    obliquity = 1L,
    legacy = 2L
  )
  
  x[is.na(x)] <- 0
  
  if (vel_mode == "constant") {
    # original constant-velocity core: no velocity model at all
    out <- kirMigTopoConst_cpp(
      x = x, tx_x = tx_x, tx_z = tx_z, rx_x = rx_x, rx_z = rx_z, wq = wq,
      xout = xout, surface = surface, zout = zout, dz = dz,
      dts = dts, v = v, max_depth = max_depth,
      use_fresnel = use_fresnel,
      lambda = if (use_fresnel) 1000 * v / fdo else 0,
      max_angle = max_angle * pi / 180,
      weight_type = weight_type, spreading = spreading,
      normalize = normalize, antialias = antialias, aa_factor = aa_factor
    )
  } else {
    out <- kirMigTopoVar_cpp(
      x = x, tx_x = tx_x, tx_z = tx_z, rx_x = rx_x, rx_z = rx_z, wq = wq,
      xout = xout, surface = surface, zout = zout, dz = dz,
      dts = dts, max_depth = max_depth,
      use_fresnel = use_fresnel,
      lambda_per_v = lambda_per_v,
      max_angle = max_angle * pi / 180,
      weight_type = weight_type, spreading = spreading,
      normalize = normalize, antialias = antialias, aa_factor = aa_factor,
      vel_mode = c(layered = 1L, general = 2L)[[vel_mode]],
      cum = cum, slow = slow, xv0 = xv0, dxv = dxv,
      ray_step = ray_step, n_ray_max = as.integer(n_ray)
    )
  }
  # x, tx_x, tx_z, rx_x, rx_z, wq, xout, surface, zout, dz,
  # dts, v, max_depth,
  # use_fresnel, lambda, max_angle * pi / 180,
  # weight == "obliquity", spreading, normalize,
  # antialias, aa_factor)
  
  # Depth (m) below the highest trace; row 1 = 0. Absolute elevation of a row
  # is  zmax - depth  (stored in attr "elevation").
  # attr(out, "x")          <- xout
  attr(out, "z")          <- depth_out          # depth below max(topoGPR)
  # attr(out, "elevation")  <- zmax - depth_out
  attr(out, "zmax")       <- zmax
  # attr(out, "topography") <- surface + zmax     # absolute ground elevation
  out
}

