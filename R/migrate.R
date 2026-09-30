#--------------------- 'migration()' = DEPRECATED -----------------------------#
#' @name migration
#' @rdname migrate
#' @export
setGenericVerif("migration", function(x, type = c("static", "kirchhoff"), ...) 
  standardGeneric("migration"))


#' Deprecated
#'
#' @name migration
#' @rdname migrate
#' @export
setMethod("migration", "GPR", function(x, type = c("static", "kirchhoff"), ...){
  message("Soon deprecated. Use 'migrate()' instead of 'migration()'.")
  migrate(x, type = type, ...)
})

#----------------------------------------------------------------------------- #

#' @name migrate
#' @rdname migrate
#' @export
setGenericVerif("migrate", function(x, type = c("static", "kirchhoff"), ...) 
  standardGeneric("migrate"))


# max_depth = to which depth should the migration be performed
# dz = vertical resolution of the migrated data
# fdo = dominant frequency of the GPR signal

# for static time-to-depth migration 
# dz = depth resolution for the time to depth conversion. If dz = NULL, then
#      dz is set equal to the smallest depth resolution computed from x_depth.
# d_max = maximum depth for the time to depth conversion. If d_max = NULL, then
#         d_max is set equal to the largest depth in x_depth.
# method = method for the interpolation (see ?signal::interp1)

#' Migrate of the GPR data
#' 
#' Migrate the GPR data (time-to-depth conversion accounting for the 
#' topography). 
#' 
#' \itemize{
#'   \item \code{static} a trace-by-trace time-to-depth conversion that
#'         shifts the traces vertically according to their elevation.
#'   \item \code{kirchhoff} Topographic Kirchhoff migration (a Krichhoff
#'         migration that accounts for the topography). See Dujardin and Bano 
#'         (2013) Topographic migration of GPR data: Examples from Chad and 
#'         Mongolia, Comptes Rendus Géoscience, 345(2):73-80. 
#'         Doi: 10.1016/j.crte.2013.01.003
#' }
#' 
#' Fresnel zone (used in the Kirchhoff migration) is defined according to 
#' Perez-Gracia et al. (2008) Horizontal resolution in a non-destructive
#' shallow GPR survey: An experimental evaluation. NDT & E International,
#' 41(8): 611-620.
#' doi:10.1016/j.ndteint.2008.06.002
#'
#' @param type      Either \code{static} or \code{kirchhoff}. See Details.
#' @param max_depth maximum depth to appply the migration
#' @param dz        vertical resolution of the migrated data
#' @param fdo       dominant frequency of the GPR signal
#' 
#' @name migrate
#' @rdname migrate
#' @export
setMethod("migrate", "GPR", function(x, type = c("static", "kirchhoff"), ...){
  if(length(x@antsep) == 0 || (!is.numeric(x@antsep))){
    stop("You must first define the antenna separation ",
         "with 'antsep(x) <- 1' for example!")
  }
  if(is.null(x@vel) || length(x@vel)==0){
    stop("You must first define the EM wave velocity ",
         "with 'vel(x) <- 0.1' for example!")
  }
  if(length(x@coord) != 0 && ncol(x@coord) == 3){
    topo <- x@coord[, 3]
    
  }else{
    topo <- rep.int(0L, ncol(x@data))
    message("Trace vertical position set to zero!")
  }
  
  type <- match.arg(type, c("static", "kirchhoff"))
  
  if(type == "static"){  
    x <- convertTimeToDepth(x, track = FALSE, ...)
  }else if(type == "kirchhoff"){
    A <- x@data
    #topo <- x@coord[,3]
    dx <- x@dx
    dts <- x@dz
    # interval velocity (m/ns): scalar, vector (length nrow(x)) or matrix
    # (nrow(x) x ncol(x)), same convention as in convertTimeToDepth()
    v <- .getVel2(x, type = "vint", strict = FALSE)
    # initialisation
    #max_depth <- nrow(x)*x@dx
    if(length(v) == 1L){
      max_depth <- max(x@depth) * v / 2 * 0.9
    }else{
      tt <- x@depth - x@depth[1]
      vm <- if(is.matrix(v)) v else matrix(v, nrow = length(v), ncol = ncol(x))
      max_depth <- max(apply(c(0, diff(tt)) * vm / 2, 2, sum)) * 0.9
    }
    dz <- 0.25 * x@dz
    fdo <- x@freq
    FUN <- sum
    antialias      <- FALSE
    aa_factor      <- 1
    spreading      <- TRUE
    wavelet_filter <- "halfderiv"   # "none", "halfderiv"
    weight         <- "obliquity"   # "obliquity", "legacy", "none"
    normalize      <- FALSE
    dots <- list(...)
    if( !is.null(dots$max_depth))      max_depth      <- dots$max_depth
    if( !is.null(dots$dz))             dz             <- dots$dz
    if( !is.null(dots$fdo))            fdo            <- dots$fdo
    if( !is.null(dots$FUN))            FUN            <- dots$FUN
    if( !is.null(dots$antialias))      antialias      <- dots$antialias
    if( !is.null(dots$aa_factor))      aa_factor      <- dots$aa_factor
    if( !is.null(dots$spreading))      spreading      <- dots$spreading
    if( !is.null(dots$wavelet_filter)) wavelet_filter <- dots$wavelet_filter
    if( !is.null(dots$weight))         weight         <- dots$weight
    if( !is.null(dots$normalize))      normalize      <- dots$normalize
    # variable-velocity ray sampling (ignored for a constant velocity)
    vel_dx    <- NULL   # horizontal spacing of the velocity grid (m)
    vel_dz    <- NULL   # vertical spacing of the velocity grid (m)
    ray_step  <- NULL   # spacing of the samples along a ray (m)
    n_ray     <- 16L    # maximum number of samples per ray leg
    vel_mode  <- "auto" # "auto", "constant", "layered", "general"
    if( !is.null(dots$vel_mode))       vel_mode       <- dots$vel_mode
    if( !is.null(dots$vel_dx))         vel_dx         <- dots$vel_dx
    if( !is.null(dots$vel_dz))         vel_dz         <- dots$vel_dz
    if( !is.null(dots$ray_step))       ray_step       <- dots$ray_step
    if( !is.null(dots$n_ray))          n_ray          <- dots$n_ray
    
    # attributes:
    #   - "z"
    #   - "zmax"
    xkir <-.kirMigTopo(x@data, topoGPR = topo, xpos = x@pos, dts = x@dz, v = v, 
                       fdo = fdo, max_depth = max_depth, dz = dz, xout = x@pos,
                       tx_x = x@pos, rx_x = x@pos, tx_z = topo, rx_z = topo,
                       max_angle = NULL,  weight = weight,
                       normalize = normalize, spreading = spreading,
                       wavelet_filter = wavelet_filter, # "none", # "halfderiv"),
                       antialias = antialias,
                       aa_factor = aa_factor,
                       vel_mode = vel_mode, vel_dx = vel_dx, vel_dz = vel_dz,
                       ray_step = ray_step, n_ray = n_ray)
    x@data <- unclass(xkir)
    # rows = depth below max(topo); the trace elevation stays in @coord
    x@depth     <- attr(xkir, "z") # seq(0,by=dz, length.out = nrow(x))
    x@time0     <- rep(0, ncol(x))
    x@dz        <- dz
    x@depthunit <- x@posunit          # check!!!
    # x@data      <- .kirMig(x@data, topoGPR = topo, xpos = x@pos,
    #                        dts = x@dz, v = v, max_depth = max_depth, 
    #                        dz = dz, fdo = fdo, FUN = FUN)
    # x@depth     <- seq(0,by=dz, length.out = nrow(x))
    # x@time0     <- rep(0, ncol(x))
    # x@dz        <- dz
    # x@depthunit <- x@posunit          # check!!!
    if(length(x@coord) > 0 && ncol(x@coord) == 3 ){
      x@coord[,3] <- max(x@coord[,3])
    }
  }
  proc(x) <- getArgs()
  return(x)
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
#'
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


# x = data matrix (col = traces)
# topoGPR = z-coordinate of each trace
# dx = spatial sampling (trace spacing)
# dts = temporal sampling
# v = GPR wave velocity (ns)
# max_depth = to which depth should the migration be performed
# dz = vertical resolution of the migrated data
# fdo = dominant frequency of the GPR signal
.kirMig <- function(x, topoGPR, xpos, dts, v, max_depth = 8, 
                    dz = 0.025, fdo = 80, FUN = sum){
  n <- nrow(x)
  m <- ncol(x)
  z <- max(topoGPR) - topoGPR
  fdo <- fdo * 10^6   # from MHz to Hz
  lambda <- fdo / v * 10^-9
  v2 <- v^2
  # message("max depth1 = ", max_depth)
  max_depth <- max_depth + max(z)
  # message("max depth2 = ", max_depth)
  kirTopoGPR <- matrix(0, nrow = floor(max_depth/dz) + 1, ncol = m)
  
  dx <- mean(diff(xpos))
  for( i in seq_len(m)){
    x_d <- xpos[i]   # diffraction
    z_d <- seq(z[i], max_depth, by = dz)
    for(k in seq_along(z_d)){
      t_0 <- 2*(z_d[k] - z[i])/v    # = k * dts in reality
      # z_idx <- round(z_d[k] /dz + 1)
      z_idx <-  floor(z_d[k] /dz) + 1
      # print(z_idx)
      # if(z_idx <= nrow(kirTopoGPR) && z_idx > 0){
      # Fresnel zone
      # Pérez-Gracia et al. (2008) Horizontal resolution in a non-destructive
      # shallow GPR survey: An experimental evaluation. NDT & E International,
      # 41(8): 611–620. doi:10.1016/j.ndteint.2008.06.002
      rf <- 0.5 * sqrt(lambda * 2 * (z_d[k] - z[i]))
      rf_tr <- round(rf/dx)
      mt <- (i - rf_tr):(i + rf_tr)
      mt <- mt[mt > 0 & mt <= m]
      
      lmt <- length(mt)
      Ampl <- numeric(lmt)
      for(j in mt){
        # x_a <- (j-1)*dx
        x_a <- xpos[j]
        t_top <-  t_0 - 2*(z[j] - z[i])/v
        t_x <- sqrt( t_top^2 +   4*(x_a - x_d)^2 /v2)
        t1 <- floor(t_x/dts) + 1 # the largest integers not greater
        t2 <- ceiling(t_x/dts) + 1 # smallest integers not less
        if(t2 <= n && t1 > 0 && t_x != 0){
          w <- ifelse(t1 != t2, abs((t1 - t_x)/(t1 - t2)), 0)
          # Dujardin & Bano amplitude factor weight: cos(alpha) = t_top/t_x
          # Ampl[j- mt[1] + 1] <- (t_top/t_x) * 
          # ((1-w)*A[t1,j] + w*A[t2,j])
          # http://sepwww.stanford.edu/public/docs/sep87/SEP087.Bevc.pdf
          Ampl[j- mt[1] + 1] <- (dx/sqrt(2*pi*t_x*v))*
            (t_top/t_x) * 
            ((1-w)*x[t1,j] + w*x[t2,j])
        }
        # }
        kirTopoGPR[z_idx, i] <- FUN(Ampl)
      }
    }
  }
  kirTopoGPR <- kirTopoGPR/max(kirTopoGPR, na.rm=TRUE) * 50
  #   kirTopoGPR2 <- kirTopoGPR
  
  return(kirTopoGPR)
}