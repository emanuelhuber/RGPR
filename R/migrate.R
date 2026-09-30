
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
    v <- x@vel[[1]]
    # initialisation
    #max_depth <- nrow(x)*x@dx
    max_depth <- max(x@depth) * v / 2 * 0.9
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
                       aa_factor = aa_factor)
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
#' @param v Positive numeric scalar giving the constant GPR-wave velocity in
#'   metres per nanosecond.
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
#' The implementation assumes a two-dimensional profile, a constant and
#' isotropic velocity, straight propagation paths, and coordinates expressed
#' in metres. It does not model refraction at the ground surface, lateral
#' velocity variation, antenna radiation patterns, or out-of-plane energy.
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
    aa_factor = 1) {
  
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
  if (!pos(v))         stop("'v' must be positive.")
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
    lambda <- 1000 * v / fdo                 # metres (v in m/ns, fdo in MHz)
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
  
  weight_type <- switch(
    weight,
    none = 0L,
    obliquity = 1L,
    legacy = 2L
  )
  
  out <- kirMigTopo_cpp(
    x = x,
    tx_x = tx_x,
    tx_z = tx_z,
    rx_x = rx_x,
    rx_z = rx_z,
    wq = wq,
    xout = xout,
    surface = surface,
    zout = zout,
    dz = dz,
    dts = dts,
    v = v,
    max_depth = max_depth,
    use_fresnel = use_fresnel,
    lambda = lambda,
    max_angle = max_angle * pi / 180,
    weight_type = weight_type,
    spreading = spreading,
    normalize = normalize,
    antialias = antialias,
    aa_factor = aa_factor
  )
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