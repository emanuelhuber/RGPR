// ============================================================================
// Topographic Kirchhoff migration for 2D GPR profiles (bistatic geometry)
//
// Usage:   Rcpp::sourceCpp("kirMigTopo.cpp")
//          mig <- .kirMigTopo(x, topoGPR, xpos, dts = 0.5, v = 0.1, fdo = 250)
//
// The file contains two independent C++ cores (heavy triple loop, no
// validation), chosen by the R wrapper `.kirMigTopo()` (see migrate.R):
//
//   1. kirMigTopoConst_cpp()  CONSTANT velocity: distance / v. No grids, no
//                             velocity model, no extra work per ray.
//   2. kirMigTopoVar_cpp()    VARIABLE velocity (see below), at the end of
//                             the file.
//
// Velocity models of kirMigTopoVar_cpp() (vel_mode):
//   1  layered velocity  : v depends on depth only (horizontal layers). `cum`
//                          holds the one-way slowness integral
//                          Sigma(z) = int s dz on the output rows. For a
//                          straight ray the leg time is then exact and O(1):
//                          t = L / |dz| * |Sigma(z_pixel) - Sigma(z_antenna)|.
//   2  general velocity  : v depends on depth and position. `slow` is a
//                          slowness grid (1/v, ns/m) on the output rows and on
//                          a regular horizontal grid (xv0, dxv). The travel
//                          time of each leg is the line integral of slowness
//                          along the STRAIGHT antenna-pixel segment (bilinear
//                          sampling, midpoint rule).
// Ray bending is not modelled.
//
// Added compared with the pure R version:
//   * spreading  : 2D geometrical-spreading compensation, w *= sqrt(d_tx*d_rx)
//   * wavelet_filter = "halfderiv": 2D Kirchhoff wavelet shaping, i.e. a
//                    half-derivative (sqrt(i*omega)) applied along time
//   * antialias  : triangle low-pass filter on every trace contribution,
//                  with half-width set by the local operator dip
//
// Assumptions: row 1 of `x` is t = 0 (time-zero corrected data), no NA in x,
// constant velocity, straight rays (rays through air are NOT checked).
// ============================================================================

#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
using namespace Rcpp;

// ---------------------------------------------------------------------------
// Evaluate the double cumulative sum C2 of one trace at a fractional
// (zero-based) sample coordinate u, by linear interpolation.
//
//   C1(k) = sum_{m<=k} x(m),   C2(i) = sum_{k<=i} C1(k)
//
// The trace is assumed to be zero before sample 0 and after the last sample:
//   * for u <= -1      : C2 = 0
//   * for -1 < u < 0   : linear ramp from 0 (at -1) to C2(0) (at 0)
//   * for u >= nt - 1  : C1 stays equal to its total S1, so C2 grows linearly
// ---------------------------------------------------------------------------
static inline double c2_eval(const double* c2, int nt, double S1, double u) {
  if (u <= -1.0) return 0.0;
  if (u < 0.0)   return c2[0] * (u + 1.0);
  if (u >= nt - 1) return c2[nt - 1] + S1 * (u - (nt - 1));
  int    i = (int) std::floor(u);
  double f = u - i;
  return (1.0 - f) * c2[i] + f * c2[i + 1];
}

// ---------------------------------------------------------------------------
// Core migration routine (all inputs are already validated by the R wrapper).
//
// x          nt x nx amplitudes (rows = time samples, columns = traces)
// tx_x,tx_z  transmitter coordinates per trace   (length nx)
// rx_x,rx_z  receiver coordinates per trace      (length nx)
// wq         spatial quadrature weight per trace (length nx)
// xout       horizontal output coordinates       (length nxo)
// surface    ground elevation at xout (NA outside the profile)
// zout       output elevations, decreasing, regular spacing dz (length nz)
// dts        sampling interval (ns);  v velocity (m/ns)
// max_depth  maximum depth below local ground surface (m)
// use_fresnel  TRUE: depth-dependent aperture from the first Fresnel zone
// lambda     wavelength (m), used only when use_fresnel
// max_angle  fixed aperture angle in radians, used only when !use_fresnel
// obliquity, spreading, normalize, antialias : logical options
// aa_factor  scales the anti-alias filter half-width (1 = nominal)
// weight_type:
// 0 = no angular or legacy weighting
// 1 = bistatic obliquity weighting
// 2 = legacy RGPR weighting
// w *= obliquity / sqrt(2 * pi * travel_time * v)
//
// spreading:
// Optional geometrical-spreading compensation. This should normally
// remain FALSE with weight_type == 2 because legacy weighting already
// includes distance-dependent amplitude decay.
// ---------------------------------------------------------------------------
// [[Rcpp::export]]
NumericMatrix kirMigTopoConst_cpp(
    const NumericMatrix& x,
    const NumericVector& tx_x, const NumericVector& tx_z,
    const NumericVector& rx_x, const NumericVector& rx_z,
    const NumericVector& wq,
    const NumericVector& xout, const NumericVector& surface,
    const NumericVector& zout, double dz,
    double dts, double v, double max_depth,
    bool use_fresnel, double lambda, double max_angle,
    int weight_type, bool spreading, bool normalize,
    bool antialias, double aa_factor) {
  
  const int nt  = x.nrow();
  const int nx  = x.ncol();
  const int nxo = xout.size();
  const int nz  = zout.size();
  const double INF = std::numeric_limits<double>::infinity();
  
  const double TWO_PI = 2.0 * std::acos(-1.0);
  
  NumericMatrix out(nz, nxo);
  std::fill(out.begin(), out.end(), NA_REAL);
  
  if (weight_type < 0 || weight_type > 2) {
    stop("'weight_type' must be 0, 1 or 2.");
  }
  
  // ---- Per-trace pre-computations ---------------------------------------
  // Trace midpoints (sorted, checked in R): used to select candidate traces
  // with a binary search instead of scanning all traces for every pixel.
  std::vector<double> mid(nx);
  double off_max = 0.0;   // largest horizontal distance antenna <-> midpoint
  double z_ant_max = -INF; // highest antenna elevation in the profile
  for (int j = 0; j < nx; ++j) {
    mid[j] = 0.5 * (tx_x[j] + rx_x[j]);
    off_max = std::max(off_max, std::max(std::fabs(tx_x[j] - mid[j]),
                                         std::fabs(rx_x[j] - mid[j])));
    z_ant_max = std::max(z_ant_max, std::max(tx_z[j], rx_z[j]));
  }
  
  // Trace-to-trace step of each antenna (central differences). They give the
  // local travel-time moveout between neighbouring traces for anti-aliasing.
  std::vector<double> stx_x(nx, 0.0), stx_z(nx, 0.0),
  srx_x(nx, 0.0), srx_z(nx, 0.0);
  for (int j = 0; j < nx; ++j) {
    int a = std::max(j - 1, 0), b = std::min(j + 1, nx - 1);
    double n = (double)(b - a);           // 1 at the ends, 2 inside
    stx_x[j] = (tx_x[b] - tx_x[a]) / n;
    stx_z[j] = (tx_z[b] - tx_z[a]) / n;
    srx_x[j] = (rx_x[b] - rx_x[a]) / n;
    srx_z[j] = (rx_z[b] - rx_z[a]) / n;
  }
  
  // Double cumulative sums for the O(1) triangle filter (anti-aliasing).
  // A triangle of half-width L convolved with a trace equals
  //   [C2(u + L) - 2*C2(u) + C2(u - L)] / L^2.
  std::vector<double> c2, S1;
  if (antialias) {
    c2.assign((size_t)nt * nx, 0.0);
    S1.assign(nx, 0.0);
    for (int j = 0; j < nx; ++j) {
      double c1 = 0.0, cc = 0.0;
      for (int i = 0; i < nt; ++i) {
        c1 += x(i, j);
        cc += c1;
        c2[(size_t)j * nt + i] = cc;
      }
      S1[j] = c1;
    }
  }
  
  const double eps = 1e-12;
  const double aa_min = 1.0;      // below this half-width (samples) skip filter
  
  // ---- Loop over output columns -----------------------------------------
  for (int ix = 0; ix < nxo; ++ix) {
    if (!R_finite(surface[ix])) continue;
    if ((ix & 31) == 0)
      Rcpp::checkUserInterrupt();
    const double xo = xout[ix];
    const double surf = surface[ix];
    
    // Range of rows [k0, k1] between the ground surface and max_depth below
    // it. zout[k] = zout[0] - dz * k, so the range follows from arithmetic;
    // the while loops only correct floating-point rounding.
    const double zlo = surf - max_depth;
    int k0 = (int) std::ceil((zout[0] - surf) / dz);
    int k1 = (int) std::floor((zout[0] - zlo) / dz);
    k0 = std::max(k0, 0);
    k1 = std::min(k1, nz - 1);
    while (k0 > 0 && zout[k0 - 1] <= surf) --k0;
    while (k0 < nz && zout[k0] > surf) ++k0;
    while (k1 + 1 < nz && zout[k1 + 1] >= zlo) ++k1;
    while (k1 >= 0 && zout[k1] < zlo) --k1;
    
    for (int k = k0; k <= k1; ++k) {
      const double zk = zout[k];
      
      // ---- Aperture: tan of the limiting angle ---------------------------
      // A trace passes if |dx| <= dz_vertical * tan_lim for both antennas.
      // Fresnel: rf = 0.5*sqrt(2*lambda*d)  =>  tan = rf/d = 0.5*sqrt(2*lambda/d)
      double tan_lim;
      if (use_fresnel) {
        double depth = surf - zk;
        tan_lim = (depth > 0.0) ? 0.5 * std::sqrt(2.0 * lambda / depth) : INF;
      } else {
        tan_lim = (max_angle >= M_PI / 2.0 - 1e-12) ? INF : std::tan(max_angle);
      }
      
      // ---- Candidate trace window via binary search on midpoints ---------
      double zab = z_ant_max - zk;        // largest possible vertical distance
      if (zab <= 0.0) continue;           // pixel above every antenna
      double reach = tan_lim * zab + off_max;   // may be Inf
      int jlo = 0, jhi = nx;
      if (std::isfinite(reach)) {
        jlo = (int)(std::lower_bound(mid.begin(), mid.end(), xo - reach) - mid.begin());
        jhi = (int)(std::upper_bound(mid.begin(), mid.end(), xo + reach) - mid.begin());
      }
      
      double value = 0.0, wsum = 0.0;
      int    count = 0;
      
      for (int j = jlo; j < jhi; ++j) {
        // Antenna-to-pixel vectors (horizontal h, vertical vz = antenna - pixel)
        const double hx = tx_x[j] - xo, hr = rx_x[j] - xo;
        const double vt = tx_z[j] - zk, vr = rx_z[j] - zk;
        
        // Pixel must lie below both antennas
        if (vt <= 0.0 || vr <= 0.0) continue;
        
        // Aperture restriction.
        // Angle from vertical <= limit  <=>  |h| <= vz * tan(limit)
        if (std::fabs(hx) > vt * tan_lim || std::fabs(hr) > vr * tan_lim) continue;
        
        const double dt_ = std::sqrt(hx * hx + vt * vt);   // transmitter-pixel
        const double dr_ = std::sqrt(hr * hr + vr * vr);   // pixel-receiver
        
        // Bistatic travel time in ns.
        const double travel_time = (dt_ + dr_) / v;
        
        // Bistatic two-way time -> fractional zero-based sample coordinate
        const double u = (dt_ + dr_) / v / dts;
        if (u > nt - 1 + eps) continue;                    // beyond the record
        
        // ---- Amplitude: linear interpolation or anti-aliased ------------
        const double* xc = &x[(size_t)j * nt];
        double amp = 0.0;
        double L = 0.0;                                    // half-width (samples)
        if (antialias) {
          // d(twt)/d(trace index): derivative of both ray lengths with respect
          // to the antenna positions times the antenna step per trace.
          double dtdj = ((hx * stx_x[j] + vt * stx_z[j]) / dt_ +
                         (hr * srx_x[j] + vr * srx_z[j]) / dr_) / v;
          L = aa_factor * std::fabs(dtdj) / dts;
        }
        if (antialias && L >= aa_min) {
          const double* cj = &c2[(size_t)j * nt];
          amp = (c2_eval(cj, nt, S1[j], u + L) - 2.0 * c2_eval(cj, nt, S1[j], u) +
            c2_eval(cj, nt, S1[j], u - L)) / (L * L);
        } else {
          int    i1 = (int) std::floor(u);
          double f  = u - i1;
          amp = xc[i1];
          if (f > eps && i1 + 1 < nt) amp = (1.0 - f) * amp + f * xc[i1 + 1];
        }
        
        // ---- Weights -----------------------------------------------------
        // double w = wq[j];                                  // spatial quadrature
        // if (obliquity) w *= std::sqrt((vt / dt_) * (vr / dr_));  // cos of both legs
        // if (spreading) w *= std::sqrt(dt_ * dr_);          // 2D spreading (1/sqrt(r) per leg)
        
        // Spatial quadrature weight, expressed in metres.
        double w = wq[j];
        
        // Cosines of the transmitter and receiver ray angles relative
        // to the vertical direction.
        const double cos_tx = vt / dt_;
        const double cos_rx = vr / dr_;
        
        // Symmetric bistatic obliquity factor.
        //
        // For zero-offset geometry:
        //   dt_ == dr_
        //   vt  == vr
        //
        // and this reduces to cos(alpha), as in the old RGPR code.
        const double obliquity_weight = std::sqrt(cos_tx * cos_rx);
        
        if (weight_type == 1) {
          
          // Bistatic obliquity weighting only.
          w *= obliquity_weight;
          
        } else if (weight_type == 2) {
          
          // Legacy RGPR weighting:
          //   dx / sqrt(2 * pi * t_x * v) * cos(alpha)
          // Here:
          //   travel_time = (dt_ + dr_) / v
          // and therefore:
          //   travel_time * v = dt_ + dr_
          // where dt_ and dr_ are geometric path lengths in metres.
          const double total_path = dt_ + dr_;
          
          if (total_path > 0.0) {
            w *= obliquity_weight /
              std::sqrt(TWO_PI * total_path);
          } else {
            continue;
          }
        }
        
        // Optional spreading compensation.
        //
        // Do not normally combine this with legacy weighting because the
        // two options have opposing distance dependence.
        if (spreading) {
          w *= std::sqrt(dt_ * dr_);
        }
        
        value += w * amp;
        wsum  += std::fabs(w);
        ++count;
      }
      
      if (count == 0) continue;                            // leave NA
      if (normalize && wsum > 0.0) value /= wsum;
      out(k, ix) = value;
    }
  }
  return out;
}


// ---------------------------------------------------------------------------
// Slowness grid with bilinear interpolation (column-major, nz rows x nxv cols).
// Row k lies at elevation z0 - k*dz, column i at horizontal position x0 + i*dx.
// Queries outside the grid are clamped to the nearest edge value.
// ---------------------------------------------------------------------------
struct SlowGrid {
  const double* s;
  int nz, nxv;
  double z0, dz, x0, dx;
  
  inline double at(double xq, double zq) const {
    double fx = (xq - x0) / dx;
    double fz = (z0 - zq) / dz;
    if (fx < 0.0) fx = 0.0; else if (fx > nxv - 1) fx = nxv - 1;
    if (fz < 0.0) fz = 0.0; else if (fz > nz - 1)  fz = nz - 1;
    int i = std::min((int) fx, nxv - 2);
    int k = std::min((int) fz, nz - 2);
    const double wx = fx - i, wz = fz - k;
    const double* p = s + (size_t) i * nz + k;
    const double* q = p + nz;
    return (1.0 - wx) * ((1.0 - wz) * p[0] + wz * p[1]) +
      wx  * ((1.0 - wz) * q[0] + wz * q[1]);
  }
  
  // One-way travel time along the straight segment a -> p of length len:
  // integral of slowness (midpoint rule with n samples).
  inline double leg(double xa, double za, double xp, double zp, double len,
                    double ray_step, int n_max) const {
    int n = (int) std::ceil(len / ray_step);
    n = std::max(2, std::min(n, n_max));
    const double inv = 1.0 / n;
    double sum = 0.0;
    for (int m = 0; m < n; ++m) {
      const double t = (m + 0.5) * inv;
      sum += at(xa + (xp - xa) * t, za + (zp - za) * t);
    }
    return sum * len * inv;
  }
};

// ---------------------------------------------------------------------------
// Layered model: cumulative one-way slowness Sigma(z) tabulated on the output
// rows (row k at elevation z0 - k*dz), linear interpolation, clamped at ends.
// ---------------------------------------------------------------------------
struct Layered {
  const double* c;
  int nz;
  double z0, dz;
  
  inline double sigma(double zq) const {
    double f = (z0 - zq) / dz;
    if (f < 0.0) f = 0.0; else if (f > nz - 1) f = nz - 1;
    int k = std::min((int) f, nz - 2);
    const double w = f - k;
    return (1.0 - w) * c[k] + w * c[k + 1];
  }
  // local slowness = dSigma/d(depth)
  inline double slowness(double zq) const {
    double f = (z0 - zq) / dz;
    if (f < 0.0) f = 0.0; else if (f > nz - 1) f = nz - 1;
    int k = std::min((int) f, nz - 2);
    return (c[k + 1] - c[k]) / dz;
  }
};

// ---------------------------------------------------------------------------
// Core migration routine (all inputs are already validated by the R wrapper).
//
// x          nt x nx amplitudes (rows = time samples, columns = traces)
// tx_x,tx_z  transmitter coordinates per trace   (length nx)
// rx_x,rx_z  receiver coordinates per trace      (length nx)
// wq         spatial quadrature weight per trace (length nx)
// xout       horizontal output coordinates       (length nxo)
// surface    ground elevation at xout (NA outside the profile)
// zout       output elevations, decreasing, regular spacing dz (length nz)
// dts        sampling interval (ns)
// max_depth  maximum depth below local ground surface (m)
// use_fresnel  TRUE: depth-dependent aperture from the first Fresnel zone
// lambda_per_v  1000 / fdo (fdo in MHz), so that lambda[m] = lambda_per_v *
//            v[m/ns]. The velocity at the image point is used. Only needed
//            when use_fresnel
// vel_mode   1 layered, 2 general (see top of the file)
// cum        vel_mode 1: Sigma(z) (ns/2 per row), length nz (else empty)
// slow       vel_mode 2: slowness grid (ns/m), nz x nxv (else empty)
// xv0, dxv   first column position and column spacing of `slow` (m)
// ray_step   target spacing (m) of the samples along a ray
// n_ray_max  maximum number of samples per ray leg (cost control)
// max_angle  fixed aperture angle in radians, used only when !use_fresnel
// obliquity, spreading, normalize, antialias : logical options
// aa_factor  scales the anti-alias filter half-width (1 = nominal)
// weight_type:
// 0 = no angular or legacy weighting
// 1 = bistatic obliquity weighting
// 2 = legacy RGPR weighting
// w *= obliquity / sqrt(2 * pi * travel_time * v)
//
// spreading:
// Optional geometrical-spreading compensation. This should normally
// remain FALSE with weight_type == 2 because legacy weighting already
// includes distance-dependent amplitude decay.
// ---------------------------------------------------------------------------
// [[Rcpp::export]]
NumericMatrix kirMigTopoVar_cpp(
    const NumericMatrix& x,
    const NumericVector& tx_x, const NumericVector& tx_z,
    const NumericVector& rx_x, const NumericVector& rx_z,
    const NumericVector& wq,
    const NumericVector& xout, const NumericVector& surface,
    const NumericVector& zout, double dz,
    double dts, double max_depth,
    bool use_fresnel, double lambda_per_v, double max_angle,
    int weight_type, bool spreading, bool normalize,
    bool antialias, double aa_factor,
    int vel_mode, const NumericVector& cum,
    const NumericMatrix& slow, double xv0, double dxv,
    double ray_step, int n_ray_max) {
  
  const int nt  = x.nrow();
  const int nx  = x.ncol();
  const int nxo = xout.size();
  const int nz  = zout.size();
  const double INF = std::numeric_limits<double>::infinity();
  
  const double TWO_PI = 2.0 * std::acos(-1.0);
  
  NumericMatrix out(nz, nxo);
  std::fill(out.begin(), out.end(), NA_REAL);
  
  if (weight_type < 0 || weight_type > 2) {
    stop("'weight_type' must be 0, 1 or 2.");
  }
  
  // ---- Velocity model ---------------------------------------------------
  if (vel_mode != 1 && vel_mode != 2) stop("'vel_mode' must be 1 or 2.");
  const bool general = (vel_mode == 2);
  SlowGrid grid = {NULL, 0, 0, 0.0, 1.0, 0.0, 1.0};
  Layered  lay  = {NULL, 0, 0.0, 1.0};
  if (general) {
    if (slow.nrow() != nz) stop("'slow' must have one row per output row.");
    if (slow.ncol() < 2 || nz < 2) stop("'slow' needs at least 2 rows and 2 columns.");
    if (!(dxv > 0.0) || !(ray_step > 0.0) || n_ray_max < 2)
      stop("'dxv', 'ray_step' must be > 0 and 'n_ray_max' >= 2.");
    grid.s = slow.begin(); grid.nz = nz; grid.nxv = slow.ncol();
    grid.z0 = zout[0];     grid.dz = dz;
    grid.x0 = xv0;         grid.dx = dxv;
  } else {
    if (cum.size() != nz || nz < 2) stop("'cum' must have one value per output row.");
    lay.c = cum.begin(); lay.nz = nz; lay.z0 = zout[0]; lay.dz = dz;
  }
  
  // Slowness at every antenna (surface value): dt/d(antenna position) =
  // slowness * unit ray direction, exactly, by the eikonal equation.
  std::vector<double> s_tx(nx, 0.0), s_rx(nx, 0.0);
  if (general) {
    for (int j = 0; j < nx; ++j) {
      s_tx[j] = grid.at(tx_x[j], tx_z[j]);
      s_rx[j] = grid.at(rx_x[j], rx_z[j]);
    }
  } else {
    for (int j = 0; j < nx; ++j) {
      s_tx[j] = lay.slowness(tx_z[j]);
      s_rx[j] = lay.slowness(rx_z[j]);
    }
  }
  
  // ---- Per-trace pre-computations ---------------------------------------
  // Trace midpoints (sorted, checked in R): used to select candidate traces
  // with a binary search instead of scanning all traces for every pixel.
  std::vector<double> mid(nx);
  double off_max = 0.0;   // largest horizontal distance antenna <-> midpoint
  double z_ant_max = -INF; // highest antenna elevation in the profile
  for (int j = 0; j < nx; ++j) {
    mid[j] = 0.5 * (tx_x[j] + rx_x[j]);
    off_max = std::max(off_max, std::max(std::fabs(tx_x[j] - mid[j]),
                                         std::fabs(rx_x[j] - mid[j])));
    z_ant_max = std::max(z_ant_max, std::max(tx_z[j], rx_z[j]));
  }
  
  // Trace-to-trace step of each antenna (central differences). They give the
  // local travel-time moveout between neighbouring traces for anti-aliasing.
  std::vector<double> stx_x(nx, 0.0), stx_z(nx, 0.0),
  srx_x(nx, 0.0), srx_z(nx, 0.0);
  for (int j = 0; j < nx; ++j) {
    int a = std::max(j - 1, 0), b = std::min(j + 1, nx - 1);
    double n = (double)(b - a);           // 1 at the ends, 2 inside
    stx_x[j] = (tx_x[b] - tx_x[a]) / n;
    stx_z[j] = (tx_z[b] - tx_z[a]) / n;
    srx_x[j] = (rx_x[b] - rx_x[a]) / n;
    srx_z[j] = (rx_z[b] - rx_z[a]) / n;
  }
  
  // Double cumulative sums for the O(1) triangle filter (anti-aliasing).
  // A triangle of half-width L convolved with a trace equals
  //   [C2(u + L) - 2*C2(u) + C2(u - L)] / L^2.
  std::vector<double> c2, S1;
  if (antialias) {
    c2.assign((size_t)nt * nx, 0.0);
    S1.assign(nx, 0.0);
    for (int j = 0; j < nx; ++j) {
      double c1 = 0.0, cc = 0.0;
      for (int i = 0; i < nt; ++i) {
        c1 += x(i, j);
        cc += c1;
        c2[(size_t)j * nt + i] = cc;
      }
      S1[j] = c1;
    }
  }
  
  const double eps = 1e-12;
  const double aa_min = 1.0;      // below this half-width (samples) skip filter
  
  // ---- Loop over output columns -----------------------------------------
  for (int ix = 0; ix < nxo; ++ix) {
    if (!R_finite(surface[ix])) continue;
    if ((ix & 31) == 0)
      Rcpp::checkUserInterrupt();
    const double xo = xout[ix];
    const double surf = surface[ix];
    
    // Range of rows [k0, k1] between the ground surface and max_depth below
    // it. zout[k] = zout[0] - dz * k, so the range follows from arithmetic;
    // the while loops only correct floating-point rounding.
    const double zlo = surf - max_depth;
    int k0 = (int) std::ceil((zout[0] - surf) / dz);
    int k1 = (int) std::floor((zout[0] - zlo) / dz);
    k0 = std::max(k0, 0);
    k1 = std::min(k1, nz - 1);
    while (k0 > 0 && zout[k0 - 1] <= surf) --k0;
    while (k0 < nz && zout[k0] > surf) ++k0;
    while (k1 + 1 < nz && zout[k1 + 1] >= zlo) ++k1;
    while (k1 >= 0 && zout[k1] < zlo) --k1;
    
    for (int k = k0; k <= k1; ++k) {
      const double zk = zout[k];
      
      // ---- Aperture: tan of the limiting angle ---------------------------
      // A trace passes if |dx| <= dz_vertical * tan_lim for both antennas.
      // Fresnel: rf = 0.5*sqrt(2*lambda*d)  =>  tan = rf/d = 0.5*sqrt(2*lambda/d)
      double tan_lim;
      if (use_fresnel) {
        double depth = surf - zk;
        const double v_loc = general ? 1.0 / grid.at(xo, zk)
          : 1.0 / lay.slowness(zk);
        const double lambda = lambda_per_v * v_loc;
        tan_lim = (depth > 0.0) ? 0.5 * std::sqrt(2.0 * lambda / depth) : INF;
      } else {
        tan_lim = (max_angle >= M_PI / 2.0 - 1e-12) ? INF : std::tan(max_angle);
      }
      
      // ---- Candidate trace window via binary search on midpoints ---------
      double zab = z_ant_max - zk;        // largest possible vertical distance
      if (zab <= 0.0) continue;           // pixel above every antenna
      double reach = tan_lim * zab + off_max;   // may be Inf
      int jlo = 0, jhi = nx;
      if (std::isfinite(reach)) {
        jlo = (int)(std::lower_bound(mid.begin(), mid.end(), xo - reach) - mid.begin());
        jhi = (int)(std::upper_bound(mid.begin(), mid.end(), xo + reach) - mid.begin());
      }
      
      double value = 0.0, wsum = 0.0;
      int    count = 0;
      
      for (int j = jlo; j < jhi; ++j) {
        // Antenna-to-pixel vectors (horizontal h, vertical vz = antenna - pixel)
        const double hx = tx_x[j] - xo, hr = rx_x[j] - xo;
        const double vt = tx_z[j] - zk, vr = rx_z[j] - zk;
        
        // Pixel must lie below both antennas
        if (vt <= 0.0 || vr <= 0.0) continue;
        
        // Aperture restriction.
        // Angle from vertical <= limit  <=>  |h| <= vz * tan(limit)
        if (std::fabs(hx) > vt * tan_lim || std::fabs(hr) > vr * tan_lim) continue;
        
        const double dt_ = std::sqrt(hx * hx + vt * vt);   // transmitter-pixel
        const double dr_ = std::sqrt(hr * hr + vr * vr);   // pixel-receiver
        
        // Bistatic travel time in ns (transmitter leg + receiver leg).
        // Variable velocity: line integral of slowness along each straight leg.
        double travel_time;
        if (general) {
          travel_time =
            grid.leg(tx_x[j], tx_z[j], xo, zk, dt_, ray_step, n_ray_max) +
            grid.leg(rx_x[j], rx_z[j], xo, zk, dr_, ray_step, n_ray_max);
        } else {
          // straight ray through horizontal layers: t = L/|dz| * dSigma
          const double s_pix = lay.sigma(zk);
          travel_time = dt_ / vt * (s_pix - lay.sigma(tx_z[j])) +
            dr_ / vr * (s_pix - lay.sigma(rx_z[j]));
        }
        
        // Bistatic two-way time -> fractional zero-based sample coordinate
        const double u = travel_time / dts;
        if (u > nt - 1 + eps) continue;                    // beyond the record
        
        // ---- Amplitude: linear interpolation or anti-aliased ------------
        const double* xc = &x[(size_t)j * nt];
        double amp = 0.0;
        double L = 0.0;                                    // half-width (samples)
        if (antialias) {
          // d(twt)/d(trace index): derivative of both ray travel times with
          // respect to the antenna positions (slowness x ray direction) times
          // the antenna step per trace.
          double dtdj = s_tx[j] * (hx * stx_x[j] + vt * stx_z[j]) / dt_ +
            s_rx[j] * (hr * srx_x[j] + vr * srx_z[j]) / dr_;
          L = aa_factor * std::fabs(dtdj) / dts;
        }
        if (antialias && L >= aa_min) {
          const double* cj = &c2[(size_t)j * nt];
          amp = (c2_eval(cj, nt, S1[j], u + L) - 2.0 * c2_eval(cj, nt, S1[j], u) +
            c2_eval(cj, nt, S1[j], u - L)) / (L * L);
        } else {
          int    i1 = (int) std::floor(u);
          double f  = u - i1;
          amp = xc[i1];
          if (f > eps && i1 + 1 < nt) amp = (1.0 - f) * amp + f * xc[i1 + 1];
        }
        
        // ---- Weights -----------------------------------------------------
        // double w = wq[j];                                  // spatial quadrature
        // if (obliquity) w *= std::sqrt((vt / dt_) * (vr / dr_));  // cos of both legs
        // if (spreading) w *= std::sqrt(dt_ * dr_);          // 2D spreading (1/sqrt(r) per leg)
        
        // Spatial quadrature weight, expressed in metres.
        double w = wq[j];
        
        // Cosines of the transmitter and receiver ray angles relative
        // to the vertical direction.
        const double cos_tx = vt / dt_;
        const double cos_rx = vr / dr_;
        
        // Symmetric bistatic obliquity factor.
        //
        // For zero-offset geometry:
        //   dt_ == dr_
        //   vt  == vr
        //
        // and this reduces to cos(alpha), as in the old RGPR code.
        const double obliquity_weight = std::sqrt(cos_tx * cos_rx);
        
        if (weight_type == 1) {
          
          // Bistatic obliquity weighting only.
          w *= obliquity_weight;
          
        } else if (weight_type == 2) {
          
          // Legacy RGPR weighting:
          //   dx / sqrt(2 * pi * t_x * v) * cos(alpha)
          // Here:
          //   travel_time = (dt_ + dr_) / v
          // and therefore:
          //   travel_time * v = dt_ + dr_
          // where dt_ and dr_ are geometric path lengths in metres.
          const double total_path = dt_ + dr_;
          
          if (total_path > 0.0) {
            w *= obliquity_weight /
              std::sqrt(TWO_PI * total_path);
          } else {
            continue;
          }
        }
        
        // Optional spreading compensation.
        //
        // Do not normally combine this with legacy weighting because the
        // two options have opposing distance dependence.
        if (spreading) {
          w *= std::sqrt(dt_ * dr_);
        }
        
        value += w * amp;
        wsum  += std::fabs(w);
        ++count;
      }
      
      if (count == 0) continue;                            // leave NA
      if (normalize && wsum > 0.0) value /= wsum;
      out(k, ix) = value;
    }
  }
  return out;
}