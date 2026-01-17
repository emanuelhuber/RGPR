// iradon_fbp_rcpp.cpp
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins(openmp)]]
// Note: uses Eigen's unsupported FFT module
#include <RcppEigen.h>
#include <unsupported/Eigen/FFT>
#include <cmath>
#include <string>
#include <vector>

// Improved Filtered Backprojection (FBP) with:
// - FFT using Eigen's FFT (unsupported module)
// - selectable filter (ramp, shepp-logan, cosine, hann)
// - linear interpolation along rho during backprojection
// - preserves output size N x M
// - OpenMP parallelization over angles

// Build frequency-domain filter response of length n (for real-input FFT ordering)
static Eigen::VectorXd build_filter(const std::string &filter, int n) {
  Eigen::VectorXd H(n);
  int n2 = n / 2;
  for (int k = 0; k < n; ++k) {
    int kk = (k <= n2) ? k : k - n;            // signed frequency index
    double w = std::abs((double)kk);
    double val = w;
    // normalized frequency for window functions
    double freq = (double)kk / (double)n; // cycles/sample (signed)
    if (filter == "shepp-logan") {
      if (w == 0) val = 0.0; // ramp*sinc -> 0 at DC
      else {
        double x = M_PI * freq;
        double sinc = std::sin(x) / x;
        val = w * sinc;
      }
    } else if (filter == "cosine") {
      double x = M_PI * freq / 2.0;
      val = w * std::cos(x);
    } else if (filter == "hann") {
      // Hann window in freq domain
      double x = M_PI * freq / 2.0;
      double hann = 0.5 * (1.0 + std::cos(x));
      val = w * hann;
    } else { // default: ramp
      val = w;
    }
    H[k] = val;
  }
  // Normalize to max 1 to avoid scaling blow-up
  double mx = H.cwiseAbs().maxCoeff();
  if (mx > 0) H /= mx;
  return H;
}

// [[Rcpp::export]]
Eigen::MatrixXd iradon_transform_fbp_rcpp(
    const Eigen::MatrixXd &R,         // n_rho x n_theta sinogram
    int N, int M,                     // output image size (rows N, cols M)
    double delta_x = 1.0,             // pixel spacing (assumed same in both axes)
    double theta_min = 0.0,
    double theta_max = M_PI,
    double rho_min_in = NAN,
    double delta_rho_in = NAN,
    const std::string &filter = "ramp",
    const std::string &normalization = "none"
) {
  const int n_rho = R.rows();
  const int n_theta = R.cols();
  if (N <= 0 || M <= 0) return Eigen::MatrixXd(0,0);
  
  // If rho grid not provided, assume diagonal covers image
  double diag = std::sqrt((double)(N*N + M*M)) * delta_x;
  double rho_min = rho_min_in;
  double delta_rho = delta_rho_in;
  if (!std::isfinite(delta_rho) || delta_rho <= 0.0) {
    if (!std::isfinite(rho_min)) rho_min = -diag / 2.0;
    delta_rho = (2.0 * diag) / std::max(1, n_rho - 1);
  }
  
  const double delta_theta = (theta_max - theta_min) / std::max(1, n_theta - 1);
  
  // Output image
  Eigen::MatrixXd A = Eigen::MatrixXd::Zero(N, M);
  
  // Pixel centers (centered coordinates)
  std::vector<double> xs(M), ys(N);
  for (int j = 0; j < M; ++j) xs[j] = (j - (M-1)/2.0) * delta_x;
  for (int i = 0; i < N; ++i) ys[i] = (i - (N-1)/2.0) * delta_x;
  
  // Build filter response and FFT helper
  Eigen::VectorXd H = build_filter(filter, n_rho);
  Eigen::FFT<double> fft;
  
  // Parallel over angles (each angle independent)
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for (int t = 0; t < n_theta; ++t) {
    // extract projection (length n_rho)
    Eigen::VectorXd proj = R.col(t);
    
    // forward FFT (real -> complex)
    Eigen::VectorXcd Pc;
    fft.fwd(Pc, proj);
    
    // apply filter in frequency domain
    for (int k = 0; k < n_rho; ++k) Pc[k] *= H[k];
    
    // inverse FFT (complex -> real)
    Eigen::VectorXd proj_filt;
    fft.inv(proj_filt, Pc);
    
    double theta = theta_min + t * delta_theta;
    double cos_t = std::cos(theta);
    double sin_t = std::sin(theta);
    
    // backproject with linear interpolation along rho
    for (int i = 0; i < N; ++i) {
      double y = ys[i];
      for (int j = 0; j < M; ++j) {
        double x = xs[j];
        double rho = x * cos_t + y * sin_t;
        double f = (rho - rho_min) / delta_rho;
        int r0 = (int)std::floor(f);
        double w = f - r0;
        double val = 0.0;
        if (r0 >= 0 && r0 + 1 < n_rho) {
          val = (1.0 - w) * proj_filt[r0] + w * proj_filt[r0 + 1];
        } else if (r0 >= 0 && r0 < n_rho) {
          val = proj_filt[r0];
        }
        // Atomic add because multiple threads update A
#ifdef _OPENMP
#pragma omp atomic
#endif
        A(i,j) += val;
      }
    }
  } // end angle loop
  
  // final normalization choices:
  // - "none": divide by n_theta (simple average)
  // - "scikit": multiply by pi/(2 * n_theta) (used by scikit-image's iradon)
  // - "delta": account for sampling spacing: multiply by delta_rho * delta_theta
  Eigen::MatrixXd out = A;
  if (normalization == "none") {
    out /= (double) n_theta;
  } else if (normalization == "scikit") {
    out *= (M_PI / (2.0 * (double) n_theta));
  } else if (normalization == "delta") {
    out *= delta_rho * delta_theta;
  } else {
    out /= (double) n_theta;
  }
  return out;
}
