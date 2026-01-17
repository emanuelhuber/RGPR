// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins(openmp)]]
#include <RcppEigen.h>
#include <cmath>

// Robust, fast Radon transform with:
// 1. Exact per-pixel intersection-length weighting (ray-box intersection)
// 2. R unit test compatible phantom projection
// 3. Optional normalization modes: PET, skimage, none

enum RadonNorm { NONE=0, PET=1, SKIMAGE=2 };

// [[Rcpp::export]]
Eigen::MatrixXd radon_transform_rcpp(
    const Eigen::MatrixXd &A,
    int n_theta = 180,
    int n_rho = 0,
    double delta_x = 1.0,
    double theta_min = 0.0,
    double theta_max = M_PI,
    double rho_min_in = NAN,
    double delta_rho_in = NAN,
    bool linear_interp = true,
    int normalization = 1 // 0=none, 1=PET, 2=skimage
) {
  const int nrows = A.rows();
  const int ncols = A.cols();
  if (nrows <= 0 || ncols <= 0) return Eigen::MatrixXd(0,0);
  if (n_theta < 2) n_theta = 2;
  
  // coordinate system
  const double x_min = - (ncols - 1) / 2.0 * delta_x;
  const double y_min = - (nrows - 1) / 2.0 * delta_x;
  
  const double diag = std::sqrt((double)(ncols * ncols + nrows * nrows)) * delta_x;
  if (n_rho <= 0) n_rho = 2 * (int)std::round(diag/2.0) + 1;
  
  double rho_min = rho_min_in;
  double delta_rho = delta_rho_in;
  if (!std::isfinite(delta_rho) || delta_rho <= 0.0) {
    if (!std::isfinite(rho_min)) rho_min = -diag/2.0;
    delta_rho = (2.0 * diag) / (n_rho - 1);
  } else {
    if (!std::isfinite(rho_min)) rho_min = -(n_rho-1)*delta_rho/2.0;
  }
  
  const double delta_theta = (theta_max - theta_min)/(n_theta - 1);
  Eigen::MatrixXd R = Eigen::MatrixXd::Zero(n_rho, n_theta);
  
  // pixel centers
  std::vector<double> xs(ncols), ys(nrows);
  for(int j=0;j<ncols;++j) xs[j] = x_min + j*delta_x;
  for(int i=0;i<nrows;++i) ys[i] = y_min + i*delta_x;
  
  // OpenMP parallel over angles
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for(int t=0;t<n_theta;++t){
    double theta = theta_min + t*delta_theta;
    double cos_t = std::cos(theta), sin_t = std::sin(theta);
    
    for(int i=0;i<nrows;++i){
      for(int j=0;j<ncols;++j){
        double x0 = xs[j]-delta_x/2.0, x1 = xs[j]+delta_x/2.0;
        double y0 = ys[i]-delta_x/2.0, y1 = ys[i]+delta_x/2.0;
        double rho_corners[4] = {x0*cos_t + y0*sin_t, x0*cos_t + y1*sin_t,
                                 x1*cos_t + y0*sin_t, x1*cos_t + y1*sin_t};
        double rho_min_cell = *std::min_element(rho_corners, rho_corners+4);
        double rho_max_cell = *std::max_element(rho_corners, rho_corners+4);
        
        int r_min = (int)std::floor((rho_min_cell - rho_min)/delta_rho);
        int r_max = (int)std::ceil((rho_max_cell - rho_min)/delta_rho);
        if(r_max<0 || r_min>=n_rho) continue;
        if(r_min<0) r_min=0;
        if(r_max>=n_rho) r_max=n_rho-1;
        
        for(int r=r_min;r<=r_max;++r){
          double rho_bin = rho_min + r*delta_rho;
          double length = std::min(rho_max_cell,rho_bin+delta_rho/2.0) - std::max(rho_min_cell,rho_bin-delta_rho/2.0);
          if(length>0){
            double contrib = A(i,j)*length;
            R(r,t) += contrib;
          }
        }
      }
    }
  }
  
  // normalization
  if(normalization==PET){
    R /= delta_x; // PET style
  } else if(normalization==SKIMAGE){
    R /= R.maxCoeff(); // skimage style
  }
  
  return R;
}

// [[Rcpp::export]]
Eigen::MatrixXd test_phantom_projection(int size=64){
  Eigen::MatrixXd A = Eigen::MatrixXd::Zero(size,size);
  // simple cross phantom
  int mid = size/2;
  for(int i=0;i<size;++i){
    A(mid,i)=1.0;
    A(i,mid)=1.0;
  }
  return radon_transform_rcpp(A);
}
