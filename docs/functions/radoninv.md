# Filtered Backprojection (FBP) for PET / CT

```r
radoninv(
  sinogram,
  N,
  M,
  delta_x = 1,
  theta_min = 0,
  theta_max = pi,
  rho_min = NA,
  delta_rho = NA,
  filter = c("ramp", "shepp-logan", "cosine", "hann"),
  normalization = c("delta", "scikit", "none")
)
```

## Arguments

- `sinogram`: Numeric matrix of size n_rho x n_theta representing the sinogram.
- `N`: Integer. Number of rows in the reconstructed image.
- `M`: Integer. Number of columns in the reconstructed image.
- `delta_x`: Pixel spacing in x and y directions. Default is 1.
- `theta_min`: Minimum projection angle in radians. Default is 0.
- `theta_max`: Maximum projection angle in radians. Default is pi.
- `rho_min`: Minimum rho value (detector coordinate). Default is NA, automatically set.
- `delta_rho`: Spacing between rho bins. Default is NA, automatically set.
- `filter`: Character. Filter type: "ramp", "shepp-logan", "cosine", "hann". Default is "ramp".
- `normalization`: Character. Normalization scheme: "delta" (PET), "scikit" (scikit-image/MATLAB), "none" (average). Default is "delta".

## Returns

Numeric matrix of size N x M representing the reconstructed image.

## Description

This function reconstructs an image from a sinogram using filtered backprojection (FBP) via a high-performance C++ implementation. Supports multiple filter types and normalization conventions (PET vs scikit-image / MATLAB style).

## Examples

```r
# Small numeric example
sinogram <- matrix(1:9, nrow = 3, ncol = 3)
img_pet <- iradon_fbp_wrapper(sinogram, N = 3, M = 3, normalization = "delta")
img_sci <- iradon_fbp_wrapper(sinogram, N = 3, M = 3, normalization = "scikit")
img_pet
img_sci
```


