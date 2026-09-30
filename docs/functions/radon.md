# Compute the Radon Transform of a 2D Image

```r
radon(
  image_matrix,
  n_theta = 180,
  n_rho = 0,
  delta_x = 1,
  theta_min = 0,
  theta_max = pi,
  linear_interp = TRUE,
  normalization = c("PET", "none", "skimage")
)
```

## Arguments

- `image_matrix`: Numeric matrix representing the image to be transformed.
- `n_theta`: Integer, number of projection angles (default 180).
- `n_rho`: Integer, number of distance bins (rho) in the sinogram (default 0, auto).
- `delta_x`: Numeric, spacing of pixels in the image coordinate system (default 1.0).
- `theta_min`: Numeric, minimum angle in radians (default 0).
- `theta_max`: Numeric, maximum angle in radians (default pi).
- `linear_interp`: Logical, if TRUE use linear interpolation along rho (default TRUE).
- `normalization`: Character, type of normalization: 'none', 'PET', or 'skimage' (default 'PET').

## Returns

Numeric matrix: the Radon transform (sinogram) with rows corresponding to rho bins and columns to angles.

## Description

This function is an R wrapper for the C++ implementation `radon_transform_rcpp`. It computes the discrete Radon transform (sinogram) of a given 2D numeric image.

## Examples

```r
# Generate a simple test image
n <- 64
img <- matrix(0, n, n)
mid <- n/2
img[mid, ] <- 1
img[, mid] <- 1

# Compute sinogram
sino <- radon_wrapper(img, n_theta = 180, n_rho = 90)
image(t(sino[nrow(sino):1, ]), col = gray.colors(256), main = 'Sinogram')
```


