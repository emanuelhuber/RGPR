# Two-dimensional filters

```r
filter2D(obj, type = c("median3x3", "adimpro", "gaussian"), ..., track = TRUE)

## S4 method for signature 'GPRvirtual'
filter2D(obj, type = c("median3x3", "adimpro", "gaussian"), ..., track = TRUE)

## S4 method for signature 'GPRslice'
filter2D(obj, type = c("median3x3", "adimpro", "gaussian"), ..., track = TRUE)

## S4 method for signature 'GPRcube'
filter2D(obj, type = c("median3x3", "adimpro", "gaussian"), ..., track = TRUE)

filter2Dmedian3x3(obj, ..., track = TRUE)

## S4 method for signature 'GPRvirtual'
filter2Dmedian3x3(obj, ..., track = TRUE)

filter2Dadimpro(obj, ..., track = TRUE)

## S4 method for signature 'GPRvirtual'
filter2Dadimpro(obj, ..., track = TRUE)

filter2Dgaussian(obj, ..., track = TRUE)

## S4 method for signature 'GPRvirtual'
filter2Dgaussian(obj, ..., track = TRUE)

filter2Disoblur(obj, sigma = 1, ..., track = TRUE)

## S4 method for signature 'GPRvirtual'
filter2Disoblur(obj, sigma = 1, ..., track = TRUE)

filter2Dmedianblur(obj, n = 3, threshold = 0, ..., track = TRUE)

## S4 method for signature 'GPRvirtual'
filter2Dmedianblur(obj, n = 3, threshold = 0, ..., track = TRUE)

filter2Danisotropic(
  obj,
  amplitude = 1,
  sharpness = 0.7,
  anisotropy = 0.6,
  alpha = 0.6,
  sigma = 1.1,
  dl = 0.8,
  da = 30,
  ...,
  track = TRUE
)

## S4 method for signature 'GPRvirtual'
filter2Danisotropic(
  obj,
  amplitude = 1,
  sharpness = 0.7,
  anisotropy = 0.6,
  alpha = 0.6,
  sigma = 1.1,
  dl = 0.8,
  da = 30,
  ...,
  track = TRUE
)

filter2Ddiffusiontensors(
  obj,
  sharpness = 0.7,
  anisotropy = 0.6,
  alpha = 0.6,
  sigma = 1.1,
  ...,
  track = TRUE
)

## S4 method for signature 'GPRvirtual'
filter2Ddiffusiontensors(
  obj,
  sharpness = 0.7,
  anisotropy = 0.6,
  alpha = 0.6,
  sigma = 1.1,
  ...,
  track = TRUE
)

filter2Dgradient(obj, type = c("xy", "x", "y"), ..., track = TRUE)

## S4 method for signature 'GPRvirtual'
filter2Dgradient(obj, type = c("xy", "x", "y"), ..., track = TRUE)

filter2Dfftlowpass(obj, cutoff = 0.04, strength = 0.85, ..., track = TRUE)

## S4 method for signature 'GPRvirtual'
filter2Dfftlowpass(obj, cutoff = 0.04, strength = 0.85, ..., track = TRUE)

filter2Dblur_anisotropic(
  obj,
  amplitude = 1,
  sharpness = 0.7,
  anisotropy = 0.6,
  alpha = 0.6,
  sigma = 1.1,
  dl = 0.8,
  da = 30,
  ...,
  track = TRUE
)

filter2DcannyEdges(obj, sigma = 1, alpha = 0.5, ..., track = TRUE)

filter2Dimhessian(obj, sigma = 1, ..., track = TRUE)

filter2Dimlap(obj, ..., track = TRUE)

filter2Dimsharpen(
  obj,
  amplitude = 1,
  type = "diffusion",
  edge = 1,
  alpha = 0,
  sigma = 0,
  ...,
  track = TRUE
)

filter2DlocalContrast(
  obj,
  win = 3,
  alpha = 0.1,
  epsilon = sqrt(.Machine$double.eps),
  ...,
  track = TRUE
)

filter2DsmoothSparse(obj, scale = 0.05, power = 1.5, ..., track = TRUE)
```

## Arguments

- `obj`: (`GPR* object`)
- `type`: (`character[1]`) Filter method.
- `...`: Additional arguments passed to `imager`::imsharpen.
- `track`: (`logical(1)`), default TRUE.
- `sigma`: numeric, default 1. Gaussian smoothing before computing Hessian.
- `n`: odd integer kernel size (default 3)
- `amplitude`: numeric, default 1. Controls intensity of blur.
- `sharpness`: numeric, default 0.7. Controls edge sharpness preservation.
- `anisotropy`: numeric, default 0.6. Controls anisotropy of smoothing.
- `alpha`: (`numeric[1]`) A non-negative numeric scalar defining the minimum local standard deviation as a fraction of the global standard deviation of `image`. For example, `alpha = 0.1` prevents the local standard deviation from falling below 10 percent of the global standard deviation. This limits noise amplification in locally homogeneous areas. Set to `0` to disable the relative standard-deviation floor. The default is `0.1`.
- `dl`: numeric, default 0.8. Step size along linear diffusion.
- `da`: numeric, default 30. Angular resolution for anisotropic blur.
- `cutoff`: numeric normalized frequency cutoff (0-0.5)
- `strength`: numeric attenuation factor (0-1)
- `win`: (`integer[1]`) A positive odd integer greater than or equal to `3` specifying the number of rows and columns in the square sliding window. Larger values enhance features relative to a broader spatial neighbourhood. The default is `3L`.
- `epsilon`: (`numeric[1]`)A positive numeric scalar defining the absolute lower bound applied to the denominator. It provides numerical stability when the global and local standard deviations are zero or nearly zero. The default is `sqrt(.Machine$double.eps)`.
- `scale`: Numeric value controlling soft threshold (default 0.05).
- `power`: Numeric exponent for amplifying high values (default 1.5).
- `amount`: numeric, default 1. Strength of sharpening.
- `image_matrix`: Numeric matrix representing the GPR depth-slice.

## Returns

Object of same class as `obj` with updated `@data`.

Object of same class as `obj` with updated `@data`.

Object of same class as `obj` with processed data.

Object of same class as `obj` with processed data.

Object of same class as `obj` with processed data.

GPR object

Numeric matrix of same size as image_matrix, normalized to `[0,1]`.

## Description

Two-dimensional filters

A collection of 2-D processing methods that operate on objects inheriting from `GPRvirtual` (including `GPR`, `GPRslice`, `GPRcube`).

Apply a 3x3 median filter to `obj@data`.

Wrapper around `adimpro` anisotropic smoothing (awsaniso).

Apply a 2-D Gaussian smoothing using `mmand` (or fallback to imager).

This function performs a column-wise FFT attenuation of low-frequency components (normalized frequency cutoff controlled with `cutoff`).

Apply an edge-preserving anisotropic blur to the GPR data. Uses `imager`::blur_anisotropic internally.

Applies the Canny edge detection algorithm to 2D GPR data. Uses `imager`::cannyEdges internally.

Enhances ridges and edges using Hessian-based filtering. Uses `imager`::imhessian.

Enhances edges by computing the Laplacian of the image. Uses `imager`::imlap.

Sharpens the GPR data using `imager`::imsharpen.

The matrix is processed using a square sliding window. For each finite cell `x_{ij}`, the locally standardized value is calculated as

c("`\n`", "`z_{ij} =\n`", "`\\frac{x_{ij} - \\mu_{ij}}\n`", "`{\\max(\\sigma_{ij}, \\sigma_{\\mathrm{min}})}\n`")

where `\mu_{ij}` and `\sigma_{ij}` are the mean and standard deviation within the local window. The lower bound `\sigma_{\mathrm{min}}` is defined as `alpha` times the global standard deviation of the matrix, subject to the absolute lower bound given by `epsilon`.

The standard-deviation floor prevents excessive amplification of small variations in locally homogeneous areas. This transformation produces signed local standardized values and does not perform histogram equalization.

Matrix edges are handled by replicating the nearest edge values. Non-finite values are excluded from the local statistics, and non-finite cells in the original matrix remain missing in the output.

Applies a smooth nonlinear transformation to increase sparsity in a GPR depth-slice. Small values near zero are compressed while peaks and valleys are amplified.

## Details

Each method has the form `filter2D<name>()` and an S4 method for the `GPRvirtual` parent class that applies the operation to `obj@data`. For `GPRcube` objects the operation is applied slice-by-slice.

The functions include classic filters (median3x3, gaussian) and wrappers for common `imager` processing functions (isoblur, medianblur, anisotropic, diffusion_tensors, imgradient) plus a small FFT-based low-pass destriper.

The S4 methods implemented here rely on an internal helper `.filter2D_apply(obj, FUN, ...)` that dispatches to the appropriate behaviour depending on whether `obj` is a `GPR`, `GPRslice`, or `GPRcube`. For `GPRcube` each slice `obj@data[,,i]` is processed independently.

Many functions rely on the `imager` package. If `imager` is not installed the corresponding functions will throw an informative error telling the user to install `imager`.

Enhances the local contrast of a numeric matrix by centring each cell on the mean of its local neighbourhood and scaling it by the corresponding local standard deviation.

## Examples

```r
## Not run:

gpr2 <- filter2Dblur_anisotropic(gpr_obj, amplitude = 0.9, sigma = 1.2)
## End(Not run)

## Not run:

gpr2 <- filter2DcannyEdges(gpr_obj, sigma = 1, alpha = 0.5)
## End(Not run)

## Not run:

gpr2 <- filter2Dimhessian(gpr_obj, sigma = 1)
## End(Not run)

## Not run:

gpr2 <- filter2Dimlap(gpr_obj)
## End(Not run)

## Not run:

gpr2 <- filter2Dimsharpen(gpr_obj, amount = 1.5)
## End(Not run)

set.seed(1)
img <- matrix(runif(100), nrow = 10)
enhanced <- local_contrast_enhancement(img, window_size = 5)
print(enhanced)

set.seed(1)
gpr_slice <- matrix(rnorm(100, 0, 0.05), nrow = 10)
enhanced_slice <- smooth_sparse_gpr_image(gpr_slice, scale = 0.05, power = 2)
print(enhanced_slice)
```


