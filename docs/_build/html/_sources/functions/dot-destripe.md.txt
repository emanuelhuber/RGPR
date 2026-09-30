# Hybrid destriping of geophysical raster images

```r
.destripe(
  S,
  stripeDir = "column",
  adaptive = TRUE,
  fft = TRUE,
  fftCutoff = 0.04,
  fftStrength = 0.85,
  sig = 1,
  radius = 2
)
```

## Arguments

- `S`: Numeric matrix representing a gridded image. Stripes are assumed to be aligned along columns by default.
- `stripeDir`: Character string. Direction of stripes: `"column"` (default) or `"row"`.
- `adaptive`: Logical. If `TRUE`, apply adaptive stripe-direction smoothing based on stripe strength.
- `fft`: Logical. If `TRUE`, apply frequency-domain destriping.
- `fftCutoff`: Numeric. Normalized frequency cutoff (0–0.5) for FFT attenuation.
- `fftStrength`: Numeric. Attenuation strength (0–1) applied below the cutoff frequency.
- `sig`: Numeric. Standard deviation of the final Gaussian smoothing kernel.
- `radius`: Integer. Radius of the Gaussian kernel.

## Returns

Numeric matrix of the same dimensions as `S`, with striping artefacts reduced.

## Description

Removes striping artefacts from gridded geophysical data (e.g. GPR, DEMs, magnetic or resistivity grids) using a hybrid spatial–frequency approach.

## Details

The algorithm combines:

 * Adaptive smoothing along the stripe direction
 * Oimoen-style perpendicular high-pass correction
 * Optional FFT-based low-frequency attenuation
 * Final Gaussian regularization

## References

Oimoen, M. (2000). An effective filter for removal of production artifacts in digital elevation models.

Ernenwein, E. G., & Kvamme, K. L. (2008). Data processing issues in large-area GPR surveys.


