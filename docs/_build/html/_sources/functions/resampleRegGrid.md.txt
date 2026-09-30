# Resample a GPR profile to a regular grid

```r
resampleRegGrid(
  obj,
  dx = NULL,
  dz = NULL,
  method = c("linear", "nearest", "pchip", "cubic", "spline"),
  track = TRUE
)

## S4 method for signature 'GPR'
resampleRegGrid(
  obj,
  dx = NULL,
  dz = NULL,
  method = c("linear", "nearest", "pchip", "cubic", "spline"),
  track = TRUE
)
```

## Arguments

- `obj`: A `GPR` object.
- `dx`: Numeric. Desired trace spacing in the horizontal direction. If `NULL`, the average spacing of `obj@x` is used. If `FALSE`, no resampling is performed along the horizontal axis.
- `dz`: Numeric. Desired sample spacing in the vertical direction. If `NULL`, the average spacing of `obj@z` is used. If `FALSE`, no resampling is performed along the vertical axis.
- `method`: Character string specifying the interpolation method passed to `interp1`. One of:
    
    - **`"linear"`**: Linear interpolation.
    - **`"nearest"`**: Nearest-neighbour interpolation.
    - **`"pchip"`**: Shape-preserving cubic interpolation.
    - **`"cubic"`**: Cubic interpolation.
    - **`"spline"`**: Cubic spline interpolation.
- `track`: Logical. If `TRUE`, the processing step is added to the processing history stored in the object.

## Returns

A `GPR` object resampled onto a regular grid.

## Description

Resamples a `GPR` object onto a regular spatial (`x`) and/or temporal/depth (`z`) grid using interpolation.

## Details

This function is useful for correcting irregular trace spacing, standardizing sampling intervals, and preparing data for imaging, migration, filtering, or comparison between profiles.

The radargram amplitudes are interpolated independently along the horizontal (`x`) and vertical (`z`) dimensions.

When resampling along the profile direction (`dx`), the function also interpolates associated trace attributes where available:

 * trace positions (`@x`),
 * first-break positions (`@z0`),
 * acquisition times (`@time`),
 * antenna separations (`@antsep`),
 * marker information (`@markers`),
 * annotations (`@ann`),
 * spatial coordinates (`@coord`),
 * antenna orientation angles (`@angles`).

Spatial coordinates are resampled along cumulative profile distance using `pathRelPos`. This preserves the geometry of curved survey lines.

Resampling of receiver coordinates (`@rec`) and transmitter coordinates (`@trans`) is currently not implemented and will generate an error if present.

## Examples

```r
## Not run:

data(frenkeLine00)

# Resample to a regular trace spacing of 0.05 m
x <- resampleRegGrid(frenkeLine00, dx = 0.05)

# Resample both horizontal and vertical axes
x <- resampleRegGrid(frenkeLine00,
dx = 0.05,
dz = 0.2)

# Use shape-preserving interpolation
x <- resampleRegGrid(frenkeLine00,
method = "pchip")
## End(Not run)
```


