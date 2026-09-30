# Time to depth conversion

```r
convertTimeToDepth(
  obj,
  dz = NULL,
  zmax = NULL,
  method = c("pchip", "linear", "nearest", "spline", "cubic")
)

## S4 method for signature 'GPR'
convertTimeToDepth(
  obj,
  dz = NULL,
  zmax = NULL,
  method = c("pchip", "linear", "nearest", "spline", "cubic")
)
```

## Arguments

- `obj`: (`GPR* object`) An object of the class `GPR`
- `dz`: (`numeric[1]`) Desired depth resolution. If `dz = NULL`, then `dz` is set equal to the smallest depth resolution inferred from the data.
- `zmax`: (`numeric[1]`) Maximum Desired depth. If `zmax = NULL`, then `zmax` is set equal to the largest depth inferred from the data.
- `method`: (`character[1]`) Interpolation method to be applied: one of `pchip`, `linear`, `nearest`, `spline`, or `cubic` (see also `signal::interp1()`).

## Returns

(`GPR* object`) with signal as a function of depth.

## Description

Convert the two-way travel time of the recorded waves into depth. It does not account for the topography.


