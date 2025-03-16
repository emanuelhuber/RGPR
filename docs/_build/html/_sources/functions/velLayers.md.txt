# Set layer velocities

```r
velLayers(x, v, twt = NULL, method = "pchip", clean = TRUE)

## S4 method for signature 'GPR'
velLayers(x, v, twt = NULL, method = "pchip", clean = TRUE)
```

## Arguments

- `x`: GPR object
- `v`: ‘numeric’ velocities (length equal number of delineations plus one)
- `twt`: vertical resolution of the migrated data
- `method`: ‘character(1)’ interpolation method. One of `linear`, `nearest`, `pchip`, `cubic`, `spline`.

Given delineation or the output of the function... and velocity values, set 2D velocity model