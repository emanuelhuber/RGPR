# Set layer velocities

```r
velSetLayers(obj, v, twt = NULL, method = "pchip", clean = TRUE)

## S4 method for signature 'GPR'
velSetLayers(obj, v, twt = NULL, method = "pchip", clean = TRUE)
```

## Arguments

- `obj`: (`GPR* object`) An object of the class `GPR`
- `v`: (`numeric[n]`) velocities (length `n` equal number of delineations plus one)
- `twt`: (FIXME) Two-way travel time (optinal)
- `method`: (`character[1]`) interpolation method. One of `linear`, `nearest`, `pchip`, `cubic`, `spline`.
- `clean`: (`logical[1]`) When the interface crosses, should the older interfaces be clipped by the new ones?

## Description

Given delineation or the output of the function... and velocity values, set 2D velocity model


