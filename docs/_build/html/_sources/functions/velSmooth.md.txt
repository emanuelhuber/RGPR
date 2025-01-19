# Smooth velocity model

```r
velSmooth(x, type = c("vrms", "vint"), w)

## S4 method for signature 'GPR'
velSmooth(x, type = c("vrms", "vint"), w)
```

## Arguments

- `x`: (`GPR class`) An object of the class `GPR`
- `type`: (`character[1]`) Which type of velocity values has to be updated? The root-mean-square velocity (`vrms`) or the internal velocity (`vint`)?
- `w`: (`numeric[1]|NULL`) Standard deviation of the standard deviation of the smoothing kernel. If `w = NULL`, no smoothing will be applied.

## Returns

(`GPR class`) An object of the class GPR.

Define the smoothing parameters that will be used when the velocities will be plotted or used in other functions. To undo smoothing, set `w = NULL`.