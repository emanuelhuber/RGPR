# Appla Normal Move-Out (NMO) correction and stack the traces

```r
NMOstack(
  x,
  thrs = NULL,
  v = NULL,
  method = c("linear", "nearest", "pchip", "cubic", "spline")
)

## S4 method for signature 'GPR'
NMOstack(
  x,
  thrs = NULL,
  v = NULL,
  method = c("linear", "nearest", "pchip", "cubic", "spline")
)
```

## Arguments

- `x`: An object of the class `GPR`
- `thrs`: (`numeric[1]|NULL`) Definite the threshold for muting (i.e., suppressing) the values where the NMO-stretching is above the threshold. Setting `thrs = NULL`, the full data will be used.
- `v`: A length-one numeric vector defining the radar wave velocity in the ground
- `method`: (`character[1]`) Interpolation method to be applied: one of `pchip`, `linear`, `nearest`, `spline`, `cubic`
    
    (see also `signal::interp1()`).

Removes the Normal Move-Out (NMO) from the trace given a velocity and stacks (sums) the traces.