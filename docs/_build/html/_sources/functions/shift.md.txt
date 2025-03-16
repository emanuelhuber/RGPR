# Shift trace vertically

```r
shift(
  x,
  z,
  method = c("pchip", "linear", "nearest", "spline", "cubic", "none"),
  crop = FALSE,
  track = TRUE
)

## S4 method for signature 'GPR'
shift(
  x,
  z,
  method = c("pchip", "linear", "nearest", "spline", "cubic", "none"),
  crop = FALSE,
  track = TRUE
)

## S4 method for signature 'GPR'
shiftTopo(
  x,
  method = c("pchip", "linear", "nearest", "spline", "cubic", "none"),
  crop = FALSE,
  track = TRUE
)
```

## Arguments

- `x`: (`GPR`) An object of the class `GPR`
- `z`: (`numeric`) Amount of time (or depth, depending on the trace unit) to shift the traces. `z` is eiter a single value (all the traces are shifted by the same amount `z`) or a vector with `m` elements (`m` is equal to the number of traces).
- `method`: (`character[1]`) Interpolation method to be applied: one of `pchip`, `linear`, `nearest`, `spline`, `cubic`, `none`
    
    (see also `signal::interp1()`). `"none"` means that the trace is shifted by the amount of trace samples the closest to `z` without interpolation.
- `crop`: (`logical[1]`) If `TRUE` (default), remove the rows containing only zero's (no data).
- `track`: (`logical[1]`) If `TRUE`, processing will be tracked.

## Returns

(`GPR class`) An object of the class GPR.

Shift traces vertically by an amount of depth (time) units. New traces are interpolated.

## Details

Modified slots

 * `data`: trace shifted. The number of rows of data may be smaller if `crop = TRUE`.
 * `proc`: updated with function name and arguments.