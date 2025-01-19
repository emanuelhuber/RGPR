# Time zero correction

```r
shiftToTime0(
  x,
  method = c("pchip", "linear", "nearest", "spline", "cubic", "none"),
  crop = TRUE,
  track = TRUE
)

## S4 method for signature 'GPR'
shiftToTime0(
  x,
  method = c("pchip", "linear", "nearest", "spline", "cubic", "none"),
  crop = TRUE,
  track = TRUE
)
```

## Arguments

- `x`: (`GPR* object`) An object of the class `GPR`
- `method`: (`character[1]`) Interpolation method to be applied (one of `pchip` `linear`, `nearest`, `spline`, `cubic`, `none`, see also `signal::interp1()`). `"none"` means that the trace is shifted by the amount of trace samples the closest to `ts` without interpolation.
- `crop`: (`logical[1]`) If `TRUE` (default), remove the rows containing only zero's (no data).
- `track`: (`logical[1]`) If `TRUE`, processing will be tracked.

## Returns

(`GPR class`) An object of the class `GPR`

`shiftToTime0` shift the traces vertically such that they start at time zero (time zero of the data can be modified with the function). New traces are interpolated.

## Details

This function is a wrapper for the following commands

 * `x \<- traceShift( x, -time0(x), method = method, crop = crop)`
 * `time0(x) \<- 0`

Modified slots

 * `data`: trace shifted. The number of rows of data may be smaller if `crop = TRUE`.
 * `time0`: set to 0.
 * `proc`: updated with function name and arguments.