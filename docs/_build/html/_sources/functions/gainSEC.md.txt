# Spreading and Exponential Compensation (SEC) gain

```r
## S4 method for signature 'GPR'
gainSEC(
  x,
  a = 0.01,
  b = 1,
  t0 = NULL,
  tend = NULL,
  tcst = NULL,
  return_gain = FALSE,
  track = TRUE
)
```

## Arguments

- `x`: `GPR` An object of the class GPR.
- `a`: `numeric[1]` Parameter of the exponential filter (`a` `\geq` 0).
- `b`: `numeric(1)` Parameter of the power filter (`b` `\geq` 0). Usually, `b = 1`.
- `t0`: `numeric` Start time of the gain filter (if `t0 = NULL`, `t0` is set equal to `time0(x)`).
- `tend`: `numeric[1]` End time of the gain filter (optional)
- `tcst`: `numeric[1]` Constant time: the gain before `tcst` is set equal to the gain value at `tcst`.

## Returns

`GPR class` An object of the class GPR.

`gainSEC` Applies a combination of a power and exponential time gain to compensate for the signal attenuation through spherical spreading losses and exponential ohmic dissipation of energy with depth. Usually, the power in the power gain is set to zero.

## Details

Spreading and Exponential Compensation (SEC) gain can be written as `\exp(a \cdot t) \cdot t^b`, where `t^b` is the power gain (set `b = 1` to get a linear gain) and `\exp(a \cdot t)` is the exponential gain.

Modified slots `data`: trace gained. `proc`: updated with function name and arguments.

## See Also

`gainAGC()`