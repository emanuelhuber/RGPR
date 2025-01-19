# Normal Move-Out correction

```r
NMOcorrect(
  x,
  thrs = NULL,
  v = NULL,
  method = c("linear", "nearest", "pchip", "cubic", "spline")
)

## S4 method for signature 'GPR'
NMOcorrect(
  x,
  thrs = NULL,
  v = NULL,
  method = c("linear", "nearest", "pchip", "cubic", "spline")
)
```

## Arguments

- `x`: An object of the class `GPR`
- `thrs`: (`numeric[1]|NULL`) Definite the threshold for muting (i.e., suppressing) the values where the NMO-stretching is above the threshold. Setting `thrs = NULL`, the full data will be used. `thrs = NULL` ranges between 0 and 1.
- `v`: A length-one numeric vector defining the radar wave velocity in the ground
- `method`: (`character[1]`) Interpolation method to be applied: one of `pchip`, `linear`, `nearest`, `spline`, `cubic`
    
    (see also `signal::interp1()`).

## Returns

An object of the class `GPR` with NMO removed and with antenna separation set equal to zero.

Remove the Normal Move-Out (NMO) from the trace given a velocity. The NMO correction is a non-linear transformation of the time axis to compensate for the offset between transmitter and receiver antennae (antenna separation distance): the time of data acquired with a bi-static antenna system is converted into the time of data virtually acquired with a mono-static system under the assumption of the multi-layer model with constant velocities. Note that only the conventional NMO correction is currently implemented. The conventional NMO introduces a streching effect. A nonstretch NMO will be implemented in a near future. The Normal Move-out is defined as the difference between the two-way time at a given offset and the two-way zero-offset time.

## Details

Assuming a horizontal reflecting plane and homogeneous medium, the two-way bistatic travel time of the reflected wave for an antenna separation `x` follows directly from the Pythagorean theorem: `t_{TWT}(x,z) = \sqrt{\frac{x^2}{v^2} + \frac{4z^2}{v^2}}`

where `t_{TWT}(x)` is the two-way travel time at antenna separation `x` of the wave reflected at depth `z` with propagation velocity `v`. This equation defines an hyperbola (keep `z` constant, increase the antenna separation `x` and you obtain a hyperbola similar to the reflection signals you obtain with common-mid point survey). The idea behind NMO-correction is to correct the signal for the antenna separation (offset) and therefore to transform the signal to the signal we would have recorded with zero offset (`x = 0`). We write the vertical two-way traveltime at zero offset `t_0 = t_{TWT}(x = 0) = \frac{2z}{v}`

Therefore, the NMO-correction `\Delta_{NMO}` is `\Delta_{NMO} = t_{TWT}(x) - t_0`

`\Delta_{NMO} = t_0 (\sqrt{1 + \frac{x^2}{v^2 t_0^2}} - 1)`

## References

 * Tillard and Dubois (1995) Analysis of GPR data: wave propagation velocity determination. Journal of Applied Geophysics, 33:77-91
 * Shatilo and Aminzadeh (2000) Constant normal-moveout (CNMO) correction: a technique and test results. Geophysical Prospecting, 473-488