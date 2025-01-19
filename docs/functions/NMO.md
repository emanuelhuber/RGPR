# Normal Move-Out

```r
NMO(x, v = NULL)

## S4 method for signature 'GPR'
NMO(x, v = NULL)
```

## Arguments

- `x`: An object of the class `GPR`
- `v`: A length-one numeric vector defining the radar wave velocity in the ground

Compute the Normal Move-Out (NMO) for a data set given a constant velocity (?? FIXME): The Normal Move-out is defined as the difference between the two-way time at a given offset and the two-way zero-offset time. We write the vertical two-way traveltime at zero offset `t_0 = t_{TWT}(x = 0) = \frac{2z}{v}`

Therefore, the NMO-correction `\Delta_{NMO}` is `\Delta_{NMO} = t_{TWT}(x) - t_0`

`\Delta_{NMO} = t_0 (\sqrt{1 + \frac{x^2}{v^2 t_0^2}} - 1)`