# Normal Move-Out streching

```r
NMOstreching(x, v = NULL)

## S4 method for signature 'GPR'
NMOstreching(x, v = NULL)
```

## Arguments

- `x`: An object of the class `GPR`
- `v`: A length-one numeric vector defining the radar wave velocity in the ground

Compute the Normal Move-Out (NMO) streching for a data set given a constant velocity. The NMO streching is defined by `S_{NMO} = \frac{\Delta_{NMO}}{t_0}`, where `t_0 = t_{TWT}(x = 0) = \frac{2z}{v}` is the vertical two-way traveltime at zero offset.