# Set velocities

```r
setVel(x, v, twt, type = c("vrms", "vint"))

## S4 method for signature 'GPR'
setVel(x, v, twt, type = c("vrms", "vint"))
```

## Arguments

- `x`: (`GPR`) GPR object
- `v`: (`numeric[n]`) `n` velocity values
- `twt`: (`numeric[n]`) `n` two-way travel time values associated with the velocities `v`. Optional.
- `type`: (`vrms|vint`) Kind of velocities `v`: root-mean-square velocities `"vrms"` (genrally inferred from CMP data) or internal velocities `"vint"` (layer velocities).

## Returns

(`GPR`)

set velocities.