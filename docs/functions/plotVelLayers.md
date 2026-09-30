# Plot the velocity layers on a 2D plot

```r
plotVelLayers(
  obj,
  method = c("linear", "nearest", "pchip", "cubic", "spline", "none"),
  col = NULL,
  ...
)

## S4 method for signature 'GPR'
plotVelLayers(
  obj,
  method = c("linear", "nearest", "pchip", "cubic", "spline", "none"),
  col = NULL,
  ...
)
```

## Arguments

- `obj`: (`GPR* object`) An object of the class `GPR`
- `method`: (`character[1]`) Interpolation method.
- `col`: (`character[1|n]`) Color.
- `...`: Additional arguments for the function `plotDelineations()`

## Description

Plot the velocity layers on a 2D plot


