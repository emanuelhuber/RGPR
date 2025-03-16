# Delineate structure on GPR data

```r
delineate(x, name = NULL, values = NULL, n = 10000, plotDel = NULL, ...)

## S4 method for signature 'GPR'
delineate(x, name = NULL, values = NULL, n = 10000, plotDel = NULL, ...)

plotDelineations(
  x,
  method = c("linear", "nearest", "pchip", "cubic", "spline", "none"),
  col = NULL,
  ...
)

## S4 method for signature 'GPR'
plotDelineations(
  x,
  method = c("linear", "nearest", "pchip", "cubic", "spline", "none"),
  col = NULL,
  ...
)
```

## Arguments

- `x`: (`GPR`)
- `name`: (`character[1]`) Names of the delineated line
- `values`: (`list`) list of x and y coordinates (optional). If `NULL`, `locator()` function is run.
- `n`: (`numeric[1]`) the maximum number of points to locate. Valid values start at 1.
- `plotDel`: (`logical[1]`) If `TRUE`plot delineation.

Delineate structure on GPR data

Plot the delineation on a 2D plot