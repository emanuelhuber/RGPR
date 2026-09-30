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

delineations(obj, name = NULL)

## S4 method for signature 'GPR'
delineations(obj, name = NULL)

rmDelineations(x) <- value

## S4 replacement method for signature 'GPR'
rmDelineations(x) <- value

identifyDelineations(
  obj,
  method = c("linear", "nearest", "pchip", "cubic", "spline", "none"),
  ...
)

## S4 method for signature 'GPR'
identifyDelineations(
  obj,
  method = c("linear", "nearest", "pchip", "cubic", "spline", "none"),
  ...
)

exportDelineations(obj, dsn = ".")

## S4 method for signature 'GPR'
exportDelineations(obj, dsn = ".")
```

## Arguments

- `x`: (`GPR`)
- `name`: (`character[n]`) Names of the delineations.
- `values`: (`list`) list of x and y coordinates (optional). If `NULL`, `locator()` function is run.
- `n`: (`numeric[1]`) the maximum number of points to locate. Valid values start at 1.
- `plotDel`: (`logical[1]`) If `TRUE`plot delineation.
- `...`: Additional parameters (not yet used)
- `method`: (`character[1]`) Interpolation method.
- `col`: (`character[n]`) `n` colors for the `n` delineations
- `obj`: (`GPR* object`) An object of the class `GPR`
- `value`: (`integer[n]|all`) Either a vector of indexes of delineations to to remove or `all` to remove all delineations,
- `dsn`: (`character(1)|connection object`) data source name: either the filepath to the GPR data (character), or an open file connection.

## Description

Print and invisible returns the GPR data delineations.

Works only close to the points !!!


