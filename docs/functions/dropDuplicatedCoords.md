# Remove traces with duplicated trace positions

```r
dropDuplicatedCoords(x, tol = NULL, z = FALSE, verbose = TRUE)

## S4 method for signature 'GPR'
dropDuplicatedCoords(x, tol = NULL, z = FALSE, verbose = TRUE)

## S4 method for signature 'matrix'
dropDuplicatedCoords(x, tol = NULL, z = FALSE, verbose = TRUE)
```

## Arguments

- `x`: An object of the class GPR
- `tol`: Length-one numeric vector: if the horizontal distance between two consecutive traces is smaller than `tol`, then the second trace is removed. If `tol = NULL`, `tol` is set equal to `sqrt(.Machine$double.eps)`.
- `z`: (`logical[1]`) If `TRUE`, the third dimension (z-dimension) will also be accounted for.
- `verbose`: Logical. `TRUE`: a message will be thrown, `FALSE`: no message will be thrown.

Checks for duplicates trace positions (up to precision defined by 'tol') and remove them from 'x' (object of the class GPR or GPRsurvey).