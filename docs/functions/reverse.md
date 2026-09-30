# Reverse the trace position.

```r
reverse(x, id = NULL, tol = 0.3, onlyData = FALSE, track = TRUE)

## S4 method for signature 'GPR'
reverse(x, id = NULL, tol = 0.3, onlyData = FALSE, track = TRUE)

## S4 method for signature 'GPRsurvey'
reverse(x, id = NULL, tol = 0.3, onlyData = FALSE, track = TRUE)
```

## Arguments

- `x`: (`GPR`|`GPRsurvey`) Object of the class `GPR` or `GPRsurvey`
- `id`: (`NULL`|`integer[n]`|`zigzag`) Wokrs only if `x` is an object of the class `GPRsurvey`. Either the index of the GPR data to reverse (e.g., `id = c(1, 3, 4, 7)`) or `id = "zigzag` to reverse every second GPR data. If `id = NULL` and `x` has coordinates, `reverse()` will cluster the GPR data according to their names (e.g., cluster 1 = XLINE01, XLINE02, XLINE03; cluster 2 = YLINE01, YLINE02; cluster 3 = XYLINE1, XYLINE2, XYLINE3) and reverse the data such that all GPR lines within the same cluster have the same orientation (up to a tolerance value `tol`).
- `tol`: Length-one numeric vector. Tolerance angle in radian to determine if the data have the same orientation. The first data of the cluster is set as reference angle `\alpha_0`, then for data `i` in the same cluster, if `\alpha_i` is not between `\alpha_0 - \frac{tol}{2}` and `\alpha_0 + \frac{tol}{2}`, then the data is reversed.
- `onlyData`: (`logical[1]`) If `TRUE`, only the data are reversed. If `FALSE`, both the data and the coordinates are reversed.

## Description

Reverse the trace position (but not the coordinates).

## Examples

```r
## Not run:

# SU class GPRsurvey
SU <- reverse(SU, id = "zigzag")
# identical to above
SU <- reverse(SU, id = seq(from = 2, to = length(SU), by = 2))
## End(Not run)
```


