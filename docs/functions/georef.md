# Rotate coordinates of the GPR traces

```r
georef(
  x,
  alpha = NULL,
  cloc = NULL,
  creg = NULL,
  ploc = NULL,
  preg = NULL,
  FUN = mean
)

## S4 method for signature 'GPRsurvey'
georef(
  x,
  alpha = NULL,
  cloc = NULL,
  creg = NULL,
  ploc = NULL,
  preg = NULL,
  FUN = mean
)
```

## Arguments

- `x`: (`GPR|GPRsurvey`)
- `alpha`: (`numeric[1]`) The rotation angle in radians. If `alpha = NULL`, `alpha` is estimated from the pairs of points in the local reference system (`ploc`) and in the regional reference system (`preg`).
- `cloc`: (`numeric[2]`) the coordinate center of the local reference system
- `creg`: (`numeric[2|3]`) the coordinate center of the regional reference system. Setting `creg = NULL` (default) is equivalent to apply a rotation of angle `alpha` and center `cloc`.
- `ploc`: (`numeric[2|3]`) the first two columns corresponding
- `preg`: (`numeric[2|3]`)
- `FUN`: (`function`)

Rotate coordinates of the GPR traces...