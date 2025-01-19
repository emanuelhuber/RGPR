# Interpolate horizontal slices

```r
interpSlices(
  x,
  dx = NULL,
  dy = NULL,
  dz = NULL,
  h = 6,
  extend = c("bbox", "obbox", "chull", "buffer"),
  buffer = NULL,
  shp = NULL,
  rot = FALSE
)

## S4 method for signature 'GPRsurvey'
interpSlices(
  x,
  dx = NULL,
  dy = NULL,
  dz = NULL,
  h = 6,
  extend = c("bbox", "obbox", "chull", "buffer"),
  buffer = NULL,
  shp = NULL,
  rot = FALSE
)
```

## Arguments

- `x`: (`GPRsurvey`)
- `dx`: (`numeric[1]`) x-resolution
- `dy`: (`numeric[1]`) y-resolution
- `dz`: (`numeric[1]`) z-resolution
- `h`: (`numeric[1]`) FIXME: see function...
- `extend`: (`character[1]`) FIXME: see function...
- `buffer`: (`numeric[1]`) FIXME: see function...
- `shp`: (`matrix[n,2]|list[2]|sf`) FIXME: see function...
- `rot`: (`logical[1]|numeric[1]`) If `TRUE` the GPR lines are fist rotated such to minimise their axis-aligned bounding box. If `rot` is numeric, the GPR lines is rotated first rotated by `rot` (in radian).

Interpolate horizontal slices