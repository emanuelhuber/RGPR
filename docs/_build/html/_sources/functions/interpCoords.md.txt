# Interpolate trace positions from measurement (e.g., GPS).

```r
interpCoords(
  x,
  coords,
  tt = NULL,
  r = NULL,
  UTM = FALSE,
  interp3D = FALSE,
  tol = NULL,
  verbose = TRUE,
  plot = TRUE,
  method = c("linear", "linear", "linear")
)

## S4 method for signature 'GPR'
interpCoords(
  x,
  coords,
  tt = NULL,
  r = NULL,
  UTM = FALSE,
  interp3D = FALSE,
  tol = NULL,
  verbose = TRUE,
  plot = TRUE,
  method = c("linear", "linear", "linear")
)

## S4 method for signature 'GPRsurvey'
interpCoords(
  x,
  coords,
  tt = NULL,
  r = NULL,
  UTM = FALSE,
  interp3D = FALSE,
  tol = NULL,
  verbose = TRUE,
  plot = TRUE,
  method = c("linear", "linear", "linear")
)
```

## Arguments

- `x`: (`GPR|GRPsurvey`)
- `coords`: `matrix|sf::sf|list`: A numeric matrix with at least 3 columns (the (x,y,z)-coordinates) and optionally a fourth column containing the trace number (id) or `NA` if for some coordinates there is no link to the trace number; or a simple feature from the `sf` package of `POINT`
    
    geometry type (one point per coordinates). If there are some attributes, it is assumed that the first one corresponds to the trace number (id). Use `sf::st_cast(..., "POINT")` to convert a `LINESTRING` simple feature to a `point` simple feature. If `x` is a `GPRsurvey` `coords` is a list of either matrix or simple featue as previously defined.
- `tt`: (`numeric|list`) A numeric vector or a list of vectors (if `x` is a `GPRsurvey` object) corresponding to the trace recording time (e.g., output GPS device). If provided, it will be assumed that the traces were recorded at regular time interval (the 4th column of `coords` will be ignored).
- `r`: (`terra::SpatRaster`) A `SpatRaster`
    
    object from the package `terra` from which trace elevation `z` will be extracted based on the trace position `(x, y)` on the raster. The extracted trace elevation will overwrite the values from the third column of `coords`.
- `UTM`: (`logical[1]|character[1]`) If `TRUE` it is assumed that the coordinates are in the in geographic (longitude/latitude) coordinate reference system (WGS84) and the coordinates are projected into the guessed UTM WGS84 coordinate reference system (RGPR guesses the UTM zone based on the coordinates). If `UTM` is a character string corresponding to a UTM zone (for example `UTM = "32N"`), it is assumed that the coordinates are in the in geographic (longitude/latitude) coordinate reference system (WGS84) and the coordinates are projected into the provided UTM zone. Note that only `N`
    
    and `S` are allowed after the zone number (for example, `"31X"` will be not recognized).
- `interp3D`: (`logical[1]`) If `interp3D = TRUE` it is assumed that data recording was triggered by an interp3D. In this case, the coordinate interpolation is based on the 3D distance between the coordinates (x,y,z). If `interp3D = FALSE` (e.g. GPS data) a 2D distance (x, y) is used.
- `tol`: [`numeric[1]`] Length-one numeric vector: if the horizontal distance between two consecutive trace positions is smaller than `tol`, then the traces in between as well as the second trace position are removed. If `tol = NULL`, `tol` is set equal to `sqrt(.Machine$double.eps)`.
- `verbose`: (`logical[1]`) If `FALSE`, all messages and warnings are suppressed (use with care).
- `plot`: (`logical[1]`) If `TRUE` some control/diagnostic plots are displayed.
- `method`: (`character[3]`) The interpolation methods: First element for the interpolation of the inter-trace distances, second element for the interpolation of the horizontal trace positions, and third element for the interpolation of the vertical trace positions. The methods that can be used are `linear`, `nearest`, `pchip`, `cubic`, and `spline`
    
    (same methods as in `[signal]{interp1}`.

## Returns

x with interpolated trace positions.

Interpolate trace positions from measurement (e.g., GPS).