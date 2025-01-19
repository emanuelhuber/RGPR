# Relative position on a path

```r
pathRelPos(xy, lonlat = FALSE)
```

## Arguments

- `xy`: (`matrix`) Each column corresponding to a coordinate
- `lonlat`: (`logical[1]`) If `TRUE` computes geodistance. Otherwise Euclidean distance.

## Returns

Relative position of the coordinates (cumulative distance).

Relative position of each coordinates (knots) along a path (polyline).