# Convert longitude, latitude to UTM

```r
lonLatToUTM(lon, lat, zone = NULL, south = NULL, west = FALSE)
```

## Arguments

- `lon`: (`numeric`) Longitude.
- `lat`: (`numeric`) Latitude
- `zone`: (`integer[1]`) UMT zone (optional).
- `south`: (`logical[1]`) `TRUE` if the coordinates are in the southern hemisphere, else `FALSE`.
- `west`: (`logical[1]`) `TRUE` if the longitude measures the angle west of the Prime Meridian; `FALSE` if the longitude measures the angle east of the Prime Meridian.

## Returns

(`list[2]`) `xy` the coordinates in UTM, `crs` the UTM coordinate reference system (proj4string).

see https://stackoverflow.com/a/30225804 https://stackoverflow.com/questions/18639967/converting-latitude-and-longitude-points-to-utm check also https://stackoverflow.com/questions/176137/java-convert-lat-lon-to-utm