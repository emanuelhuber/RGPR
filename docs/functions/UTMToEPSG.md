# EPGS code from UTM zone

```r
UTMToEPSG(zone, south = FALSE)
```

## Arguments

- `zone`: (`integer[1]`) the UTM zone.
- `south`: (`integer[1]`) `TRUE` if the UTM is located in southern hemisphere.

## Returns

(`integer[1]`) The EPSG code.

Returns the EPSG code from UTM zone. EPSG code is: 32600+zone for positive latitudes and 32700+zone for negatives latitudes.