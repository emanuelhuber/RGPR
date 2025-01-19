# EPGS code from UTM zone string

```r
UMTStringToEPSG(x)
```

## Arguments

- `x`: (`character[1]`) The EPSG code string (e.g. 32N).

## Returns

(`integer[1]`) The EPSG code.

Returns the EPSG code from UTM zone string (e.g., '32N'). EPSG code is: 32600+zone for positive latitudes and 32700+zone for negatives latitudes.