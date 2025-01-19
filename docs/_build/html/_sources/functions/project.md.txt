# Project trace coordinates

```r
project(x, CRSobj)

## S4 method for signature 'GPR'
project(x, CRSobj)

## S4 method for signature 'GPRsurvey'
project(x, CRSobj)
```

## Arguments

- `x`: Object of the class GPR
- `CRSobj`: (`character[1]`) A string accepted by GDAL (e.g., `"EPSG:2056"`, WKT-string).

Project the trace coordinates give a coordinate reference system.

## Details

Modified slots

 * `coord` the trace coordinates
 * `x` the local trace position (along profile)
 * `crs` the coordinate reference system.
 * `spunit` the spatial units are updated accroding to the new coordinate reference system.