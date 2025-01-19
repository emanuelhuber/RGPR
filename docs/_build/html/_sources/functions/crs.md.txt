# Coordinate reference system (CRS) of the GPR data

```r
crs(x)

crs(x) <- value

## S4 method for signature 'GPR'
crs(x)

## S4 replacement method for signature 'GPR'
crs(x) <- value

## S4 method for signature 'GPRsurvey'
crs(x)

## S4 replacement method for signature 'GPRsurvey'
crs(x) <- value
```

## Arguments

- `x`: (`GPR class`) An object of the class `GPR`
- `value`: (`character[1]`) A string accepted by GDAL (e.g., `"EPSG:2056"`, WKT-string).

## Returns

(`GPR class`) An object of the class `GPR`

Coordinate reference system (CRS) of the GPR data

## Details

Modified slots

 * `crs` the coordinate reference system.
 * `spunit` the spatial units are updated accroding to the new coordinate reference system.

## Examples

```r
## Not run:

x <- readGPR("LINE.DT1")
crs(x) <- "EPSG:3857"
## End(Not run)
```