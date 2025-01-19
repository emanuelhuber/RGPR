# Check wether the Coordinate Reference Systems is lon/lat

```r
isCRSGeographic(x)

## S4 method for signature 'numeric'
isCRSGeographic(x)

## S4 method for signature 'integer'
isCRSGeographic(x)

## S4 method for signature 'character'
isCRSGeographic(x)

## S4 method for signature 'crs'
isCRSGeographic(x)

## S4 method for signature 'GPR'
isCRSGeographic(x)

## S4 method for signature 'GPRsurvey'
isCRSGeographic(x)
```

## Arguments

- `x`: (`character`) CRS (one or more)

## Returns

(`logical`) `TRUE` if lon/lat else `FALSE`

Returns `TRUE` or `FALSE`.