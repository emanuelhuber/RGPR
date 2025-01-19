# Coerce object to an sf POINT object

```r
as.sf(x)

## S4 method for signature 'GPR'
as.sf(x)

## S4 method for signature 'GPRsurvey'
as.sf(x)
```

## Arguments

- `x`: (`class GPR|GPRsurvey`)

## Returns

(`class sf`) Geometry type is `POINT`.

Coerce object to a simple feature (sf) geometry POINT. Only coordinates are coerced, no additional information.