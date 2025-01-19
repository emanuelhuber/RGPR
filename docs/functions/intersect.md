# Compute GPR profile intersections

```r
intersect(x)

## S4 method for signature 'GPRsurvey'
intersect(x)
```

## Arguments

- `x`: `GPRsurvey` An object of the class `GPRsurvey`

## Returns

(`object GPRsurvey`) An object of the class GPRsurvey.

Compute GPR profile intersections

## Details

Modified slots

 * `intersects`: trace shifted. The number of rows of data may be smaller if `crop = TRUE`.