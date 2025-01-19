# Trace coordinates

```r
coordinates(x)

coordinates(x) <- value

## S4 method for signature 'GPR'
coordinates(x)

## S4 replacement method for signature 'GPR'
coordinates(x) <- value

## S4 method for signature 'GPRsurvey'
coordinates(x)

## S4 replacement method for signature 'GPRsurvey'
coordinates(x) <- value
```

## Arguments

- `x`: (`GPR class`) An object of the class `GPR`
- `value`: (`matrix[n,3]|list`) coordinates (x, y, z)

## Returns

(`GPR class`) An object of the class `GPR`

Return or update the trace coordinates (x, y, z). Not that you cannot change the number of coordinates with `coord`.

## Details

Modified slots class GPR

 * `coord` the trace coordinates
 * `x` the local trace position (along profile)

Modified slots class GPRsurvey

 * `coords` the trace coordinates
 * `xlengths` the local trace position (along profile)
 * `intersections` the local trace position (along profile)