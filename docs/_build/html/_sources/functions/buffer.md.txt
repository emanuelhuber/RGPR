# Buffer the GPR lines

```r
buffer(x, d, combine = TRUE)

## S4 method for signature 'GPR'
buffer(x, d)

## S4 method for signature 'GPRsurvey'
buffer(x, d, combine = TRUE)

## S4 method for signature 'sfc'
buffer(x, d, combine = TRUE)

## S4 method for signature 'sf'
buffer(x, d, combine = TRUE)
```

## Arguments

- `x`: (`GPR|GPRsurvey`) An object of the class `GPR` or `GPRsurvey`
- `d`: (`numeric[1]`) buffer distance
- `combine`: (`logical[1]`) If `TRUE`, returns the buffer for all the GPR lines together. Otherwise it returns the buffer for each line.

## Returns

(`sfc`) Polygon as a simple feature geometry list-column.

Returns the buffered lines as polygon(s)