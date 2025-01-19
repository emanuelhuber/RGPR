# Return the clipped values of the GPR signal as GPR object

```r
clippedData(obj, dlim = NULL, verbose = TRUE)

## S4 method for signature 'GPR'
clippedData(obj, dlim = NULL, verbose = TRUE)
```

## Arguments

- `obj`: (`GPR`)
- `dlim`: (`numeric[2]`) The min and max clipped values if they are a priori known. If `dlim = NULL`, the function tries to get the the clipped values from `metadata(obj)$clip`. If there are no clipped values, the function tries to estimate them.
- `verbose`: (`logical[1]`) If `FALSE`, all messages and warnings are suppressed (use with care).

## Returns

(`GPR`) The object with the clipped values.

Max and min values