# Clip the amplitude

```r
clipData(x, cl = NULL, track = TRUE)

## S4 method for signature 'GPRvirtual'
clipData(x, cl = NULL, track = TRUE)
```

## Arguments

- `x`: (`GPR`) GPR object.
- `cl`: (`numeric[1|2]`) Value above and below which the signal has to be clipDataped. If `cl` is a length-one vector, the signal outside the range `-cl`, `cl` will be clipDataped. If `cl` is a length-one vector, the signal outside the range `cl[1]`, `cl[2]` will be clipDataped.
- `track`: (`logical[1]`) If `TRUE`, processing will be tracked.

## Returns

(`GPR class`) clipDataped GPR object.

Clip the amplitude

Clip the amplitude