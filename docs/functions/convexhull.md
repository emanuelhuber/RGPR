# Convex hull of GPR data

```r
convexhull(x, verbose = FALSE)

## S4 method for signature 'GPRsurvey'
convexhull(x, verbose = FALSE)

## S4 method for signature 'GPR'
convexhull(x, verbose = FALSE)

## S4 method for signature 'matrix'
convexhull(x, verbose = FALSE)
```

## Arguments

- `x`: (`GPR|GPRsurvey`)
- `verbose`: (`logical[1]`) If `FALSE`, all messages and warnings are suppressed (use with care).

Return the convex hull.