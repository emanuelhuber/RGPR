# Estimate stripe strength per column

```r
.estimateStripeStrength(S)
```

## Arguments

- `S`: Numeric matrix.

## Returns

Numeric vector of length `ncol(S)` representing relative stripe strength per column. @noRd

## Description

Computes a quantitative estimate of striping intensity for each column of a gridded image by measuring high-frequency perpendicular differences.

## Details

This metric is used to drive adaptive smoothing.


