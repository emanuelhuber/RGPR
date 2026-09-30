# Adaptive smoothing along stripe direction

```r
.adaptiveStripeSmoothing(S, min_len = 3, max_len = 21)
```

## Arguments

- `S`: Numeric matrix.
- `min_len`: Integer. Minimum smoothing window length.
- `max_len`: Integer. Maximum smoothing window length.

## Returns

Numeric matrix with adaptively smoothed columns. @noRd

## Description

Applies column-wise smoothing using variable kernel sizes determined from estimated stripe strength.

## Details

Columns with stronger striping are smoothed more strongly, while well-behaved columns are preserved.


