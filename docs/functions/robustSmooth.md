# Robust smoothing

```r
robustSmooth(
  x,
  spar = NULL,
  width,
  method = "PRMMH",
  extrapolate = TRUE,
  minNonNAs = 3
)
```

## Arguments

- `x`: a numeric vector or (univariate) time series object.
- `spar`: smoothing parameter, typically (but not necessarily) in (0,1]. See `[stats:smooth.spline()]`.
- `width`: an odd positive integer (>=3) defining the window width used for fitting. See `[robfilter:hybrid.filter()]`.
- `method`: a (vector of) character string(s) containing the method(s) to be used for the estimation of the signal level. Method choice: "MED", "RM", "MEAN", FMH, "PFMH", "CFMH", "MH", "PRMH", "CRMH", "MMH", "PRMMH", "CRMMH". See `[robfilter:hybrid.filter()]`.
- `extrapolate`: a logical indicating whether the level estimations should be extrapolated to the edges of the time series. See `[robfilter:hybrid.filter()]`.
- `minNonNAs`: a positive integer defining the minimum number of non-missing observationswithin each window (half) which is required for a 'sensible' estimation. See `[robfilter:hybrid.filter()]`.

A wrapper for the functions `robfilter::hybrid.filter` and `smooth.spline`