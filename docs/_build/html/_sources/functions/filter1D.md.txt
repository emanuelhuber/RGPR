# One dimensional filters

```r
filter1D(
  x,
  type = c("runmed", "runmean", "MAD", "Gaussian"),
  w = NULL,
  track = TRUE
)

## S4 method for signature 'GPR'
filter1D(
  x,
  type = c("runmed", "runmean", "mad", "gaussian", "hampel"),
  w = NULL,
  track = TRUE
)
```

One dimensional filters