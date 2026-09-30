# Scale the traces

```r
scale1D(
  obj,
  type = c("stat", "min-max", "95", "eq", "sum", "rms", "mad", "invNormal"),
  track = TRUE
)

## S4 method for signature 'GPR'
scale1D(
  obj,
  type = c("stat", "min-max", "95", "eq", "sum", "rms", "mad", "invNormal"),
  track = TRUE
)
```

## Arguments

- `obj`: (`GPR* object`)
- `type`: (`character[1]`) Type of scaling
- `track`: (`logical[1]`) Should the processing step be tracked?

## Description

Scale the traces


