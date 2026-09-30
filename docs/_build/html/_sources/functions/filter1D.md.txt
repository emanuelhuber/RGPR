# One dimensional filters

```r
filter1D(
  obj,
  type = c("runmed", "runmean", "mad", "gaussian", "hampel"),
  w = NULL,
  track = TRUE
)

## S4 method for signature 'GPR'
filter1D(
  obj,
  type = c("runmed", "runmean", "mad", "gaussian", "hampel"),
  w = NULL,
  track = TRUE
)
```

## Arguments

- `obj`: (`GPR* object`)
- `type`: (`character[1]`) Filter method.
- `w`: (`numeric[1]`) Filter window width.
- `track`: (`logical[1]`) Should the processing step be tracked?

## Returns

(`GPR* object`)

## Description

One dimensional filters


