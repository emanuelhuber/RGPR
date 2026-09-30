# Return time-zero

```r
time0(obj)

time0(obj) <- value

setTime0(obj, t0, track = TRUE)

## S4 method for signature 'GPR'
time0(obj)

## S4 replacement method for signature 'GPR'
time0(obj) <- value

## S4 method for signature 'GPR'
setTime0(obj, t0, track = TRUE)
```

## Arguments

- `obj`: (`GPR* object`)
- `value`: (`numeric[n]`) Time-zero with `n = 1` or `n = ncol(obj)`
- `t0`: (`numeric[n]`) Time-zero with `n = 1` or `n = ncol(obj)`
- `track`: (`logical[1]`) Should the processing step be tracked?

## Returns

A vector containing the time-zero values of each traces.

## Description

`time0` returns the 'time-zero' of every traces. Generally, 'time-zero' corresponds to the first wave arrival (also called first wave break).

## See Also

`pickFirstBreak()` to estimate the first wave break.


