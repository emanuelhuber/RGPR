# 'time-zero' of every traces

```r
time0(x)

time0(x) <- value

setTime0(x, t0, track = TRUE)

## S4 method for signature 'GPR'
time0(x)

## S4 replacement method for signature 'GPR'
time0(x) <- value

## S4 method for signature 'GPR'
setTime0(x, t0, track = TRUE)
```

## Arguments

- `x`: An object of the class GPR.

## Returns

A vector containing the time-zero values of each traces.

`time0` returns the 'time-zero' of every traces. Generally, 'time-zero' corresponds to the first wave arrival (also called first wave break).

## Examples

```r
data(frenkeLine00)
time0(frenkeLine00)
```

## See Also

`firstBreak` to estimate the first wave break.