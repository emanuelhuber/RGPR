# Simulates a GPR wavelet

```r
simWavelet(twt, type = c("annan", "ricker"), fc = 100, lag = 0, q = 1)
```

## Arguments

- `twt`: (`numeric[n]`) Two-way travel time (generally positive if `lag = 0`)
- `type`: (`character[1]`) The Annan wavelet (`type = "annan"`) with dampling factor or the classical Ricker wavelet (`type = "ricker"`)
- `fc`: (`numeric[1]`) Center frequency in MHz
- `lag`: (`numeric[1]`) Used to shift the wavelet in time. For the Ricker wavelet, `lag = -3e3/(pi * fc)` centers the wavelet at `twt = 0`.
- `q`: (`numeric[1]`) Damping factor, where 0 < q < 1 (only for `type = "annan"`)

## Returns

(`numeric[n]`) The wavelet

Simulates a GPR wavelet with specified center frequency.

## Examples

```r
fc <- 100 # MHz 

xt <- seq(-50, by = 0.1, to = 50) # ns 

plot(xt, simWavelet(xt, fc = fc, q = 0.9), type = "l")
plot(xt, simWavelet(xt, fc = fc, q = 0.9, lag = 5), type = "l")

plot(xt, simWavelet(xt, type = "ricker", fc = fc, q = 0.9), type = "l")
plot(xt, simWavelet(xt, type = "ricker", fc = fc, q = 0.9, lag = 5), type = "l")
plot(xt, simWavelet(xt, type = "ricker", fc = fc, q = 0.9, lag = -10), type = "l")
plot(xt, simWavelet(xt, type = "ricker", fc = fc, q = 0.9, lag = -3e3/(pi * fc)), type = "l")
abline(v = 0)
```