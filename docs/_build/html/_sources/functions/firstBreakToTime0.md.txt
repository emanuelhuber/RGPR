# Convert first wave break time to time-zero

```r
firstBreakToTime0(x, fb, c0 = 0.299)

## S4 method for signature 'GPR'
firstBreakToTime0(x, fb, c0 = 0.299)
```

## Arguments

- `x`: (`GPR`)
- `fb`: (`numeric[1|n]`) Time of first break (`n = ncol(x)`)
- `c0`: (`numeric[1]`) Propagation speed of the GPR wave through air in unit of space per unit of time (generally in m/ns).

Account for the delay time between time of wave emission and time of first wave break recording due to the antenna separation (offset).