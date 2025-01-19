# Time to depth conversion

```r
timeToDepth(twt, t0, v = 0.1, antsep = 1)
```

## Arguments

- `twt`: `numeric` Two-way travel time vector.
- `t0`: [`numeric[1]`] Time-zero: only `x >= x0` will be considered.
- `v`: [`numeric[1]`] Electromagnetic wave propagation in the ground (or in the considered media).
- `antsep`: (`numeric[1]`) Antenna separation (distance between the transmitter and the receiver).

## Returns

(`numeric`) Corresponding depth.

Convert two-way travel time into depth by accounting for the antenna separation between the transmitter and the receiver.