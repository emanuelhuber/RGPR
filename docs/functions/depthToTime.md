# Depth to time conversion

```r
depthToTime(x, t0, v = 0.1, antsep = 1)
```

## Arguments

- `x`: [`numeric[n]`] Depth vector.
- `t0`: [`numeric[1]`] Time-zero.
- `v`: [`numeric[1]`] Electromagnetic wave propagation in the ground (or in the considered media).
- `antsep`: (`numeric[1]`) Antenna separation (distance between the transmitter and the receiver).

## Returns

(`numeric`) Corresponding two-way travel time

Convert depth to the equivalent two-way travel time by accounting for the antenna separation between the transmitter and the receiver.