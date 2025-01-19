# Return the position of depth-zero on the two-way travel time axis

```r
depth0(t0 = 0, v = 0.1, antsep = 1)
```

## Arguments

- `t0`: [`numeric[1]`] Time-zero.
- `v`: [`numeric[1]`] Electromagnetic wave propagation in the ground (or in the considered media).
- `antsep`: (`numeric[1]`) Antenna separation (distance between the transmitter and the receiver).

## Returns

(`numeric`) Position of depth-zero on the two-way time axis

Useful if you want to plot a depth axis beside the two-way travel time axis.