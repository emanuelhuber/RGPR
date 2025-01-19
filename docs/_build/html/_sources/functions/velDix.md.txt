# Computes Dix velocities

```r
velDix(twt, v)
```

## Arguments

- `twt`: (`numeric[n]`) $n$ two-way travel times ($n > 0$)
- `v`: [`numeric[n]`] $n$ corresponding velocities

## Returns

(`list`) List with two elements: `t` (time) and `v` (corresponding Dix velocities)

Computes Dix velocities from root-mean-square velocity and corresponding two-way travel times