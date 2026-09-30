# Simulates CMP data

```r
simCMP(
  vint = c(0.1, 0.095, 0.08, 0.09, 0.105, 0.09, 0.095),
  d = c(0.5, 0.75, 1.01, 1.4, 1.9, 2.2, 2.6),
  antsep = seq(0, to = 20, by = 0.25),
  dz = 0.25,
  zmax = 250,
  fc = 100,
  lw = 15,
  qw = 0.9
)
```

## Arguments

- `vint`: (`numeric[n]`) internal velocity of the layers
- `d`: (`numeric[n]`) thickness of the layers
- `antsep`: (`numeric[m]`) antenna separations use for the data acquisition
- `dz`: (`numeric[1]`) time sampling (ns)
- `zmax`: (`numeric[1]`) maximum time (ns)
- `fc`: (`numeric[1]`) Center frequency in MHz
- `lw`: (`numeric[1]`) wavelet duration (ns)
- `qw`: (`numeric[1]`) Damping factor for the wavelet, where 0 < q < 1

## Returns

(`GPR`) CMP data

## Description

Simulates CMP data.


