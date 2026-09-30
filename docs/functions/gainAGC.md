# Classical Automatic Gain Control (AGC)

```r
gainAGC(
  obj,
  sig = 10,
  p = 2,
  r = 0.5,
  floorquantile = 0.05,
  return_gain = FALSE,
  track = TRUE
)

## S4 method for signature 'GPR'
gainAGC(
  obj,
  sig = 10,
  p = 2,
  r = 0.5,
  floorquantile = 0.05,
  return_gain = FALSE,
  track = TRUE
)
```

## Arguments

- `obj`: (`GPR* object`) An object of the class GPR.
- `sig`: (`numeric[1]`) standard deviation of the kernel.
- `p`: (`numeric[1]`) Parameter of the power filter (`p` `\geq` 0). Usually, `p` = 2.
- `r`: (`numeric[1]`) Root applied to the smoothed power. Usually, `r` = 0.5.
- `floorquantile`: (`numeric[1]`) Quantile of local gain used as minimum threshold to prevent extreme amplification (default = 0.05, value between 0 and 1.).
- `return_gain`: (`logical[1]`) Should the gain be returned (instead of the gained object)?
- `track`: (`logical[1]`) Should the processing step be tracked?

## Returns

`GPR class` An object of the class GPR.

## Description

`gainAGC` applies an AGC (Automatic Gain Control) gain to compensate for amplitude decay and enhances visibility of weaker signals. The function uses a Gaussian-weighted local RMS or power measure, with local mean removal. A gain floor is applied to avoid extreme amplification in regions of near-zero signal.

## Details

The trace signal is smoothed with a Gaussian filter. The smoothed trace is substracted from the original trace, raised to power `p`, smoothed by a Gaussian filter (to obtain a local weighted sum) and raised to power `r` to obtain the gain. Typical values are `p` = 2 and `r` = 0.5 which will make gain equal to the local RMS. The abs() function is used to allow for arbitrary 'p' and 'r'.

Modified slots `data`: trace gained. `proc`: updated with function name and arguments.


