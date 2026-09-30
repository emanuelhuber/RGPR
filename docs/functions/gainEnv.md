# Gain Compensation Based on Envelope

```r
gainEnv(
  obj,
  FUN = mean,
  floorquantile = 0.05,
  return_gain = FALSE,
  track = TRUE
)

## S4 method for signature 'GPR'
gainEnv(
  obj,
  FUN = mean,
  floorquantile = 0.05,
  return_gain = FALSE,
  track = TRUE
)
```

## Arguments

- `obj`: (`GPR`)] A GPR object containing traces to be gain-compensated.
- `FUN`: (`function`) A summary function applied to the envelope of each time sample across traces. Defaults to `mean`. Other options could be `median`, `max`, etc.
- `floorquantile`: (`numeric[1]`) Quantile of local gain used as minimum threshold to prevent extreme amplification (default = 0.05, value between 0 and 1.).
- `return_gain`: (`logical[1]`) Should the gain be returned (instead of the gained object)?
- `track`: (`logical[1]`) Should the processing step be tracked?

## Returns

A GPR object with amplitude gain applied according to the envelope-based compensation.

## Description

Applies a gain to a GPR object based on the envelope of the signal. The gain is computed from a summary statistic (e.g., mean) of the envelope across traces and normalized to compensate for amplitude decay.

## Details

The function computes the envelope of each trace (using `amplEnv`) and summarizes it across traces using the specified `FUN`. The resulting vector is normalized to its maximum value to produce a gain vector. The gain is set to 1 for all samples before the peak of the normalized envelope to avoid boosting early-time noise. The reciprocal of the gain is applied to the original GPR data.

Mathematically: c("`\n`", "`g_0(t) = \\frac{\\text{FUN}(\\text{Envelope}(obj))}{\\max(\\text{FUN}(\\text{Envelope}(obj)))}, \\quad\n`", "`g(t) = 1/g_0(t)\n`")

where \(g_0(t) = 1\) for times before the peak of the envelope.


