# Resolve antenna frequency for GSSI data

```r
.resolve_gssi_antfreq(ant_name)
```

## Arguments

- `ant_name`: (`character`) Antenna name vector (one element per channel).

## Returns

A named list: `$freq` (numeric), `$unit` (character).

## Description

Tries `getAntFreqGSSI()` first; falls back to `freqFromString()` for any `NA` values. Returns a list suitable for populating `GPR@freq` and the y-axis of a `GPRset`.


