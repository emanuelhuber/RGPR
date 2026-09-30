# Extract a frequency value (in MHz) from a free-form antenna name string

```r
freqFromString(s)
```

## Arguments

- `ant_name`: (`character`) Antenna name string(s).

## Returns

(`numeric`) Frequency in MHz, or `NA` if none found.

## Description

Falls back to pattern matching on strings like `"800 MHz"`, `"1.5GHz"`, etc. when `getAntFreqGSSI` returns `NA`.


