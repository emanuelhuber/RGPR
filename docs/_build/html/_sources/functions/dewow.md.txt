# Trace dewowing

```r
dewow(x, type = c("runmed", "runmean", "gaussian"), w = NULL, track = TRUE)

## S4 method for signature 'GPR'
dewow(x, type = c("runmed", "runmean", "gaussian"), w = NULL, track = TRUE)
```

## Arguments

- `x`: (`GPR`) An object of the class GPR.
- `type`: (`character[1]`) Dewow method, one of `runmed` (running median), `runmean` (running mean), `Gaussian` (Gaussian smoothing).
- `w`: (`numeric[1]`) If `type` = `runmed`, `MAD` or `runmean`, window length of the filter in trace unit; If `type` = `Gaussian`, standard deviation in trace unit. If `w = NULL`, `w` is estimated as five times the wavelength corresponding to the maximum frequency of x (estimated with spec )

## Returns

(`GPR`) An object of the class GPR whose traces are dewowed.

`dewow` remove the low-frequency component (the so-called 'wow') of every traces.

## Details

The low-frequency component is computed by different methods:

 * `runmed` running median based on stats::runmed
 * `runmean` running mean based on stats::filter
 * `MAD` DEPRECATED - Median Absolute Deviation filter
 * `Gaussian` Gaussian smoothing applied to the trace samples after time-zero based on mmand::gaussianSmooth

Modified slots:

 * `data`: trace dewowed.
 * `proc`: updated with function name and arguments.