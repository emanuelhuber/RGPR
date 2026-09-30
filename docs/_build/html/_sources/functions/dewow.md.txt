# Trace dewowing

```r
dewow(obj, type = c("runmed", "runmean", "gaussian"), w = NULL, track = TRUE)

## S4 method for signature 'GPR'
dewow(obj, type = c("runmed", "runmean", "gaussian"), w = NULL, track = TRUE)
```

## Arguments

- `obj`: (`GPR`) A GPR object.
- `type`: (`character[1]`) Dewow method. One of:
    
     * `runmed` for running median filtering;
     * `runmean` for running mean filtering;
     * `Gaussian` for Gaussian smoothing.
- `w`: (`numeric[1]|NULL`) Filter width. For `runmed`, `MAD`, and `runmean`, this corresponds to the window length (in trace units). For `Gaussian`, it corresponds to the standard deviation (in trace units).
    
    If `NULL`, `w` is estimated as five times the wavelength associated with the maximum frequency of `obj` estimated by `spec()`.
- `track`: (`logical[1]`) Should the processing step be tracked?

## Returns

(`GPR`) A dewowed GPR object.

## Description

Removes the low-frequency component (the so-called **wow**) from each trace.

## Details

The low-frequency component can be estimated using:

 * `runmed`: running median based on `stats::runmed()`.
 * `runmean`: running mean based on `stats::filter()`.
 * `MAD`: deprecated Median Absolute Deviation filter.
 * `Gaussian`: Gaussian smoothing applied to trace samples after time-zero based on `mmand::gaussianSmooth()`.

Modified slots:

 * `data`: dewowed traces.
 * `proc`: updated with function name and arguments.


