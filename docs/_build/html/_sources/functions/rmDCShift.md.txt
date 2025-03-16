# Direct-Current shift removal

```r
rmDCShift(x, u = NULL, FUN = mean, ..., track = TRUE)

## S4 method for signature 'GPR'
rmDCShift(x, u = NULL, FUN = mean, ..., track = TRUE)
```

## Arguments

- `x`: (`GPR`) An object of the class `GPR`.
- `u`: (`integer[1]`) Index of the trace samples used to evaluate for every trace the DC-shift. If `u = NULL`, the function takes for each trace 90\
    
    of samples can vary from trace to trace).
- `FUN`: (`function()`) A function to apply on the `u` trace samples (default is `mean`; alternatively, `median`
    
    could be of interest because it is more robust but slower to compute).
- `...`: (`ANY`) Further arguments to be passed to `FUN`.

## Returns

(`GPR`) An object of the class `GPR`.

The direct-current offset (DC-shift) is estimated and removed from every trace individually. For a given trace, the DC-shift is estimated by a user supplied function applied on few trace samples, normally the samples before time-zero (e.g., the average of the samples before time-zero). Then, the DC-shift is substracted from the trace.

## Details

The direct-current offset (or DC-shift) is a constant bias over time that slightly shifts the signal amplitude. The DC-shift is best observed on the trace samples recorded before the signal was emitted.

Modified slots

 * `data`: DC-shift removed (data dimensions unchanged).
 * `proc`: updated with function name and arguments.