# Amplitude envelope

```r
amplEnv(x, method = c("peak", "hilbert"), npad = 100, threshold = 2)

## S4 method for signature 'GPR'
amplEnv(x, method = c("peak", "hilbert"), npad = 100, threshold = 2)
```

## Arguments

- `x`: An object of the class GPR.
- `method`: (`character[1]`) Method to use. See details.
- `npad`: (`integer[1]`) Only for `method = "hilbert"`. Positive value defining the number of values to pad `x` (the padding help to minimize the Gibbs effect at the beginning and end of the data caused by the Hilbert transform).
- `threshold`: (`numeric[1]`) Threshold value for peak detection. The larger the value, the longer the computation time.

## Description

Estimate for each trace the amplitude envelope with the Hilbert transform (instataneous amplitude).

## Details

Two methods:

 * `hilbert` Envelope based on the Hilbert transform
 * `peak` The local maxima of the absolute values of the signal are first estimated. The envelope is determined using spline interpolation over local maxima.


