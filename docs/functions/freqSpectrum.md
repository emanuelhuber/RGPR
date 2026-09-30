# Frequency spectrum the traces (1D)

```r
freqSpectrum(obj, plotSpec = TRUE, unwrapPhase = TRUE, ...)

## S4 method for signature 'GPR'
freqSpectrum(obj, plotSpec = TRUE, unwrapPhase = TRUE, ...)
```

## Arguments

- `obj`: (`GPR* object`)
- `plotSpec`: (`logical[1]`) Plot spectrum?
- `unwrapPhase`: (`logical[1]`) Unwrap phase?
- `...`: Additional parameters

## Returns

(`GPRset`) Sets = Amplitude and Phase as a function of frequency

## Description

Based on the function `fft()` of R base


