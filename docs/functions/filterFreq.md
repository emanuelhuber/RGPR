# Frequency filter

```r
## S4 method for signature 'GPR'
filterFreq1D(
  x,
  f = 100,
  type = c("low", "high", "bandpass", "bandpass-reject"),
  L = 257,
  plotSpec = FALSE,
  track = TRUE
)

## S4 method for signature 'GPR'
filterFreq2D(x, fk = NULL, L = c(5, 5), npad = 1, track = TRUE)
```

## Arguments

- `x`: An object of the class GPR
- `f`: numeric vector: cut-off frequencies. Cutoff frequency is the frequency beyond which the filter will not pass signals. See Details.
- `type`: length-one character vector: type of frequency vector. `low`
    
    for low-pass filter, `high` for high-pass filter and `bandpass` for bandpass filter.
- `L`: length-one numeric defining the filter length. See Details.
- `plotSpec`: boolean. If `TRUE` plot the frequency spectrum as well.

The frequency filter alters the signal amplitude with respect to frequency.

 * Low-pass filter: low frequencies are passed, high frequencies are attenuated.
 * High-pass filter: high frequencies are passed, low frequencies are attenuated.
 * Band-pass filter: only frequencies in a frequency band are passed.
 * Band-pass-reject filter: a normally narrow band of frequencies is attenuated.

## Details

For the low- and high-pass filter, only one cut-off frequency can be defined while the argument `L` will define the filter length of the Hamming window (necessary to reduce ringing artifacts from the Gibbs phenomenon). If two values are passed to the cut-off frequency argument `f`, the value of `L` will be ignored. Example for low-pass filter: `f = c(150, 200)`. Example for high-pass filter: `f = c(10, 20)`

For the band-pass filter and the band-pass-reject filter, only two cut-off frequency can be defined while the argument `L` will define the filter length of the Hamming window (necessary to reduce ringing artifacts from the Gibbs phenomenon). If four values (the two first corner frequencies followed by the two last corner frequencies ) are passed to the cut-off frequency argument `f`, the value of `L` will be ignored. Example: `f = c(10, 20, 150, 200)`

Check this free book: The Scientist and Engineer's Guide to Digital Signal Processing By Steven W. Smith, Ph.D.