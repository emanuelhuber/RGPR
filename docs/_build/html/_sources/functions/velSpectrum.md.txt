# Velocity spectrum (CMP Analysis)

```r
velSpectrum(
  x,
  method = c("semblance", "winsemblance", "minsemblance", "wincoherence",
    "wincoherence2"),
  v = NULL,
  w = NULL
)

## S4 method for signature 'GPR'
velSpectrum(
  x,
  method = c("semblance", "winsemblance", "minsemblance", "wincoherence",
    "wincoherence2"),
  v = NULL,
  w = NULL
)
```

## Arguments

- `x`: An object of the class `GPR`
- `method`: A length-one character vector
- `v`: A numeric vector defining at which velocities the analysis is performed. If `v = NULL`, then `v = exp(seq(log(0.02), log(0.3), length = 100))`.
- `w`: A length-one numeric vector defining the window length for the methods 'wincoherence' and 'wincoherence2'.

Transform the space-time domain of the radargram into a velocity-time domain to obtain the velocity spectrum (i.e. change in wave velocity with depth or time). This is achieved by applying Normal Move-Out (NMO) corrections to the radargram for the range of selected velocities and computing a coherency measure for each result. In RGPR, the coherency measure can be defined using different functions: "semblance", "winsemblance", "wincoherence", "wincoherence2".

## Details

either use 'rec' and 'trans' to compute the distance between the antennas or give the distance between the antennas (asep) or `seq(x@antsep, by = x@dx, length.out = length(x))`

- **semblance**: also described as the ratio of input to output energy (Niedell and Taner, 1971)
- **winsemblance**: windowed semblance
- **wincoherence**: Windowed coherence measure based on eigen-decomposition that estimates the signal-to-noise ratio for high resolution velocity analysis (Sacchi, 2002)
- **wincoherence2**: Windowed coherence measure based on a log-generalized likelihood ratio which tests the hypothesis of equality of eigenvalues (Key and Smithson, 1990)

## References

 * Neidell and Taner (1971) Semblance and other coherency measures for multichannel data. Geophysics, 36(3):482-497.
 * Key and Smithson (1990) New approach to seismic-reflection event detection and velocity determination. Geophysics, 55(8):1057-1069.
 * Textbook: Sacchi (2002) Statistical and Transform Methods in Geophysical Signal Processing