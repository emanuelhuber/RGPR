# Frequency-domain destriping

```r
.fft_destripe(
  S,
  stripe_dir = c("column", "row"),
  cutoff = 0.05,
  strength = 0.9
)
```

## Arguments

- `S`: Numeric matrix.
- `stripe_dir`: Character string. `"column"` or `"row"`.
- `cutoff`: Numeric. Normalized frequency cutoff (0–0.5).
- `strength`: Numeric. Attenuation strength (0–1).

## Returns

Numeric matrix with frequency-domain stripe reduction. @noRd

## Description

Removes low-frequency stripe components by attenuating spectral energy perpendicular to the stripe direction.

## Details

Particularly effective for periodic or acquisition-induced striping artefacts.


