# Normalize a markers vector to match the number of traces

```r
.normalizeMarkers(markers, nx, verbose = TRUE)
```

## Arguments

- `markers`: (`character`) Raw markers vector, typically `gpr@markers`.
- `nx`: (`integer(1)`) Number of traces the line has (`ncol(gpr)`).
- `verbose`: (`logical(1)`) Print a message when padding/truncating.

## Returns

(`character[nx]`) Trimmed markers vector of length exactly `nx`.

## Description

Ensures the markers vector for a line always has exactly `nx` elements (one per trace), trimmed with `trimStr()`. This guarantees the same, consistent vector is used both for the per-line HDF5 dataset (`/lines/<name>/markers`) and for the survey-level `@markers` slot, which previously could disagree (`trimStr()` was applied in one place but not the other).


