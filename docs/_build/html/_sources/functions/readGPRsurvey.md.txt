# Read a GPRsurvey object from an HDF5 file

```r
readGPRsurvey(file)
```

## Arguments

- `file`: (`character(1)`) Path to the `.h5` file previously written by `GPRsurvey()` or `writeGPR(..., format = "h5")`.

## Returns

Object of class `GPRsurvey`.

## Description

Reconstructs the `GPRsurvey` index from the `/survey` group of an RGPR HDF5 file. The big per-line radar-data arrays are not loaded until a line is accessed (via `x[[name]]` / `getGPR()`); only the small, per-line metadata (coordinates, markers) is read eagerly here, since those are needed for e.g. `gridCoords()`/`findIntersection()` to work correctly on a freshly-read survey.

## Details

Every slot that `GPRsurvey()` sets is restored here, so `readGPRsurvey(dsn)` after `GPRsurvey(x, dsn = dsn)` yields an object equivalent to what `GPRsurvey()` returned directly (modulo `@view`, which is always `FALSE` for a survey read directly from disk -- see `.write_survey_group_hdf5()` for why `@view` is not itself persisted).

## See Also

`GPRsurvey()`, `writeGPR()`


