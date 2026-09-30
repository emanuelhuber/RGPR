# Materialize a GPRsurvey object

```r
materialize(obj, dsn, overwrite = FALSE, compress = 5L, ...)

## S4 method for signature 'GPRsurvey'
materialize(obj, dsn, overwrite = FALSE, compress = 5L, ...)

## S4 method for signature 'GPRcube'
materialize(
  obj,
  dsn,
  overwrite = FALSE,
  compress = 5L,
  batch_size = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- `obj`: Object of class `GPRsurvey`.
- `dsn`: `character[1]`. Path to the output HDF5 backing file. The file should conventionally use the extension `.h5`.
- `overwrite`: `logical[1]`. If `FALSE`, the default, and `dsn` already exists, an error is raised. If `TRUE`, the existing file is replaced.
- `compress`: `integer[1]`. gzip compression level from 0 to 9 for data arrays stored in the HDF5 file. Default is `5L`.
- `...`: Additional arguments passed to `writeGPR()`.

## Returns

A materialized `GPRsurvey` object backed by `dsn`.

## Description

Create an independent HDF5-backed copy of a `GPRsurvey` object.

## Details

A `GPRsurvey` may be a lightweight view created by subsetting another survey, for example with `x[1:10]`. Such a view remains backed by the original HDF5 file and is not modified in place. `materialize()` writes the selected survey lines and metadata to a new HDF5 backing file and returns a writable `GPRsurvey` object backed by that file.

This function is equivalent in spirit to `writeGPR(x, format = "h5")`, but is intended specifically for turning a survey view into an independent survey.

## See Also

`writeGPR`, `GPRsurvey`


