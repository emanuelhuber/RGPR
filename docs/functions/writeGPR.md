# Write the GPR object in a file.

```r
writeGPR(
  obj,
  dsn = NULL,
  format = c("rds", "dt1", "ascii", "xta", "xyza", "vtk"),
  overwrite = FALSE,
  ...
)

## S4 method for signature 'GPR'
writeGPR(
  obj,
  dsn = NULL,
  format = c("rds", "dt1", "ascii", "xta", "xyza", "vtk"),
  overwrite = FALSE,
  ...
)

## S4 method for signature 'GPRsurvey'
writeGPR(
  obj,
  dsn = NULL,
  format = c("DT1", "rds", "ASCII", "xta", "xyzv", "vtk", "h5"),
  overwrite = FALSE,
  compress = 5L,
  ...
)
```

## Arguments

- `obj`: Object of class `GPRsurvey`.
- `dsn`: (`character[1]`) Output path. Directory for multi-file formats; `.h5` file path for `format = "h5"`.
- `format`: (`character[1]`) One of `"DT1"`, `"rds"`, `"ASCII"`, `"xta"`, `"xyzv"`, `"vtk"`, or `"h5"`.
- `overwrite`: (`logical[1]`) If `FALSE` (default) and the output already exists, an error is raised.
- `...`: Additional arguments passed to the per-line `writeGPR()` calls (ignored for `"h5"` and `"vtk"`).
- `compress`: (`integer[1]`) gzip compression level 0–9 for the data array inside HDF5 files. Only used when `format = "h5"`. Default `5L`.

## Returns

Invisibly returns `obj` (updated `@paths` slot) for multi-file formats, or the output file path for `"h5"`.

## Description

Dispatches to the appropriate writer depending on `format`. For all formats except `"h5"` and `"vtk"`, `dsn` is treated as a directory path: RGPR creates the directory if necessary and writes one file per GPR line inside it. For `"h5"`, `dsn` is the path to the output `.h5` file.

## See Also

`readGPR()` `readGPRsurvey()`, `writeGPR()`


