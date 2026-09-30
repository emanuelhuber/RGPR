# Register a GPR file format

```r
register_gpr_format(
  id,
  detect_ext,
  mandatory,
  optional = character(0),
  gps_ext = NULL,
  reader_fn
)
```

## Arguments

- `id`: (`character(1)`) Unique format identifier, e.g. `"DT1"`.
- `detect_ext`: (`character`) One or more uppercase extensions that identify this format (e.g. `c("SGY", "SEGY")`).
- `mandatory`: (`character`) Named vector of required extensions, slot name as names, e.g. `c(DT1 = "DT1", HD = "HD")`. The first element is the primary file.
- `optional`: (`character`) Named vector of optional extensions. Defaults to `character(0)`.
- `gps_ext`: (`character(1)` or `NULL`) Name of the optional slot whose resolved path is the GPS companion file. `NULL` if the format has no GPS file or handles GPS internally.
- `reader_fn`: (`function`) Format-specific reader. Signature: `function(dsn, fName, fPath, desc, Vmax, verbose, ...)`.

## Description

Called once per format (typically at the bottom of each `io-*.R` file) to add a format descriptor to the package-level registry.


