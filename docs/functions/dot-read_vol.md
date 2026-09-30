# Read a 3d-Radar VOL file (.vol)

```r
.read_vol(dsn, fName, fPath, desc, Vmax, verbose, ...)
```

## Arguments

- `dsn`: Named list with slot `VOL`.
- `fName`: (`character(1)`) Base filename.
- `fPath`: (`character(1)`) Full path to the .vol file.
- `desc`: (`character(1)`) Short data description.
- `Vmax`: (`numeric(1)|NULL`) Nominal input voltage for bit conversion.
- `verbose`: (`logical(1)`) Print progress messages.
- `...`: Currently unused; reserved for future use.

## Returns

A named list with: - **x**: Object of class `GPR` or `GPRcube`.

- **x_gps**: `NULL` (VOL carries no GPS companion file).

## Description

Format-specific reader called by the dispatcher. Not intended to be called directly by users; use `readGPR` instead.


