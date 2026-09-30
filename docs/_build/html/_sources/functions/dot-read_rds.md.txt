# Read an R-internal GPR file (.rds)

```r
.read_rds(dsn, fName, fPath, desc, Vmax, verbose, ...)
```

## Arguments

- `dsn`: Named list with slot `RDS`.
- `fName`: (`character(1)`) Base filename of the .rds file.
- `fPath`: (`character(1)`) Full path of the .rds file.
- `desc`: (`character(1)`) Short data description (unused for RDS; the stored object already contains its description).
- `Vmax`: (`numeric(1)|NULL`) Unused for RDS files.
- `verbose`: (`logical(1)`) Print progress messages.
- `...`: Currently unused; reserved for future use.

## Returns

A named list with: - **x**: Object of class `GPR` or `GPRset`.

- **x_gps**: `NULL` (RDS objects already contain coordinates).

## Description

Format-specific reader called by the dispatcher. Not intended to be called directly by users; use `readGPR` instead.


