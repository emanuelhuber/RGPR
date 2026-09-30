# Read a plain-text GPR file (.txt)

```r
.read_txt(dsn, fName, fPath, desc, Vmax, verbose, ...)
```

## Arguments

- `dsn`: Named list with slot `TXT`.
- `fName`: (`character(1)`) Base filename of the .txt file.
- `fPath`: (`character(1)`) Full path of the .txt file.
- `desc`: (`character(1)`) Short data description.
- `Vmax`: (`numeric(1)|NULL`) Nominal input voltage for bit conversion.
- `verbose`: (`logical(1)`) Print progress messages.
- `...`: Currently unused; reserved for future use.

## Returns

A named list with: - **x**: Object of class `GPR`.

- **x_gps**: `NULL` (TXT carries no GPS companion file).

## Description

Format-specific reader called by the dispatcher. Not intended to be called directly by users; use `readGPR` instead.


