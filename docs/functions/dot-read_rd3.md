# Read a MALA GPR file (.rd3/.rd7 + .rad)

```r
.read_rd3(dsn, fName, fPath, desc, Vmax, verbose, ...)
```

## Arguments

- `dsn`: Named list. The data slot is keyed as `RD3` or `RD7` (whichever extension was supplied). Additional slots: `RAD` (mandatory), `COR` (optional).
- `fName`: (`character(1)`) Base filename of the primary data file.
- `fPath`: (`character(1)`) Full path of the primary data file.
- `desc`: (`character(1)`) Short data description.
- `Vmax`: (`numeric(1)|NULL`) Nominal input voltage for bit conversion.
- `verbose`: (`logical(1)`) Print progress messages.
- `...`: Currently unused; reserved for future use.

## Returns

A named list with: - **x**: Object of class `GPR`.

- **x_gps**: An `sf` object with GPS data, or `NULL`.

## Description

Format-specific reader called by the dispatcher. Not intended to be called directly by users; use `readGPR` instead.


