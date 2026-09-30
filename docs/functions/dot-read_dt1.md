# Read a Sensors & Software GPR file (.dt1 + .hd)

```r
.read_dt1(dsn, fName, fPath, desc, Vmax, verbose, ...)
```

## Arguments

- `dsn`: Named list with slots `DT1`, `HD`, and optionally `GPS` (character paths or open connections).
- `fName`: (`character(1)`) Base filename of the primary (.dt1) file.
- `fPath`: (`character(1)`) Full path of the primary (.dt1) file.
- `desc`: (`character(1)`) Short data description.
- `Vmax`: (`numeric(1)|NULL`) Nominal input voltage for bit conversion.
- `verbose`: (`logical(1)`) Print progress messages.
- `...`: Currently unused; reserved for future use.

## Returns

A named list with: - **x**: Object of class `GPR`.

- **x_gps**: An `sf` object with GPS data, or `NULL`.

## Description

Format-specific reader called by the dispatcher. Not intended to be called directly by users; use `readGPR` instead.


