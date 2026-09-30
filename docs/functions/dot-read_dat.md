# Read a Geomatrix/UTSI GPR file (.dat + .hdr)

```r
.read_dat(dsn, fName, fPath, desc, Vmax, verbose, ...)
```

## Arguments

- `dsn`: Named list with slots `DAT`, `HDR`, and optionally `GPS` and `GPT`.
- `fName`: (`character(1)`) Base filename of the primary (.dat) file.
- `fPath`: (`character(1)`) Full path of the primary (.dat) file.
- `desc`: (`character(1)`) Short data description.
- `Vmax`: (`numeric(1)|NULL`) Nominal input voltage for bit conversion.
- `verbose`: (`logical(1)`) Print progress messages.
- `...`: Currently unused; reserved for future use.

## Returns

A named list with: - **x**: Object of class `GPR`.

- **x_gps**: An `sf` object with GPS data, or `NULL`.

## Description

Format-specific reader called by the dispatcher. Not intended to be called directly by users; use `readGPR` instead.


