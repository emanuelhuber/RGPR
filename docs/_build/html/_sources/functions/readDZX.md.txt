# Read GSSI extended XML metadata (.dzx)

```r
readDZX(dsn)
```

## Arguments

- `dsn`: (`character(1)|connection`) Path or open binary connection to the .dzx file.

## Returns

A list with some or all of the following elements: - **pos**: Interpolated position for each scan (numeric vector).

- **dx**: Mean spatial sampling interval (numeric).

- **markers**: Character vector of marker labels, one per scan.

- **hUnit**: Horizontal distance unit string (e.g. `"m"`).

- **vUnit**: Vertical unit string.

- **unitsPerMark**: Units per odometer mark (numeric).

- **unitsPerScan**: Units per scan (numeric).

Returns `NULL` for empty or unreadable files.

## Description

Extracts trace positions, spatial sampling, horizontal units, and fiducial markers from the XML companion file written by GSSI instruments.

## See Also

`readDZT()`, `readDZG()`


