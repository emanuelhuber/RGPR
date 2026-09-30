# Read GSSI GPR data (.dzt)

```r
readDZT(dsn)
```

## Arguments

- `dsn`: (`character(1)|connection`) Path or open binary connection to the .dzt file.

## Returns

A list with elements: - **hd**: Parsed header (list).

- **data**: 3-D array `[nSamples, nScans, nChannels]`.

- **depth**: Time vector (ns).

- **pos**: Nominal position vector (m).

## Description

Reads the binary DZT file and returns the raw data array together with the parsed header and axis vectors.

## See Also

`readDZG()`, `readDZX()`


