# Read IDS DT radar files

```r
readDT(dsn, endian = endian)
```

## Arguments

- `dsn`: Character string or binary connection pointing to a DT file.
- `endian`: Character string specifying the byte order used when reading binary values. Typically `"little"` or `"big"`.

## Returns

A list with the following elements:

- **data**: Matrix containing radar amplitudes. Rows correspond to samples and columns to traces.
- **HD**: List containing header information and acquisition metadata.
- **mrk1**: Numeric vector containing marker information for each trace.
- **mrk2**: Numeric vector containing additional marker information for each trace.

## Description

Reads an IDS GeoRadar DT file and returns the radargram together with acquisition metadata stored in the file header.

## Details

The function parses the proprietary DT header structure, extracts survey parameters (antenna frequency, spatial sampling, scan settings, GPS offsets, acquisition geometry, etc.), and loads the radar traces into a matrix.

The DT format contains a sequence of tagged header records followed by radar trace data. The function iteratively parses all supported tags and stores the extracted information in the returned `HD` list.

The trace data are stored as signed 16-bit integers and are returned without amplitude conversion. Use `.gprDT` to convert the output into a `GPR` object.

## Examples

```r
## Not run:

x <- readDT("profile.DT")
str(x$HD)
image(x$data)
## End(Not run)
```

## See Also

`.gprDT`


