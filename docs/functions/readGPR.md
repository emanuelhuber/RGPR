# Read a GPR data file

```r
readGPR(
  dsn,
  desc = "",
  Vmax = NULL,
  verbose = TRUE,
  interpGPS = TRUE,
  UTM = TRUE,
  endian = .Platform$endian,
  ...
)
```

## Arguments

- `dsn`: (`character|connection`) Data source name: either the filepath to the GPR data (character), or an open file connection (can be a vector of file paths or connections).
- `desc`: (`character(1)`) Short description of the data.
- `Vmax`: (`numeric(1)|NULL`) Nominal analog input voltage for the bits-to-volt transformation. `NULL` skips conversion.
- `verbose`: (`logical(1)`) If `FALSE`, all messages and warnings are suppressed (use with care).
- `interpGPS`: (`logical(1)`) Should trace positions be interpolated from GPS data when available?
- `UTM`: (`logical(1)|character(1)`) If `TRUE`, geographic (lon/lat WGS84) coordinates are projected to UTM WGS84. Only used when `interpGPS = TRUE`.
- `...`: Additional parameters passed to `interpCoords`.

## Returns

(`GPR|GPRset`) An object of class `GPR`, or `GPRset` for multi-channel data.

## Description

Read GPR data file from various manufacturers and interpolate trace positions.

## Supported file formats

|||||
|:--|:--|:--|:--|
|Manufacturer|Mandatory files|Optional GPS files|Other optional files|
|Sensors & Software|.dt1  , .hd|.gps||
|MALA 16 bits|.rd3  , .rad|.cor||
|MALA 32 bits|.rd7  , .rad|.cor||
|ImpulseRadar|.iprb  , .iprh|.cor|.time, .mrk|
|GSSI|.dzt|.dzg|.dzx|
|Geomatrix (UTSI)|.dat  , .hdr|.gps, .gpt||
|Radar Systems / SEG-Y|.sgy/.segy|||
|3d-Radar|.vol|||
|R internal format|.rds|||
|Text files|.txt|||

## Notes

 * If the class of `dsn` is character, `readGPR` is insensitive to the case of the extension (.DT1 or .dt1).
 * If `dsn` is a list of connections or a character vector, the order of the elements does not matter.
 * If you use connections, `dsn` must contain at least all the connections to the mandatory files (in any order). If there is more than one mandatory file, use a list of connections.
 * If you use a file path for `dsn` (character), you only need to provide the path to the primary mandatory file (marked in bold in the table): RGPR will find the other files automatically if they share the same base name and differ only in extension. If the files have different names, supply at least the paths to all mandatory files.
 * If an optional GPS data file is passed in `dsn` or is found on disk, it will be read even if `interpGPS = FALSE`. The formatted content is stored as metadata and can be retrieved with `metadata(x)$GPS`.
 * When `interpGPS = TRUE` and a GPS file with longitude/latitude data exists, the coordinates are by default projected into the corresponding UTM (WGS 84) zone (see `interpCoords`).
 * Clipped signal values are estimated from the bit depth and stored as metadata; retrieve with `metadata(x)$clip`.

## Examples

```r
## Not run:

# File path
x1 <- readGPR(dsn = "data/RD3/DAT_0052.rd3")
y1 <- readGPR("data/FILE____050.DZT")

# Connection
con  <- file("data/RD3/DAT_0052.rd3", "rb")
con2 <- file("data/RD3/DAT_0052.rad", "rt")
x2   <- readGPR(dsn = list(con, con2))
## End(Not run)
```

## See Also

`writeGPR()`, `interpCoords()`, `metadata()`


