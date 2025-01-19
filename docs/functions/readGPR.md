# Read a GPR data file

```r
readGPR(
  dsn,
  desc = "",
  Vmax = NULL,
  verbose = TRUE,
  interpGPS = TRUE,
  UTM = TRUE,
  ...
)
```

## Arguments

- `dsn`: (`character|connection`) Data source name: either the filepath to the GPR data (character), or an open file connection (can be a list of filepaths or open file connections). See Details.
- `desc`: (`character[1]`) Short description of the data.
- `Vmax`: (`numeric[1]|NULL`) Nominal analog input voltage used for the bits to volt transformation. It assumes that `Vmin = -Vmax`. If `Vmax = NULL`, no bits to Volt transformation is applied.
- `verbose`: (`logical[1]`) If `FALSE`, all messages and warnings are suppressed (use with care).
- `interpGPS`: (`logical[1]`) Should the trace position be interpolated if possible (that means if a GPS file is available)?
- `UTM`: (`logical[1]|character[1]`) If `TRUE` it is assumed that the coordinates are in the in geographic (longitude/latitude) coordinate reference system (WGS84) and the coordinates are projected into the guessed UTM WGS84 coordinate reference system (RGPR guesses the UTM zone based on the coordinates). Only used if `interpGPS = TRUE`.
- `...`: additional parameters to be passed to `interpCoords()`

## Returns

(`GPR|GPRset`) If the data contains more than one frequency data, it returns an object of the class `GPRset`. Else an object of the class `GPR`.

Read GPR data file from various manufacturers and interpolate trace positions.

## Supported file format

|||||
|:--|:--|:--|:--|
|Manufacturer|Mandatory files|optional GPS files|other optional files|
|Sensors & software|.dt1  , .hd|.gps||
|MALA 16 bits|.rd3  , .rad|.cor||
|MALA 32 bits|.rd7  , .rad|.cor||
|ImpulseRadar|.iprb  , .iprh|.cor|.time , .mrk|
|GSSI|.dzt|.dzg||
|Geomatrix Earth Science Ltd|.dat  , .hdr|.gps, .gpt||
|Radar Systems, Inc.|.sgy/.segy|||
|SEG-Y|.sgy/.segy|||
|R internal file format|.rds|||
|txt files|.txt|||

## Notes

 * If the class of `dsn` is character, `readGPR` is insensitive to the case of the extension (.DT1 or dt1)
 * If `dsn` is a list of connections or a character vector, the order of the elements does not play any rolle.
 * If you use connections, `dsn` must contain at least all the connections to the mandatory files (in any order). If there is more than one mandatory file, use a list of connection.
 * If you use a file path for `dsn` (character), you only need to provide the path to the main mandatory file (marked in bold in the table): RGPR will find the other files if they only differ in their extension (same name). If the files have different names, you must provide at least the path to all the mandatory files.
 * If an optional GPS data file is passed in `dsn` or exists, the GPS data file will be red even if `interpGPS = FALSE`. The (formated) content of the GPS data file is then stored as meta-data and can be retrieved with `metadata(x)$GPS`.
 * If an optional GPS data file (longitude, latitude) is red (when `interpGPS = TRUE` and a GPS data file exists), the coordinates will be per default projected into the corresponding UTM (WGS 84) projection (see `interpCoords()`).
 * When reading GPR data, the clipped signal values are directly estimated from the bit values and stored as metadata. They can be retrieved with `metadata(x)$clip`.

## Examples

```r
## Not run:

# argument dsn is a file path
x1 <- readGPR(dsn = "data/RD3/DAT_0052.rd3")
y1 <- readGPR("data/FILE____050.DZT")

# argument dsn is a connection
con <- file("data/RD3/DAT_0052.rd3", "rb")   # binary mode
con2 <- file("data/RD3/DAT_0052.rad", "rt")  # text mode
x2 <- readGPR(dsn = list(con, con2))

con <- file(dsn = "data/FILE____050.DZT", "rb")
y1 <- readGPR(con)
## End(Not run)
```

## See Also

`writeGPR()`, `interpCoords()`, and `metadata()`