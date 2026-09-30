# Read IDS GeoRadar georeferencing files

```r
readGEC(dsn)
```

## Arguments

- `dsn`: Character string or connection pointing to a GEC file.

## Returns

A character matrix containing:

- **x**: X coordinate.
- **y**: Y coordinate.
- **z**: Elevation.
- **ID**: Trace or marker identifier.
- **crs**: Coordinate reference system identifier.
- **crs_add**: Additional CRS information.

## Description

Reads an IDS GeoRadar GEC file containing profile coordinates and marker information.

## Details

The function extracts georeferenced positions and returns them as a matrix suitable for assigning coordinates to a `GPR` object.

GEC files contain metadata tags followed by comma-separated coordinate records. The function automatically locates the beginning of the coordinate table, reads all entries, and returns the relevant coordinate fields.

The returned coordinates can be used to georeference a GPR profile with functions such as `coord<-()`.

## Examples

```r
## Not run:

xyz <- readGEC("profile.GEC")
head(xyz)
## End(Not run)
```

## See Also

`GPR`


