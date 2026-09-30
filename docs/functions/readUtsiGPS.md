# Read Utsi Electronics GPS files

```r
readUtsiGPS(dsn, gpt, UTM = TRUE)
```

## Arguments

- `dsn`: Character string or connection pointing to the GPS file.
- `gpt`: Numeric vector of trace identifiers, typically obtained from `readUtsiGPT`.
- `UTM`: Logical. If `TRUE`, coordinates are projected to UTM. If `FALSE`, geographic coordinates are retained.

## Returns

An `sf` object with a single attribute:

- **id**: Trace identifier from the GPT file.

Geometry coordinates correspond to `x`, `y` and `z` positions derived from the GPS records.

Returns `NULL` if the GPS file is empty or if the number of GPS positions does not match the number of GPT trace identifiers.

## Description

Reads GPS data exported by Utsi Electronics GPR systems and associates the recorded positions with trace identifiers stored in a GPT file.

## Details

GPS coordinates are extracted from NMEA GPGGA sentences and may optionally be projected to UTM coordinates.

The function expects NMEA GPGGA records, for example:

```
$GPGGA,140454.00,5518.98033,N,00203.66162,W,
1,09,1.19,123.0,M,48.6,M,,*48
```

Geographic coordinates are extracted using `getLonLatFromGPGGA` and optionally projected using `projectXYZT`. The resulting coordinates are combined with the GPT trace identifiers and returned as an `sf` point object.

## Examples

```r
## Not run:

gpt <- readUtsiGPT("PROFILE.GPT")
gps <- readUtsiGPS("PROFILE.GPS", gpt)

plot(sf::st_geometry(gps))
head(gps)
## End(Not run)
```

## See Also

`readUtsiGPT`, `getLonLatFromGPGGA`


