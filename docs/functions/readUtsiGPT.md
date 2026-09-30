# Read Utsi Electronics GPT marker files

```r
readUtsiGPT(dsn)
```

## Arguments

- `dsn`: Character string or connection pointing to a GPT file.

## Returns

A numeric vector containing trace or marker identifiers.

## Description

Reads a GPT file produced by Utsi Electronics GPR systems and returns the trace or marker identifiers associated with the survey.

## Details

GPT files typically contain a sequence of trace indices used to link GPS positions and radar traces.

The function reads all values from the GPT file and returns the resulting vector unchanged.

GPT files are commonly used together with GPS files and can be passed directly to `readUtsiGPS` for georeferencing.

## Examples

```r
## Not run:

gpt <- readUtsiGPT("PROFILE.GPT")
head(gpt)
## End(Not run)
```

## See Also

`readUtsiGPS`


