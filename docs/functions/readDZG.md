# Read GSSI GPS data

```r
readDZG(dsn, UTM = TRUE)
```

## Arguments

- `dsn`: (`character[1]|connection`) data source name: either the filepath to the GPR data (character), or an open file connection.
- `UTM`: (`logical[1]`) If `TRUE` project coordinates to the corresponding UTM zone.

## Returns

(`data.frame(,5)`) position (`x`, `y`, `z`), trace id (`id`), and time (`time`).

## Description

Read GSSI GPS data

## See Also

`readDZT()`, `readDZX()`


