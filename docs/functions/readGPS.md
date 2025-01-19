# Read Sensors and Software .GPS file

```r
readGPS(dsn, UTM = TRUE)
```

## Arguments

- `dsn`: (`character(1)|connection object`) data source name: either the filepath to the GPR data (character), or an open file connection.
- `UTM`: (`logical(1)`) If `TRUE` project coordinates to the corresponding UTM zone.

## Returns

(`sf|data.frame|NULL`) Either an object of class `sf` or a `data.frame` or `NULL` if no coordinates could be extracted.

Read Sensors and Software .GPS file

## See Also

`readDT1()`, `readHD()`