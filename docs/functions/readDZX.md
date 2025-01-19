# Read GSSI's .dzx file

```r
readDZX(dsn)
```

## Arguments

- `dsn`: (`character(1)|connection object`) data source name: either the filepath to the GPR data (character), or an open file connection.

## Returns

(`list`) contains the markers, the trace position and the spatial sampling.

.dzx files are xml files

## See Also

`readDZT()`, `readDZG()`