# Read Sensors and Software .HD file

```r
readHD(dsn)
```

## Arguments

- `dsn`: (`character(1)|connection object`) data source name: either the filepath to the GPR data (character), or an open file connection.

## Returns

(`list(3)`) Three-elements list: `HD` containing the header info, `ntr` the number of trace and `npt` the number of points per trace.

Read Sensors and Software .HD file

## See Also

`readDT1()`, `readGPS()`