# Read Sensors and Software GPR data

```r
readDT1(dsn, ntr, npt)
```

## Arguments

- `dsn`: (`character(1)|connection object`) data source name: either the filepath to the GPR data (character), or an open file connection.
- `ntr`: (`integer(1)`) Number of traces (given by .hd file)
- `npt`: (`integer(1)`) Number of samples per traces (given by .hd file)

## Returns

(`list(2)`) Two-elements list: `dt1hd` with trace header and `data` with the traces.

Read Sensors and Software GPR data

## See Also

`readHD()`, `readGPS()`