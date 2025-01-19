# Read GSSI GPR data

```r
readDZT(dsn)
```

## Arguments

- `dsn`: (`character(1)|connection object`) data source name: either the filepath to the GPR data (character), or an open file connection.

## Returns

(`list(4)`) `hd` header data, `data` GPR data, `depth` time or depth, and `pos`

position of the traces.

Read GSSI GPR data

## See Also

`readDZG()`, `readDZX()`