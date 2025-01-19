# Read GSSI GPS data

```r
readDZG(dsn)
```

## Arguments

- `dsn`: (`character(1)|connection object`) data source name: either the filepath to the GPR data (character), or an open file connection.

## Returns

(`data.frame(,5)`) position (`x`, `y`, `z`), trace id (`id`), and time (`time`).

Read GSSI GPS data

## See Also

`readDZT()`, `readDZX()`